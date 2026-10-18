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
typed nested/anonymous/function-reference callback and OUT-parameter features.
Persistent changes detach data. `InMode` revokes option/feature knowledge;
translation separately restores the verified source-mode defaults described below.
Condition analysis supports `defined`, integer comparisons, parentheses,
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
remain physical diagnostics. Initial lexical policies are explicit; supported
comment/mode changes now apply between tokens. Unknown lexical changes or
uncaptured mode defaults stop interpreting later directives and mark participation
unknown. Broader lexical modes, compiler expressions/macros and complete SDK/default capture
remain required before general language coverage can be claimed.

Tokens, expanded bytes, depth, occurrences, states and diagnostics have independent
positive fluent budgets. Limits/cancellation refuse the complete candidate. The
current native checks include Nyx's real callback gate and a matching actual native
compiler assertion; older/browser profiles are captured-data simulations. Browser
consumers compile only. WORK.md owns exact receipts and remaining incremental/
project-scale cancellation/budgets. Structured type expressions, semantic binding,
provider/editor adoption and full platform/quality/delivery remain open. No MCP
tool/schema, default engine or active design is changed by these primitives.

## Ordered lexical mode and include contexts

`NewNyxPascalContextualLexicalSnapshot` scans the original source once, notifying
an explicit construction-only `INyxPascalLexicalObserver` after each token. The
visitor may change the next comment's nesting policy without rewriting text,
changing quotas or exposing a mutable snapshot. Publication releases the visitor
and cancellation scope. `Options` describes the initial policy; individual tokens
retain their actual spans, physical line/column, spelling and completeness.

Translation gives each include occurrence its own lexical construction. A mode or
nesting directive inside an include affects the caller's next token. The same
physical bytes can consequently have different token ordinals on later visits.
`INyxPascalTranslationLexicalContexts` and `NyxPascalOccurrenceLexical` expose the
exact occurrence snapshot. `PhysicalToken`/`Text` route through it, and syntax
navigation and completion definitions preserve that identity. Legacy `Physical`
file-only access refuses differing occurrence policies/tokenizations. Alternate
translators retain the helper's legacy fallback; no existing interface GUID changes.

Known active FPC source modes now include FPC, ObjFPC, Delphi, DelphiUnicode and
TP. Translation records their verified nested-comment, H/string, Unicode-string
and OUT defaults and updates mode defines without inventing platform/version
facts. Version-sensitive callback/helper defaults remain unknown after mode reset.
The independent native compiler oracle checks each dialect in a separate job:
FPC permits [one source MODE directive per module](https://www.freepascal.org/docs-html/3.0.0/user/userse61.html).
Repeated or uncertain conditional modes refuse; other placement/grammar rules still require
compiler admission. Known NESTEDCOMMENTS on/off and +/- modeswitches work in source
order; OUT has a typed feature. The official [comment rules](https://docs.freepascal.org/daily/doc/ref/refse2.html)
and [mode switches](https://docs.freepascal.org/docs-html/current/prog/progsu105.html)
describe these compiler behaviors. Recording facts does not implement all enabled grammar.

Unknown comment termination blocks subsequent semantic certainty, including after
an include returns; no guessed ENDIF restores it. Inactive changes perform no
mutation. Directive-looking strings/comments are ordinary text. Existing token,
byte, depth, occurrence, state and diagnostic budgets and cancellation still abort
the unpublished candidate. Full RTL/default/target/include capture, broader modes,
incremental work, language meaning and Studio input/navigation/parity remain open.

## Capturing facts from a configured FPC compiler

`nyx.pascal.compilertrace` provides an immutable `INyxPascalCompilerProbe` for a
selected source mode and up to 256 explicit symbol queries, including its eight
identity/mode guards. Names use `NyxPascalDefine`; persistent additions preserve
earlier requests. Generated input forces implicit System setup before its query
block and contains no application source or external includes. It is compiled
for observation; its program, assembler and linker script are never run.

```pascal
LProbe := NewNyxFPCCompilerProbe(NyxPascalDefine('SDK_CONTEXT'))
  .WithSymbol(NyxPascalDefine('PAS2JS'))
  .WithSymbol(NyxPascalDefine('FPC_DOTTEDUNITS'))
  .WithSymbol(NyxPascalDefine('UNICODERTL'));
LFacts := CaptureNyxFPCCompilerContext(LConfiguredCompiler, LFreshPrivateDirectory,
  LProbe, TNyxPascalCompilerTraceOptions.Defaults);
```

The native `nyx.studio.pascalprofiles` host uses the existing compiler-process
family adapter, a fresh private directory, a source read lease, structured
parameters, `-s -Cn` and numbered English messages. It preserves source/log/
intermediates and leaves existing directories untouched. Output defaults to
4 MiB and compiler time to 30 seconds. Cancellation, deadline, excess output and
failures retire and join the exact process family before returning. A caller's
inline cancellation interface is retained through retirement. The operation is
synchronous and opt-in; UI callers need an owning worker. It neither selects an
application output nor changes Studio/project/compiler configuration. Current
native qualification is Win32/i386/FPC 3.3.1; the existing process adapter owns
other-host limits.

The portable reader interprets actual define/undef/value events and explicit
source-owned query replies. It requires consistent four-part numeric version
facts, FPC identity, matching mode guards, ordered unique query rows, the H-option
reply, a successful completion message and terminal zero exit. Failed, truncated,
foreign, duplicate or contradictory replies refuse atomically. Query notes use
the compiler's source-qualified message 2025; generic info output lacks those
positions. Compiler [feedback options](https://www.freepascal.org/docs-html/current/user/usersu13.html)
describe the selected message classes. Exact signed-32 values use guarded
arithmetic on both targets; replacements/oversized values remain defined with
unknown numeric meaning. Source-mode defaults restore only independently
qualified features, never closure enablement from version capability alone.

`DefinesComplete` stays false. Observed positives and explicitly queried
absences do not prove the absence of unqueried future symbols. The transcript
parser is not an authority or execution receipt: its caller owns the actual
compiler invocation and exact source/job association. Full target/ABI/options,
custom application configuration, broader compiler/message compatibility and
default Studio adoption remain required.

Actual SDK continuation identifies a source-encoding prerequisite:
`rtl/objpas/sysutils/strg.inc` contains bytes rejected by strict UTF-8 capture.
`ENyxPascalSourceEncoding` preserves its virtual file reference while remaining
an `ENyxBytes` failure; private host paths and guessed ANSI conversion are
excluded. The explicit byte/view contract below now permits retaining this whole
directory with an opaque file. SysUtils passes `sysutilh.inc`'s MACRO ON and both
selected replacement definitions and explicit inlining policy, then reaches
missing conditional/target-size facts in later headers. The intended legacy
encoding, full textual-macro effects, broader grammar and complete RTL
interface remain open requirements.

## Original source bytes and explicit decoding

`nyx.pascal.encoding` separates immutable `INyxPascalSourceBytes` from a readable
`INyxPascalSourceView`. Neither opens files or grants compiler/process authority.
`TNyxPascalByteOrigin` distinguishes captured bytes from canonical authoring UTF-8;
it is descriptive metadata, not disk-I/O provenance. Returned slices are mutable
copies. The view owns its exact original packet, Unicode text, codec identity and
complete scalar partition after the codec/caller/provider has retired.

Callers choose a typed codec object: strict UTF-8, explicit little/big-endian
UTF-16, a supplied single-byte Unicode map, or an alternate interface. No ANSI
default, encoding sniff or replacement-character fallback is selected. BOMs are
retained as U+FEFF; NUL, CR/LF and supplementary scalars preserve exact bytes.
An encoding reference names the supplied interpretation, without dispatching a
decoder by string. Decoding a source does not establish that FPC accepts that
encoding or an application's source-codepage configuration.

`TNyxPascalByteSpan` is distinct from scalar spans. `ByteOffset`,
`ScalarBoundaryAt` and `OriginalSpan` translate checked boundaries, including EOF;
an interior encoded-byte position returns -1 rather than rounding. Fixed-width
input needs no mapping vectors; variable widths use byte widths and checkpoints
every 128 scalars. `NyxPascalOriginalSpan` retains the translation occurrence.
Ready owned completion results optionally expose `INyxPascalCompletionByteOrigins`,
so navigation can retain the source packet and exact definition bytes without
reopening a changed file. This is an in-process capability, not a wire addition.

Original and decoded UTF-8 each have a four-MiB limit. Capturing alternate
implementations validates bounds before enumeration and detaches their bytes,
text and complete partition. Codec callbacks consume 1..255 bytes per valid
scalar and cannot discard bytes. Cancellation, budget and callback/IO failures
publish no partial view. Undecodable input has a typed error and byte offset;
decode-budget failure is separate and must never become an opaque fallback.

`INyxPascalByteFileCatalog` adds immutable original packets and optional views to
the existing catalog without changing its original GUID. `WithByteSource` checks
that a supplied view belongs to the exact bytes/origin. `TextAt` refuses opaque
files with their virtual reference. `WithFile` and legacy text-only catalogs
create explicitly marked authoring UTF-8, never reconstructed external evidence.
Per-file/catalog work charges the larger of original bytes and decoded UTF-8.

Native directory capture defaults to `pscpStrictUTF8`. Explicit
`pscpUTF8ViewsWithOpaqueBytes` retains undecodable packets with no readable view;
it does not drop them or guess another encoding. Reached opaque includes stop
certainty with `ptiSourceEncoding`; opaque indexed roots return a typed compiler-
context gap. An authored CODEPAGE directive cannot silently contradict its view.
The actual 48-file SysUtils directory retains every byte, including one opaque
file. This advances include capture, while complete macro/default/target/define/language
meaning, incremental budgets, editor adoption/input, full parity and delivery
remain under the original resolver owners. WORK.md owns bounded evidence.

## Ordered replacement macros

`nyx.pascal.macros` owns immutable replacement environments and detached expanded
tokens. The [FPC macro contract](https://www.freepascal.org/docs-html/prog/progse5.html)
is the language reference; an independent handwritten native compiler oracle
qualifies the supported cases separately from Nyx's own output. The browser
build of this analyzer can interpret captured FPC source; compilation of that
analyzer is not admission or execution of a macro-authored pas2js application.

`TNyxPascalMacroDefinition` retains a typed symbol, exact replacement text and its
physical occurrence/scalar span. Explicit host integer facts use occurrence -1;
no file location is invented. Names compare without ASCII case. Redefinition
returns a new table, aliases resolve at each use, and undef removes replacement
status. The environment belongs to one translation unit, with textual include
side effects and independently retained historical states; imported units do
not inherit each other's source definitions.

`ppfReplacementMacros` has three-valued enablement. Known MACRO ON/OFF and active
definitions flow in source order. Uncertain writes retain unknown replacement
meaning, never a chosen body. An unavailable include or unknown textual effect
revokes retained replacement knowledge while preserving its source evidence.
No compiler/version/build-target default is guessed.
Assignment definitions made while macro support is disabled remain unqualified
and cannot later authorize expansion without a known replacement definition.
Plain redefinition of an existing replacement clears its text, retaining its
replacement status. Escaped identifiers, reserved words, strings and comments
are not substituted. Supported condition expressions expand aliases recursively
while protecting `defined()`'s symbol argument.

`INyxPascalMacroExpansion` owns effective fragment tokens and recursive traces.
Each trace retains the exact definition and its parent use. Construction alone
borrows cancellation and the environment; detached results retain neither.
Cycles diagnose distinctly, and quotas/cancellation refuse without a partial
published packet. Tables cap at 2048 names/four MiB of replacement text. The
owning translation charges invocation, replacement, re-tokenization and boundary
work bytes against its expanded-input limit; token, depth, state and occurrence
limits bound the other collections, including empty replacements.

`INyxPascalTranslationMacros` retains expansion packets and exact replacement
tables beside saved compiler states. Equal Boolean define facts do not collapse
different replacement bodies. `PhysicalToken` and `Text` retain the authored
invocation, while `NyxPascalEffectiveToken/Text` supply the replacement's spelling
and token family. The existing syntax projection consumes effective tokens;
its `TNyxPascalSyntaxOrigin.Expansion` identifies nonlinear origins. Partial
projection slices retain the complete invocation span. A caret inside a macro
invocation returns no invented expanded character position. Completion can bind
replacement-generated record members and returns the exact authored invocation
as its physical definition; the translation trace separately retains where that
replacement body was defined.

The current token bridge refuses replacement directives, invalid fragments and
cross-boundary token joining that need composition with the original reader.
It re-tokenizes joined fragments and guards nearby physical boundaries with a
cached canonical UTF-8 map, without replacing the original source bytes. These
are explicit remaining full textual-macro requirements, alongside broader
compiler/codepage/default/target/define context and language/editor/parity work.
The actual RTL now passes INLINE ON in sysutilh.inc with its exact SUT/BU
definitions retained, then reaches missing conditional/target-size facts in later
headers. This is a bounded resolver prerequisite, not complete
RTL meaning, a default Studio engine, an MCP schema change or delivered parity.

## Typed local compiler policies and settings scopes

`TNyxPascalCompilerPolicies` records explicit inlining knowledge, symbolic record
alignment, minimum enum storage and declared default calling convention. Values
are strongly typed, independent copies. Unknown is distinct from Default or C;
the latter preserve target/mode intent without supplying an invented ABI size,
field offset or procedural-type compatibility result.

Set packing is separately typed as `TNyxPascalSetPacking`. `PackSets` retains
one/two/four/eight-byte allocation granularity or the explicit historical Fixed
policy; it does not calculate a set's size. `RequestSmartLinking` records a
module-level request independently of effective linker support/output. The
reader handles PACKSET, SMARTLINK ON/OFF and the CLASS mode feature. Unknown
conditional writes revoke only their captured knowledge. POP restores local set
packing while preserving module-link intent and global class features. A source
mode reset revokes packing and retains link intent. Actual compiler input checks
set packing before/inside/after POP; broader ABI and directive placement stay open.

```pascal
LContext := NyxPascalWithCompilerPolicies(LContext,
  TNyxPascalCompilerPolicies.Unknown
    .WithInlining(ppkTrue)
    .AlignRecords(praC)
    .StoreEnums(pes4)
    .CallBy(pccCdecl));
LContext := NyxPascalWithLocalSwitch(LContext, plsRangeChecks, ppkTrue);
```

Optional `INyxPascalContextPolicies` keeps the original compiler-context GUID
unchanged. Capturing a legacy context supplies unknown policies. Capture,
SameFacts, immutable fluent updates, mode resets and unknown effects preserve or
revoke this metadata explicitly. Alternate capture checks a bounded symbol count
once and publishes only after all getters/policy validation succeed.

The source boundary recognizes INLINE ON/OFF, PACKRECORDS numeric/C/default,
numeric ALIGN/A aliases, PACKENUM/MINENUMSIZE/Z values, CALLING and supported long
Boolean aliases. Long forms share the corresponding short-switch facts, including
IFOPT behavior. Invalid values diagnose; uncertain writes revoke their particular
policy. Mode-dependent Boolean A aliases remain an explicit gap. No compiler's
defaults are borrowed from the analyzer's own build.

`RestoreNyxPascalLocalSettings` restores supported local short switches, symbolic
policies and scoped-enum state. Current defines, replacement bodies, macro
enablement and global mode features survive. This follows the
[local settings contract](https://www.freepascal.org/docs-html/current/prog/progsu62.html)
with the installed compiler's behavior independently checked by handwritten input:
record/enum sizes, compatible calling signatures, conditional short flags and
global mode/macro persistence. That native oracle is specific to this configured
Win32/i386 compiler; it does not prove another target's ABI or actual inlining.

PUSH/POP flow across textual includes. Each historical translation state owns its
independent stack of immutable saved contexts and physical PUSH origins, alongside
current compiler facts and replacement tables. A stack change cannot coalesce
states merely because their current option values match. Optional
`INyxPascalTranslationSettings` exposes bounded state/depth/frame/origin reads.
Unknown textual effects revoke stack-shape knowledge; callers inspect SettingsKnown,
and the remaining stack getters refuse unknown states. Earlier known states stay
readable. Stack depth and retained-state quotas refuse atomically.

FPC can carry a settings stack across compilation units. The current translator
does not invent an inherited frame: an uncaptured POP or a stack continuing beyond
the unit remains a context gap with retained source evidence. Conditional unknown
stack effects refuse rather than guessing shape. Complete compilation-order
continuation, target/ABI/type-size facts, wider policies, global directive placement
and full language/editor/parity/delivery remain under their original owners.

This foundation reached 38 unknown conditional decisions in real SysUtils.
The explicit request continuation below resolves compiler names while keeping
authored type scope independent of the analyzer's host integer size.

## Target measurements and conditional requests

`TNyxPascalSizeReference` describes a checked named `sizeof` operand, separately
from a compiler define or a bound type. It accepts at most eight ASCII identifier
parts, 128 characters each and 1024 total; calls, indexing, generic expressions,
escaped identifiers and injected directives refuse. Case normalizes while
qualification stays intact.

`INyxPascalSizeProbe` optionally extends the existing FPC probe with up to 64
deduplicated operands. `NyxFPCProbeSizeOf` and `CaptureNyxFPCCompilerProbe` preserve
ordinary symbol queries and checked source. Alternate counts are bounded before
element callbacks. The native host freezes both vectors before writing its owned
source and retains its existing process-family retirement, deadline, cancellation,
read lease and output limits. A probe compiles without running application code.

Each size response has an ordered status and, when known, 31 ordered exact bits.
Missing, repeated, altered or reordered replies refuse the whole context.
Unqualified absence has its own status. FPC's `declared()` does not accept the
qualified probe operand: a qualified size is compiled directly, and an unavailable
export refuses that job rather than inventing an absence. The optional
`INyxPascalContextSizes` capability stores independent unknown, undeclared, known
and out-of-range states. Known zero stays distinct from unknown; reading `Bytes`
without a known value refuses. Each fact records its probe mode and long-string
policy. Immutable copies, capture and equality retain these facts; a changed mode
or unknown source effect revokes the old measurements.

These are facts about the probe's pristine source scope. They do not bind names
in a different unit. The independent Win32 Delphi compiler oracle observes four
bytes for bare `Integer`, one after a local `Integer = Byte`, and two for
`System.Integer` both before and after that local declaration. The configured
probe separately measures the pristine aliases and pointer. This is concrete
evidence against equating a mode alias, an imported export and a local declaration.
FPC documents the [compile-time byte result of sizeof](https://www.freepascal.org/docs-html/current/rtl/system/sizeof.html)
and the [mode-dependent Integer alias](https://docs.freepascal.org/docs-html/current/ref/refsu4.html).
No captured measurement silently enables arbitrary source `sizeof` evaluation.

`PlanNyxPascalCondition` reuses the bounded conditional parser to produce an
immutable `INyxPascalConditionNeeds`: missing compiler symbols in first-use order
and a separate vector of simple named size operands. The plan owns no context,
source, editor, callbacks or process. Known compiler symbols are omitted;
duplicates collapse. Unsupported or malformed syntax publishes no requests.
Existing 16 KiB, 512-token and 64-level bounds apply, with explicit 256-symbol and
64-size caps. Planning selects no branch, performs no I/O and does not turn a
type operand into a define. `declared()` still requires semantic scope support.

The actual RTL consumer starts from the captured SysUtils root, associates each
unknown directive with its original pre-token state, and queries only the planned
compiler names in one fresh owned job. It re-translates the same root and files;
it never replaces dependencies with export stubs or seals unqueried definitions.
WORK.md owns the accepted native reduction and exact remaining expressions.
The retained fallback FATAL remains a context diagnostic, not a selected compiler
error. Full intrinsic/import/local binding, type layout, grammar, compiler order,
incremental budgets and Studio adoption/input/navigation/quality/parity/delivery
remain required. Browser consumers compile only in this checkpoint.

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

## Procedural declaration suffixes

Procedural type ranges now retain their supported trailing calling/varargs/inline
directives across signature semicolons. Aliases, inline record fields and inline
variables feed the same complete range into the existing structured type parser.
Following declarations keep their own ranges; escaped convention names remain
identifiers. The standalone type reader also refuses an escaped identifier as a
calling directive. Conflicting conventions and unfinished types stay diagnostic;
range recovery does not establish compiler eligibility or ABI compatibility.

For example, `TCompute = function(A: LongInt): LongInt; cdecl;` exposes the full
function plus `cdecl` in its TypeSpan and typed CallingConvention, without
inventing an exported type named cdecl. Original source, trivia and physical
include/scalar mappings remain unchanged. The analysis projection is never used
as an authored replacement buffer.

The whole handwritten routine.fixture unit/include compiles and executes aliased,
inline-field and inline-variable callbacks. Its parsed convention maps back to
the exact included token after supplementary Unicode text. In the complete
captured Windows unit, the real BFFCALLBACK now has a complete typed stdcall
signature and physical origin; 78 spurious declarations/diagnostics disappear.
That checkpoint decreased syntax diagnostics from 110 to 32, followed by 18
operator and 12 variant issues. The [anonymous continuation](#anonymous-structured-type-ownership)
now passes the supported whole-Windows declaration grammar and returns its actual
wYear member. This does not establish complete SDK type meaning. WORK.md owns exact native
evidence; browser consumers compile only. Full language/context/canonical typing,
incremental budgets, editor/input/navigation/quality/parity/delivery remain open.

## Typed record operators

`TNyxPascalOperatorKind` retains a closed declared operator family separately from
its original name and signature. Symbolic/named spellings remain exact source;
unary/binary arity and operand/result types remain in their routine scopes.
Operators retain the existing routine kind for compatibility. Family equality
does not establish compiler eligibility, conversion equivalence or call selection.
Unknown spellings keep explicit unsupported-scope diagnostics.

Optional `INyxPascalOperatorSyntax.NamedOperator` indexes a family inside one
immutable scope. Ordinary `Named` remains independent, including a normal method
called Add beside the named Add operator. `NextNamed` traverses the family's
alternatives. No synthetic Pascal identifier replaces an authored operator token.
Global FPC declarations also retain their explicit result identifier in the
operator's local scope. Qualified implementation owners and exact physical
name/header/result/parameter locations remain available.

Optional `INyxPascalOperatorQueries.OperatorMembers` uses the existing world,
receiver type and caller site. It returns bounded declared alternatives with
visibility, generic context, cancellation and stale-world guards. Exhaustion
refuses atomically. Ordinary member pages exclude operators; method completion
retains its existing contract. These are in-process optional capabilities, with
the original interfaces/GUIDs and MCP transport/tool inventory unchanged.

The whole handwritten compiler input executes symbolic/named/unary/equality/
implicit/explicit record operations and an ordinary Add method in Delphi mode.
A separate ObjFPC unit executes the global operator grammar; this configured FPC
does not admit that global form in Delphi mode. Analysis preserves both actual
units and their source mode distinction. The controlled lookup world deliberately
uses the existing symbolic compatibility policy, not replacement RTL exports.
This qualifies declared families/results/origins, not expression overload ranking
or complete compiler-unit/target semantics.

At the operator checkpoint, exact TSize inspection returned its
typed equality declaration at the original shared include token without supplying
synthetic System/ObjPas. Syntax retained 7,614 declarations/18 diagnostics,
beginning with variant fields and declaration hints; whole-unit indexed completion
refused unsupported scope. The [anonymous continuation](#anonymous-structured-type-ownership)
now reaches an actual ready Windows member. WORK.md owns native compiler/
consumer receipts and retained failures; browser consumers compile only. Full
grammar/SDK/canonical typing/incremental/editor/input/navigation/quality/parity/
delivery remain original authoring requirements.

## Variant records and declaration hints

Optional `INyxPascalVariantSyntax` exposes snapshot-local selector, arm and label
records. A selector retains its record scope, optional real tag declaration,
parent arm and exact type range. Arms retain ordered label/body ranges and nested
relationships. Grouped labels, signs and ranges remain unevaluated source; no
active arm, ordinal legality, ABI storage or offset is inferred. `Complete`
describes the recognized boundary; enclosing scope diagnostics still apply.

Fields keep the ordinary enclosing record's member scope. Their `VariantArm`
ordinal identifies an alternative; -1 identifies a fixed field or an outer tag.
A nested named tag belongs to its parent arm. Unnamed selector types never
become invented fields. Existing alias/member lookup and bounded member pages
therefore see all alternatives without a hidden scope or runtime arm selection.
Incomplete/unsupported structures retain unsafe-scope diagnostics.

Optional `INyxPascalHintSyntax` retains typed deprecated, platform, experimental
and unimplemented intent independently of declaration type/value spans. Each
declaration owns its ordered `FirstHint`/`NextHint` links, including grouped names.
Messages retain original string/character source rather than decoded or executed
text. Escaped names and expression operands remain identifiers. Hint presence
does not emit a compiler diagnostic or alter alias identity. Original interface
GUIDs and MCP schemas are unchanged; these capabilities operate in process.

Variant and hint entries share the existing aggregate declaration budget.
Construction limits/cancellation refuse atomically; checked getters reject
out-of-range ordinals. Value records contain no interface arrays or editor borrow.
Translated views retain per-token original include/scalar mappings; their spaced
analysis projection never replaces authored source.

The whole handwritten Delphi-mode unit/include compiles and executes ordinary,
nested and overlapping ordinal fields under the configured native compiler.
Its physical include identities and hinted alias remain exact after supplementary
Unicode. Controlled member pages qualify declared access separately from actual
compiler storage. That unchanged Windows capture retained 10,571 declarations,
11 selectors/30 arms/30 labels, ten hints and 12 diagnostics. Exact TRect member
inspection and BROWSEINFO hint origins pass without replacement implicit units.
Its anonymous Event record boundary is addressed by the
[anonymous-type continuation](#anonymous-structured-type-ownership). Full remaining grammar/import/RTL/canonical/
expression meaning, incremental budgets, editor/input/navigation/quality/parity/
delivery remain original authoring requirements. WORK.md owns native evidence,
retained failures and browser compilation only.

## Anonymous structured type ownership

Inline bodies now have ordinary type declarations with `Anonymous = True`, an
empty Name and their original structured keyword as NameSpan. Each owns a member
scope. Grouped declarations share one expression/type identity; separate nested
bodies retain distinct identities. They never enter the named lookup index or
ordinary member suggestion list. This grants no generated Pascal name, storage
offset or canonical assignment compatibility.

Optional `INyxPascalStructuredSyntax.TypeAt` maps an exact scalar body start,
including authored packing, to its declaration ordinal. Named bodies use the
same index/grammar. `NewNyxPascalDeclaredTypeSyntax` reads a window against its
actual snapshot and emits `ptxStructured` leaves linked through optional
`INyxPascalDeclaredTypeSyntax`. This retains one immutable source owner rather
than relexing a copied body. The standalone factory still refuses structured
bodies without that context. Original interfaces/GUIDs remain unchanged.

The binder checks that source lease before resolving a leaf through its ordinary
declared type. Arrays, aliases, concrete generic environments and multi-level
member receivers retain existing lifetime, visibility, world and query-budget
guards. A new body's variant scope resets independently from its enclosing
field's arm. Method-pointer `of object` remains a procedural qualifier; a final
record field may omit its semicolon before end. Incomplete bodies and construction
limits/cancellation retain explicit refusal. Default editor quotas stay unchanged.

The whole handwritten Delphi-mode compiler input executes grouped, nested,
packed-array, generic, local and typed-constant record access. Ordinary completion
also follows an anonymous local receiver. The unchanged Windows input now retains
29,823 declarations with zero supported declaration-grammar diagnostics, plus
58 selectors/138 arms/153 labels and eleven hints. Its explicit whole-unit quota
is 100,000 entries, above the unchanged 16,384 default. Exact INPUT_RECORD.Event.
KeyEvent member inspection and native compiled access retain original include
tokens. Indexed completion loads Windows once and returns wYear with its exact
physical navigation after provider retirement. Resolving the inspected WORD field
type still refuses the uncaptured ObjPas implicit unit; no replacement exports
are supplied. Full compiler/import/RTL/canonical/expression meaning, incremental
budgets and ordinary editor/input/navigation/quality/parity/delivery remain open.
WORK.md owns exact native evidence and browser compilation only.

## Compiler requirements and module preparation

Optional `INyxPascalUnresolvedConditions` retains the expression actually evaluated
after ordered macro expansion, plus its original occurrence/span, pre-token state
and prefix position. Unsupported expansion and IFOPT remain unplannable. Prefixes
retain only earlier sites and preserve their immutable source owner; alternate
sources without these receipts refuse planning rather than reconstructing meaning.

`PlanNyxPascalCompilerRequirements` returns a bounded immutable plan with separate
compiler symbols, source sizeof operands and source declared names. It checks each
site against its owning row. Repeated includes retain distinct states. Extending
a frozen compiler probe adds symbols in first-use order and preserves existing
requests/measurements; it never converts scoped sizes to pristine compiler facts.
The original 256-symbol probe bound and plan input/output/cancellation guards
remain explicit. No source, compiler process, editor or project is mutated here.

Native `PrepareNyxFPCModule` freezes explicit files/descriptor/context and uses a
fresh caller-owned scratch directory. New symbol requests trigger joined compiler
probes, with aggregate token/probe limits, per-job timeout/transcript limits and
compiler family/version/mode guards. Its fluent value options are independent of
the exported design. The returned descriptor and exact remaining requirements
feed the existing indexed loader. Preparation leaves scoped conditions, includes
and unsupported source unresolved; it proves neither binding nor application
compilation/execution. No working toolchain or default Studio setup is replaced.

```pascal
LPrepared := PrepareNyxFPCModule(LCompiler, LFreshDirectory, LFiles, LModule,
  LBaseProbe, TNyxFPCModulePreparationOptions.Defaults.UpToProbes(4));
LModules := LModules.WithModule(LPrepared.ModuleSource);
```

Whole handwritten unit/include compilation executes the real selected branch and
its record access; the prepared descriptor supplies ordinary indexed completion.
Already prepared inputs need no new probe. The actual Win32/i386 System and ObjPas
inputs retain all 156 original byte packets, independent FPC/ObjFPC modes and one
new symbol probe per unit in that preparation checkpoint. The codegen continuation
now retains System's routine register intent and advances its complete captured
Win32 include search, exposing genuine directive/declaration grammar limits.
ObjPas retains its source-scoped
sizeof(Char) requirement despite a pristine System.Char measurement. Indexed
loading reaches both real units and returns typed incomplete ObjPas source after
two loads; no synthetic exports authorize a successful type result. Maintained
SysUtils requirement capture also consumes this public planner. Full source/
compiler/import/RTL/canonical/expression meaning, incremental budgets, ordinary
editor/input/navigation/quality/parity/delivery remain open. WORK.md owns exact
native evidence, retained failed preparations and browser compilation only.

## Assembly regions and grouped settings

Short switch groups such as `{$I-,Q-,H-,R-,V-}` are settings rather than include
filenames. Translation validates the entire group before publishing one context
state; repeated letters keep their last authored value. At most 26 immutable
context copies are needed, regardless of repetition. Malformed tails and unresolved
mode-dependent alignment aliases refuse without a partially applied prefix.
PUSH/POP and known/unknown conditional activity keep their existing contracts.
Ordinary `$I filename` and `$INCLUDE filename` retain textual include behavior.

`TNyxPascalToken.Region` records the entry envelope (`plrPascal` or `plrAssembly`).
ASM enters assembly for its successor; its unquoted END returns to Pascal.
Instruction operands have `ptkAssembly` rather than Pascal word/number meaning;
strings, comments, directives, whitespace and original scalar/storage coordinates
remain lossless. The x86 Intel/AT&T envelope handles backslash-escaped assembly
quotes without interpreting instructions, registers, ABI layout or operand types.
It follows FPC's documented [assembly block contract](https://docs.freepascal.org/docs-html/current/prog/progse9.html)
and retained installed-reader inputs; other assembler dialects require their own
comment/register-list qualification. Invalid binary literals outside ASM still
produce ordinary Pascal diagnostics.

The optional `INyxPascalLexicalRegionObserver` extends the construction-only
visitor without replacing its original interface. Translation controls selected
and physical branch regions separately, so discarded ASM/END tokens cannot alter
the selected source. Includes may open, continue or close an assembly envelope;
the whole root owns its EOF diagnostic. Unknown boundary activity remains an
explicit refusal. No source, editor or callback borrow survives construction.
Original token-family ordinals and existing interface GUIDs remain unchanged.

Native qualification executes a whole handwritten assembly function and observes
all five compiled switch values, alongside branch/include/quote/span/limit and
cancellation cases. That checkpoint advanced actual original System from 50
diagnostics to one at `{$maxfpuregisters 0}`. The codegen continuation below follows
that request without ignoring an unknown policy to force a resolved unit.
ObjPas source-scoped Char binding and complete implicit-unit meaning stay
open. Browser consumers compile only; this is neither browser execution nor
ordinary Studio adoption, parity or delivery. WORK.md owns final evidence and
the original authoring criterion's remaining outcome.

## Local assembler readers and routine register directives

`TNyxPascalCompilerPolicies.ReadAssemblyBy` retains `parDefault`, `parIntel` or
`parATT` as independent symbolic intent. Unknown never guesses the target's
default. Reader policy participates in exact compiler-context identity, frozen
capture and the local settings stack; includes share it and POP restores it.
Source ASMMODE changes need an explicit FPC context outside ASM. Unsupported
readers and in-block changes refuse; uncertain activity revokes reader knowledge.
Recording the reader does not establish target availability or admit instructions.

MAXFPUREGISTERS has a different [routine-codegen lifetime](https://www.freepascal.org/docs-html/current/prog/progsu49.html).
The resolver retains it through optional `INyxPascalRegisterDirectives`, with
typed `TNyxPascalFPURegisterPolicy`, original occurrence/span/pre-token state and
three-valued authored activity. NORMAL/DEFAULT retain a default request rather
than an invented effective count. Decimal counts are checked before publication;
the documented range 0..7 remains available, while eight requires an explicit
3.3.1-or-newer FPC context and is compiler-qualified by the native fixture.
This is a conservative capture rule, not an execution claim for older compilers.

These are point-in-source requests, not an effective global register setting.
The resolver does not extend them into another routine or synthesize a POP
restoration that FPC's local settings stack does not own. Immutable prefix views
expose only earlier requests, and preserve exact repeated-include ownership.
Records contain no interfaces; token budgets, checked ordinals and cancellation
still govern construction. Original context/translation interfaces keep their
GUIDs; the optional query is in process, with no MCP tool or schema change.

The whole handwritten unit compiles and executes both assembly readers, actual
POP restoration and floating-point code, including a compiled eight-register
request. Its exact original bytes also feed ordinary translation and syntax.
That establishes admission and behavior of the fixture, not a particular register
allocation. Browser consumers compile only. The real System route retains all
original captured bytes and now searches the explicit Win32 root directory too.
It exposes trailing directive prose and declaration nesting at the unchanged
parser ceiling; ordinary indexed completion returns a budget refusal with no
partial suggestions. No source truncation, synthetic exports or increased ceiling
supplies complete RTL meaning. Full grammar/context/incremental/adoption/input/
navigation/quality/parity/delivery remain open under original authoring criterion two.

## Native linkage and layout directive values

`TNyxPascalSyntaxDeclaration.Linkage` separates declared object/linker intent from
Pascal member visibility. The typed flags cover PUBLIC, EXPORT, EXTERNAL, ALIAS,
CVAR, compiler helpers/intrinsics and bracket platform syntax. Known suffixes stay
attached to their variable, constant or routine; they cannot turn later unit
prototypes into nested bodies or consume the next class visibility section.
Grouped declarations own independent metadata values. Unknown platform attributes
retain explicit unsupported syntax, and malformed suffixes retain incomplete
declarations rather than authorizing successful inference.

`LinkageSpan` preserves the suffix range, including operands and intervening
modifiers. As with other declaration spans, it belongs to the syntax snapshot.
A translated view's origin table maps it back to exact physical tokens, including
Unicode spelling and repeated includes; synthetic token separators never become
authored source. Multiple original occurrences require separate mapped parts.
The metadata does not decode an external name into a Pascal declaration name,
validate a linker target or supply an intrinsic implementation. Full argument,
pre-separator/dialect eligibility and ABI rules remain with the original parser/
compiler/type requirements. FPC documents the distinct [PUBLIC modifier](https://docs.freepascal.org/daily/doc/ref/refsu94.html)
and [alias meaning](https://docs.freepascal.org/docs-html/current/ref/refsu72.html).

An explicit `ppfCVariables` grammar feature retains `{$modeswitch cvar}` without
borrowing the analyzer compiler's defaults. The whole owned fixture remains in
Delphi mode and enables that native grammar explicitly. Bracket examples retain
their stated source dialect; parsing them does not claim Delphi compiler admission.
The layout-policy readers (record alignment, enum storage including
MINENUMSIZE and set packing) consume one whitespace-delimited value and retain
the rest as explanatory comment text. Invalid leading values still refuse.
This rule never truncates conditions, macro replacements, quoted include names
or arbitrary directives; broader directive argument syntax stays open.

Native qualification executes the whole unchanged unit's public variable/routine
behavior and reads actual compiler-produced sizes for all four layout examples.
The original bytes also pass ordinary translation and syntax. A 512-routine
interface stress case stays at structural depth 16; class visibility remains
independent. Browser consumers compile only. WORK.md owns the complete real RTL
continuation and exact remaining gaps, with full source/type/incremental/editor/
input/navigation/quality/parity/delivery still under original authoring criterion two.

## Compiler bootstrap and authored alias identity

`TNyxPascalCompilerScope` distinguishes an ordinary pristine program packet from
`pcsoSystemBootstrap` evidence collected before authored System aliases exist.
`NewNyxFPCSystemBootstrapProbe` emits an owned empty System type section: the TYPE
token forces compiler setup before the query directives are scanned. It declares
no substitute SDK exports. Unqualified presence/size requests retain the existing
query/byte/line budgets; dotted references refuse at this boundary.

The native capture host runs the configured compiler with `-Us`, without running
an assembler, linker or application. The probe intentionally stops after its
complete ordered replies. Only exit 1, its exact owned fatal stop and the compiler
abort receipt together admit the packet. Ordinary program probes still require
exit zero. Unexpected errors, missing/foreign/duplicate stops, incomplete packets
and source-identity mismatches publish no partial facts. This is compiler evidence,
not successful System compilation or application execution provenance.

Optional `INyxPascalCompilerScope` retains a detached root context on the ordinary
source context through `NyxPascalWithBootstrapContext`. Attachment validates
family/version and rejects reverse/nested leases; contexts store the interface in
an owning class field. Capture/equality retain both namespaces. Source MODE/H
changes preserve the independent root packet, while unknown conditional state
drops it. Module preparation retains that packet across its ordinary symbol
captures and refuses bootstrap requests as a replacement source baseline. Full
target/SDK/configuration coherence remains the caller's broader context contract.

Binding first resolves authored declarations and imports. Only System's owner
can supply the final compiler-root fallback. Measured absence returns Missing;
unqueried names retain an explicit compiler-context gap. A bound primitive has
`pioSystemBootstrap`, its unqualified measured reference and its own immutable
context. The conditional reader checks that namespace before using sizes; authored
aliases unwrap through their actual source targets. `Comp` has its own appended
builtin category. No existing enum ordinal or interface GUID changes.

The configured Win32/i386 FPC 3.3.1 capture provides AnsiChar, WideChar and Comp
before source aliases; Char, Integer, NativeInt and UTF8String are absent there.
Those are facts of this measured compiler, not universal FPC defaults. The whole
original System source binds its Char alias to the measured AnsiChar at the exact
declaration site. Its later sizeof(Char) query remains blocked inside unfinished
TObject scope; original warning/syntax gaps remain visible. Portable controlled
checks establish precedence/ownership algorithms, separately from that real RTL
evidence. Browser consumers/products compile only. Full language/incremental/
editor/input/navigation/quality/parity/adoption/delivery remain open in WORK.md.

## Lexical names in unfinished structured scopes

Existing `INyxPascalSemanticWorld.Lookup` now searches prior lexical names through
the header of an enclosing unfinished class, interface, object or record. The
owned body must reach EOF with a validated header and complete translated input;
earlier syntax/conditional uncertainty still refuses. No complete receiver type
is constructed for that open body. `TypeOfSymbol` retains its incomplete-type
refusal, and external incomplete imports do not gain absence authority.

Each enclosing member scope supplies its own owner. Own declarations are searched
at the exact caret before ancestors; a later nested declaration cannot change an
earlier field's resolved type, even when the inspected class is now complete.
Out-of-line implementations retain their ordinary full receiver visibility.
Ancestor traversal shares the existing member/overload merge, detached candidate
vector, exact generic environments, recursion checks and step/candidate budgets.
A selected explicit ancestor that cannot bind returns a missing-type gap, rather
than the Missing sentinel which ends a completed base list. Missing modules,
unbound generics, malformed headers and inaccessible/uncertain bases still refuse.

The ordered conditional reader uses `ScopeAt` at the actual EOF caret. Moving
back one scalar could select the outer scope of an empty new body or a completed
method. This matters for both inherited declaration questions and source-alias
size questions; replay still retains the exact original source and origin.

Native qualification compiles and executes the whole authored descendant example,
returning 42 through its inherited nested alias. Actual compiler sizes 1/2 keep
the earlier global Byte field and later nested Word alias distinct. The same
unchanged unit passes whole-source conditional, syntax and field-type inspection.
Its native compiler-only `declared()`/`SizeOf` oracles are explicitly excluded from
pas2js compilation; inherited aliases and runtime source remain shared. Nyx's own
conditional-reader cases compile for both source targets. Ordinary completion
returns a bounded inherited factory type with detached physical navigation, while
source, cancellation, step budgets and malformed/unknown ancestry retain guards.

Actual System now passes the unfinished-owner lookup and retains an earlier
unsupported module-syntax refusal at its Char question. The first retained gap is
the hinted initializer `NoErrMsg: Boolean platform = False` in systemh.inc; whole
RTL/source grammar is still incomplete. Browser consumers/products compile only.
WORK.md owns final receipts, retained preparations and the return to that existing
parser owner. Full language/incremental/editor/input/navigation/quality/parity/
adoption/delivery remain required; no new MCP schema or installed engine follows.

## Hints before variable initializers

The declaration reader accepts hints between a variable's declared type and its
initial value, such as `Flag: Boolean platform = False`. `TypeSpan` remains the
type, ordered `INyxPascalHintSyntax` nodes retain hint/message spans, and
`ExpressionSpan` contains only the value. Existing trailing hint inspection stays
available. A grouped structural declaration receives independent hint links after
both positions have been read; neither group entry owns the other's metadata.
Structural recovery does not establish compiler acceptance of every spelling.

Hint-like identifiers in qualified or operand expressions retain value meaning.
Labels inside record initializers stay inside their grouped expression. Named
callback signatures keep calling directives separate from variable hints; a
calling-convention separator is not silently reinterpreted to attach a following
declaration to that variable. An explicit initializer with no value has an
incomplete flag and diagnostic, so ordinary binding refuses it while a following
separated sibling remains visible. Hint nodes retain the existing construction
budget and atomic failure contract. No public interface/enum or wire schema changes.

The native compiler oracle compiles the whole original unit/include and executes
its hinted initial values, returning 42. It intentionally references marked
symbols: one deprecated and two platform warnings are exact expected evidence,
retained separately without suppressing warning categories. Regular parser/product
builds do not import that compiler-only oracle. Whole-source inspection preserves
all original text and physical include origins; ordinary bounded completion and
detached navigation follow the initialized record to its included member.

Actual System now retains NoErrMsg's Boolean type, platform hint and False value
at its original systemh.inc declaration. The first unsupported syntax before the
Char query advances to `internconst` at scalar 27,733; source meaning is still
incomplete. Browser consumers/products compile only, with no execution/parity
claim. WORK.md owns final receipts, explicit expected compiler diagnostics and
the existing linkage/parser return path. Full language/incremental/editor/input/
navigation/quality/parity/adoption/delivery remain open.

## Declared constant and runtime intrinsics

`plfConstantIntrinsic` is an appended linkage flag for an authored `internconst`
annotation. It stays distinct from the existing runtime `plfIntrinsic`
(`internproc`) flag. A constant annotation preserves an ordinary implementation
body; the runtime annotation retains its bodyless declaration behavior. This
classification is declared source intent, not compiler eligibility or execution.

Optional `INyxPascalIntrinsicSyntax` exposes checked value-owned entries through
`IntrinsicCount`/`IntrinsicAt`. A declaration's `FirstIntrinsic` and each entry's
`NextIntrinsic` retain source order. `TNyxPascalIntrinsicDeclaration` supplies the
closed role, exact key/operand/attribute spans and owning declaration ordinal.
Operands stay source expressions, including symbolic compiler operations; the
reader never converts them to an evaluated integer or executes the operation.
Ranges use the same lexical snapshot and existing physical origin table.

`Complete` establishes source delimiters and a nonempty operand, not a valid
compiler operation or constant expression. Missing colon/value/closing bracket
retains incomplete declaration diagnostics; unknown attributes remain unsupported.
Attribute and declaration entries share one construction quota, and cancellation/
exhaustion publishes no partial snapshot. No record stores interface references.
Existing interface GUIDs and enum ordinals remain intact; the additional optional
capability and expanded declaration record are qualified by rebuilding consumers
on both targets. No MCP wire/schema or installed inventory changes.

The whole authored Delphi-mode compiler companion uses a direct suffix and
retains its ordinary body. On the pinned native compiler, a constant use folds
without entering that body, while a runtime input enters it exactly once. That is
specific compiler evidence; Nyx's declaration metadata does not supply a constant
evaluator. The unchanged RTL's bracket form separately retains Swap's symbolic
operand and original included-file owner. Its remaining syntax diagnostics drop
to 13 at that checkpoint; the RTL-procedure continuation below advances that
source boundary. Browser consumers/products compile only. WORK.md owns the final
source, compiler evidence,
remaining original grammar/editor/parity/delivery requirements and return path.

## Declared RTL procedure roles

Appended `plfRTLProcedure` retains the authored `rtlproc` role independently of
`plfCompilerHelper`, constant annotations and bodyless runtime intrinsics. Both
direct and bracket suffixes preserve original `LinkageSpan` ownership. An RTL
marker has no operand; an authored operand remains unsupported rather than being
silently admitted. Unknown neighboring keys retain their own uncertainty.

The role leaves parameter modes/types, following declarations, class visibility
and ordinary implementation bodies intact. It does not provide the compiler's
special string-codepage parameter matching, aliases, eligibility or runtime
behavior. Existing enum ordinals, interfaces and record layouts stay intact;
affected consumers are rebuilt together. There is no MCP schema change.

The whole Delphi-mode native companion compiles four independent routine bodies.
Calls to its RTL-only helper and following routine return 42. Its separate
`rtlproc; compilerproc` routine compiles and retains exact source/signature/body
metadata; the pinned FPC hides that compiler helper from ordinary Pascal name
lookup, so it is not included in the callable-behavior claim.

Unchanged System source retains the original byte-string helper's RTL/compiler
roles, out parameter and physical include owner. Its declaration count stays
2,554 while syntax diagnostics fall from 13 to 6. The next unsupported suffix is
`noreturn` at that checkpoint; the continuation below advances that boundary.
The continuation records which conditional/ObjPas requirements advance and which
broader binding gaps remain. Browser consumers/products compile only. WORK.md owns
exact evidence, retained preparations and the return to full resolver/editor/parity
requirements.

## Non-returning routine intent

Appended `plfNoReturn` preserves the contextual `noreturn` suffix independently of
external, forward and runtime-intrinsic ownership. Its exact `LinkageSpan` and
original parameter/result types remain available through the existing immutable
syntax snapshot. Direct/bracket markers take no operand; malformed operands,
missing separators and unknown neighbors retain explicit uncertainty. Escaped
identifiers and ordinary variable names do not acquire effect intent.

The marker does not remove an implementation body, prove that it never returns,
perform unreachable-code analysis or validate compiler-specific exception and
eligibility rules. Existing enum ordinals, interfaces and record layouts stay
intact; affected consumers are rebuilt together. There is no MCP schema change.

The whole native Delphi-mode companion has an ordinary following routine and an
annotated terminating body. A dedicated child mode enters that body and exits
with code 42, with a clean heap. Its command-line comparison returns from a
separate helper before termination so no managed comparison temporary survives
the deliberate `Halt`. The normal analysis run inspects the unchanged source and
executes the following routine independently; source inspection grants no
termination authority. Browser consumers/products compile only.

At that non-returning checkpoint, original System `RunError` retains its effect
role, value parameter and physical
include owner. Ordered source binding now answers the former `Char` condition
inside `TObject`; there is no pending-site location to query after that answer.
System reaches 2,723 declarations with zero syntax diagnostics and five answers;
ObjPas reaches 209 declarations with four syntax diagnostics and one answer. The
ordinary indexed `TObject.Fr` query returns exactly `Free` and `FreeInstance`,
with exact original include navigation after provider retirement and a detached
`Free` insertion preview that preserves the caller. The warning directive in
objpash.inc remains an explicit compiler-context gap; these known members precede
that gap and do not establish complete RTL meaning.
WORK.md owns exact unchanged-source evidence, retained preparations and the next
warning-policy prerequisite, with full resolver/editor/parity/adoption still open.

## Ordered warning requests

`INyxPascalContextWarnings` adds optional, immutable per-warning requests without
changing the base compiler-context interface. `NyxPascalWarningNumber` validates
positive signed-32-bit message IDs; `NyxPascalWarningName` validates open ASCII
group names. Neither constructor asserts membership in a compiler's catalog.
`pwaOn`, `pwaOff`, `pwaError` and `pwaUnknown` are closed action choices, while
three-valued activity independently retains a conditional write's uncertainty.

```pascal
LContext := NyxPascalWithWarning(LContext,
  NyxPascalWarningName('SYMBOL_DEPRECATED'), pwaOff);
LContext := NyxPascalWarnings(LContext).WithWarning(
  NyxPascalWarningNumber(5066), pwaError);
```

Requests retain order, including repeated and overlapping numeric/named selectors.
There is no fabricated alias map or effective-level lookup. Capturing, equality,
source-mode copies and local PUSH/POP snapshots preserve the detached vector;
unknown-context invalidation clears it. Checked getters, the independent 2,048-row
quota, invalid selectors and alternate-producer failures refuse atomically. Records
hold values only; no record owns an interface or editor reference.

FPC translation accepts `WARN selector ON/OFF/ERROR` and adjacent `+`/`-` forms,
including numeric leading zeros and explanatory trailing text. Optional
`INyxPascalWarningDirectives` retains the exact physical include/occurrence/span
and pre-directive state beside each ordered request. Prefix views expose only
earlier receipts and retain the source owner after callers release the translation.
Inactive directives have no effect; uncertain branches retain uncertain activity.
Malformed or unsupported-family directives preserve explicit uncertainty.

These are authored settings requests, not predictions of when FPC applies messages.
The configured native compiler's PUSH saves its pending message chain, which can
differ from an already active setting. The unchanged compiler oracle deliberately
retains that behavior: three marked-symbol warnings normally, one promoted error
plus one warning in error mode, and four warnings with an unknown selector. Successful
runs return 84. Warning counts alone do not establish directive timing or validity;
WORK.md owns exact diagnostics and the remaining real-RTL source gaps.

## Authored code-alignment requests

`TNyxPascalCodeAlignmentRequest` fluently retains an ordered group of closed
`TNyxPascalCodeAlignmentSubject` assignments. The optional
`INyxPascalContextCodeAlignments` capability owns an immutable sequence of these
groups and their conditional activity. Capture, exact identity and local
PUSH/POP preserve it; source mode changes retain it and unknown textual effects
revoke it. Checked ordinals, 64 assignments per group and 2,048 groups per context
bound storage. Alternate-producer failures preserve the existing publication.

```pascal
LRequest := Default(TNyxPascalCodeAlignmentRequest)
  .Add(pcaConstantMinimum, 16)
  .Add(pcaConstantMaximum, 32);
LContext := NyxPascalWithCodeAlignment(LContext, LRequest);
```

The eleven selectors cover procedure/jump/loop labels and constant/variable/local/
record minima/maxima. Zero is an explicit request. Label selectors accept zero or
powers of two through 256; constraint selectors preserve signed-32 values. These
are FPC-authored code-generation requests, distinct from PACKRECORDS, measured
type sizes and effective target layout. FPC builds a temporary constraint packet
per directive, so grouping, duplicate selectors and assignment order stay exact.
Nyx does not merge requests into invented defaults, offsets or ABI conclusions.

Translation accepts the supported `CODEALIGN selector=signed-decimal,...` source
boundary only in an explicit FPC context. Alternate numeric notation, unknown
selectors, malformed values and overflow refuse visibly without partial groups.
Inactive branches add nothing; unknown activity remains explicit.
`INyxPascalCodeAlignmentDirectives` exposes each original include occurrence/span,
pre-directive state and typed rule. Prefixes expose only visited receipts, and
POP retains historical receipts while restoring current authored settings.
Native whole-unit execution and compiler refusals qualify syntax/ownership;
browser consumers compile only. Complete RTL meaning, full type/layout analysis,
editor adoption and both-target input/quality remain separate in WORK.md.

## Authored optimization requests

`TNyxPascalOptimizationRequest` separates symbolic ON/OFF/DEFAULT resets from
individual typed pass toggles. `pomLevelTwo` requests FPC's target-dependent
level-two set; `pomDisabled` requests an empty set; `pomInitial` requests the
compiler's initial settings. None expands into analyzer-guessed target defaults.
The 29 recognized `TNyxPascalOptimizationFeature` selectors retain individual
flags, including level flags, without claiming compiler/target availability or
effective generated instructions. Whole-program optimization remains separate.

```pascal
LContext := NyxPascalWithOptimization(LContext,
  TNyxPascalOptimizationRequest.Select(pomInitial));
LContext := NyxPascalOptimizations(LContext).WithOptimization(
  TNyxPascalOptimizationRequest.Toggle(pofFastMath, False));
```

Optional `INyxPascalContextOptimizations` owns an immutable ordered vector of
requests and independent conditional activity. Capture, exact context identity,
mode copies and local PUSH/POP preserve that vector. Unknown textual effects
revoke current knowledge; inactive source branches add nothing. The independent
2,048-row quota, checked ordinals and validated alternate getters refuse
atomically. No record owns an interface, producer, compiler, source or editor.
Reset/toggle accessors refuse the other variant; zero initialization cannot
silently request OFF.

FPC's source OPTIMIZATION scanner reads one ASCII selector, rather than the
command-line comma list. Translation retains that boundary: ON/OFF/DEFAULT or
a recognized feature, optionally prefixed by NO. Trailing text/commas stay in the
exact source receipt without applying another setting. Unknown selectors and
compiler-family mismatches remain visible refusals. Optional
`INyxPascalOptimizationDirectives` retains each physical include occurrence/span,
pre-directive state and authored rule; prefixes expose only visited requests.
Historical receipts survive POP, while current authored settings restore.

Pending application of optimizer flags remains compiler-owned. These contracts
preserve authored intent without predicting target support, precise activation
timing, layout or ABI. The whole native unit/include qualifies all pass spellings
and ordinary execution; browser consumers compile only. WORK.md owns unchanged
RTL evidence and the remaining full language/editor/parity requirements.

## Bounded compiler query cohorts

General compiler requirement plans stay minimal. Optional
`TNyxPascalCompilerPlanOptions.LookAheadConditions(True)` proposes missing compiler
symbols from already retained conditional directive rows. Required expanded queries
keep priority. Macro expansions and unresolved sites retain their existing owner;
proposals do not become branch, source-type or alias facts. Already known pristine
facts are not queried again to try to repair source-local uncertainty.

`INyxPascalCompilerCandidates` exposes bounded candidates, explicit truncation and
work accounting. Input rows, lookahead rows and expression scalars consume the
existing work quota; cancellation or exhaustion publishes no partial plan. The
256-symbol per-job quota and aggregate context/source limits remain unchanged.
Native module preparation enables lookahead by default; callers can supply a
minimal plan explicitly. The expanded options record requires affected callers to
be rebuilt together.

`PlanNyxPascalCompilerIncludeCandidates` optionally builds a separate immutable
proposal pool from a captured root and literal include links. It follows all
branches speculatively under one explicit lexical envelope, scans each reachable
physical file once, terminates cycles and excludes unrelated captured units.
Names retain exact first scanned file/span origins without fabricated source
occurrences or compiler states. Missing/dynamic include links and opaque byte
files remain visible statistics; no disk lookup, guessed decoding or macro
expansion occurs. Named sizeof/declared operands remain source requirements.

The existing `MaximumConditions` bounds unique pool names; truncation is explicit.
Lexical input/traversal rows and directive-value scalars charge the existing
planning work budget. Quotas and cancellation publish no partial pool. The pool
retains exact files/search order; immutable copies survive caller changes.
`PlanNyxPascalCompilerRequirementsWithIncludes` explicitly freezes pool names after
actual requirements and retained-row lookahead. An alternate producer grants no
source authority and is not retained. `CandidateCount` still describes retained-
row proposals; the separate pool exposes its own count/origins/statistics.

Native preparation enables this known-include pool when conditional lookahead is
enabled. `LookAheadKnownIncludes(False)` selects retained-row-only behavior; a
minimal plan also avoids speculative traversal. Preparation builds/charges a pool
once, then charges each round's bounded name enumeration against the aggregate
allowance. Required-first cohort selection retains every source requirement while
obeying the unchanged 256-symbol job ceiling. Later measured contexts can request
remaining names without repeating already captured pristine facts. No fixed SDK
list, extra initial primitive probes, raised work/job limit or source mutation
substitutes for the planning algorithm. WORK.md owns exact native RTL results and
remaining language/editor/parity requirements.

`ReadNyxFPCCompilerCapture` and its native host counterpart retain the complete
transcript gate plus a separate observed-default packet before explicit query
replies. Optional `INyxFPCCompilerCapture.Merge` freezes alternate producers,
requires the same defaults/compiler/mode/scope/options/features/policies, rejects
conflicting presence, exact integers or sizes, and preserves richer integer facts.
Observed defaults are a compatibility anchor: initial command-line observations
can differ from a source-mode guard and are never substituted for source facts.

Preparation queries each new cohort against the original base request and retains
only merged verified facts, rather than growing a single cumulative request list.
The native host owns and joins each compiler process before publishing a capture.
This metadata grants no application execution or admission authority. Portable
planner/protocol checks and real native preparation remain separate evidence;
browser consumers compile only. Full RTL grammar/type meaning, incremental/editor
adoption, interaction, parity and delivery remain under original authoring criterion
two, with no new MCP tool or schema.

## Short-switch annotations

For captured FPC contexts, the existing translation reader treats a completed
short switch as followed by either an immediate comma or comment text. For
example, `{$Q- no overflow checking }` applies the Boolean request and retains
the entire original annotation. `{$I-,Q- explanation, H-}` applies I/Q only;
the later comma is already part of its comment. Whitespace before a comma has
also ended the group: `{$I- ,H-}` leaves H unchanged.

An immediate comma commits another complete supported switch. A missing letter,
sign or unsupported mode-dependent alignment refuses the entire group before
any historical context publishes a partial prefix. Whitespace after that comma
does not invent another FPC switch. Case and repeated valid switches retain
authored order. This corrects the earlier synthetic whitespace-group assumption
against the installed compiler's scanner and independent compiled IFOPT values.
Other compiler families retain their previous strict group parser; FPC evidence
does not grant pas2js-scanner semantics.

Ordinary include propagation, inactive/uncertain branches, PUSH/POP restoration
and bounded detached prefixes share this interpretation. No source text is
rewritten, no compiler facts are inferred and no callback/editor adoption is
implied. Exact alternative delimiters and supplementary text remain lexical
input, without a broader claim about which delimiters each compiler accepts.
Native FPC observations are separated from the browser consumer, which compiles
the portable checks. WORK.md owns qualification, retained failures and the next
real RTL boundary; full grammar/type/incremental/editor/parity remains open.

## Diagnostic emission

`TNyxPascalCompilerPolicies.EmitDiagnostics` selects a closed
`TNyxPascalDiagnosticChannel` and three-valued request. Zero-initialized policies
retain unknown warnings, hints and notes. They use three scalar fields rather
than a shared mutable record array. Category emission is separate from ordered
per-message `WARN` requests and does not calculate effective severity, compiler
alias membership or queued diagnostic activation.

For example, a caller can retain its other policies while changing one category:

```pascal
LContext := NyxPascalWithCompilerPolicies(LContext,
  NyxPascalCompilerPolicies(LContext).EmitDiagnostics(pdcWarnings, ppkFalse));
```

Complete context capture and identity include these requests. Local PUSH/POP
restores them while preserving later definitions; source mode changes retain
verbosity while resetting language-specific policies. Unknown textual effects
revoke category knowledge. Missing compiler evidence remains unknown rather
than borrowing the executing host's defaults.

Captured FPC source accepts `WARNINGS`, `HINTS` and `NOTES` ON/OFF or sign
requests. Complete identifiers are checked, so OFF1 does not become OFF. Exact
trailing annotation stays in the physical source. Known inactive directives
have no effect; uncertain activity retains the authored Boolean separately from
unknown current category knowledge. Other compiler families do not silently
inherit FPC parsing. Malformed or unsupported source remains an analyzer
diagnostic even when the source compiler warning category is disabled.

`INyxPascalEmissionDirectives` exposes checked bounded receipts with original
file/occurrence/span, pre-directive state, category, authored value and activity.
Restoration changes current settings without deleting these historical values.
Prefixes freeze only earlier receipts, exclude the current directive, and retain
source ownership separately. Records own no interfaces, callbacks or editor state.

The independent native compiler fixture observes suppression and restoration at
real token barriers and across a source mode change. Its three deliberate visible
messages include one expected warning; they are separate from product warning
debt. It is compiled alone and never executed. Portable focused ownership/source/
refusal checks run natively; browser consumers compile only. WORK.md owns final
whole-RTL results, failures and remaining language/editor/parity requirements.

## Atomic primitive fulfillment

`nyx.pascal.fulfillment` consumes existing typed compiler requirements through a
preflighted immutable batch. `INyxPascalCompilerCaptures` retains ordered frozen
fact/default packets with checked access and exact-context lookup; conflicting
default anchors for identical facts refuse. No context is promoted between
pristine, source-local and compiler-root namespaces.

`PlanNyxPascalCompilerFulfillment` requires an explicit one-through-64 job limit.
It freezes files, module descriptors, requests and capture owners, verifies every
source/context association, and deduplicates exact repeated jobs. This performs
no compiler or filesystem operation. Empty request capabilities remain empty
instead of inferring missing facts from diagnostic text.

`ApplyReplies` validates every reply through the existing exact one-fact guards
against unchanged inputs before publishing anything. Compatible replies merge
into their original capture owners. Registered modules sharing an exact old
bootstrap packet receive its verified new packet together, preserving each unit's
mode and source bytes. Unrelated contexts remain unchanged. A pristine module
update retains pristine identity; source-local settings cannot become defaults.
Late invalid replies, stale owners, quotas and cancellation preserve the previous
complete result and independent inputs. Interfaces live in class-owned dynamic
vectors, never records or static arrays.

The native `FulfillNyxFPCCompilerRequirements` host preflights the whole batch and
its host-supported probe modes before creating fresh caller-owned scratch. It
delegates each bounded job to the existing joined single-request capture gate,
retains partial scratch on failure, and applies replies only after all validate.
An empty batch needs no configured compiler or scratch directory. This host runs
no generated program, edits no project/service/configuration, and grants no
application execution provenance. The caller owns background scheduling and
explicit source/world replay; this is not default Studio engine adoption.

The real-RTL qualification leaves the five initial primitive probes, symbol-job
ceiling and per-construction semantic limits unchanged. A separate explicit
eight-job fact campaign consumes successive binding-derived requests; it does not
seed a fixed list or infer target widths. Replaying larger source remains a
bounded full construction, not an incremental-performance claim. WORK.md owns
the actual results, retained failures and remaining type/editor/parity gates.

## Declaration-bound constant expressions

`nyx.pascal.values` supplies copied exact integer, Boolean and enum-ordinal values.
Integers use the existing exact decimal arithmetic through the signed/unsigned
64-bit domain, preserving values above 2^53 on both targets without floating-point
conversion. Unsigned intent survives small results; arithmetic wrapping, signed overflow and
uncertain mixed-sign coercion refuse. Enum ordinals remain distinct and require
explicit `Ord` before ordinary integer arithmetic. This is bounded constant
meaning, not full compiler overload, nominal-type or expression checking.

`PlanNyxPascalConstant` retains an immutable parsed expression independently of a
source world. Its `Evaluate` method binds names at one exact `INyxPascalBindingSite`;
nested constants use their own declaration sites, preventing later declarations
or a caller's shadowing names from donating values. Source aliases, distinct types,
inline subranges and supported enum values retain their owning lexical arena.
Primitive bounds require matching captured compiler-root/pristine size facts and
the supported FPC integer family; the analyzer's host types never supply them.

```pascal
LOptions := TNyxPascalConstantOptions.Defaults.UpToSteps(100000);
LPlan := PlanNyxPascalConstant('Answer', LOptions);
LReply := LPlan.Evaluate(LSourceSite, LOptions);

if LReply.Value.Kind = pvvInteger then
begin
  LExactText := LReply.Value.Number.ToText;
end;
```

These expression strings are an explicit Pascal-source parsing boundary. Public
values, options, operations and results use closed types. Exact integer radix
literals, unary signs, Boolean operations/comparisons, arithmetic `+`, `-`, `*`,
`div`, `mod`, qualified names and supported `Low`/`High`/`Ord`/`SizeOf`/`Declared`/
`Defined` forms feed that plan. Ordinary calls, runtime variables, mutable typed
constants, real/string/cast/width-sensitive bitwise meaning and unsupported layout
remain unknown. Checked getters refuse incompatible kinds; there is no application,
filesystem, network, script or UI invocation.

`Shifted64(TNyxPascalShiftDirection, ACount)` explicitly selects logical movement
inside the value's 64-bit two's-complement constant domain. The appended closed
operations `pvoShiftLeft64`/`pvoShiftRight64` use the same implementation. Exact
decimal steps discard high bits on a left shift and zero-fill a right shift;
the left value retains its signed/unsigned-64 intent, including small results.
The count contributes no signedness. Integer counts 0..63 admit; other counts,
Boolean values and unprojected enum ordinals return Unknown. This selects neither
a machine register width nor runtime integer promotion. The
[FPC logical-operator reference](https://www.freepascal.org/daily/doc/ref/refsu48.html)
identifies shifts as logical operations without overflow checking and leaves
negative/out-of-width counts undefined. Exploratory compiler outputs for those
undefined cases are retained observations, not portable value authority.

Source plans admit `shl`/`shr` at multiplicative precedence, left-associatively.
Ordinary Pascal also admits `<<`/`>>`; FPC's separate IF grammar does not gain
those aliases. Source evaluation requires a captured FPC family and nonzero
version. Another source compiler or missing authority refuses instead of borrowing
this analyzer build's operators. Declaration initializers retain their ordinary
Pascal dialect even when used by a source condition. Whole compiled object/record
companions qualify earlier local constants, repeated names in distinct owners,
values beyond 2^53 and logical shifts of negative values. This is a constant-fold
facility; integer and/or/xor/not, casts, narrower arithmetic promotions and runtime
width rules retain their existing separate prerequisites.

`TNyxPascalConstantOptions.InDialect` distinguishes ordinary Pascal from FPC's
separate conditional expression language. Parsed plans cannot change dialect at
evaluation, and declaration initializers always use ordinary Pascal rules. The
configured FPC oracle establishes that preprocessor `HIGH`/`ORD` are recognized
while `LOW` is not. Direct wide radix literals first use signed-64 `Val`, and
conditional `Ord` has a separate signed projection. The conditional reader retains
those distinctions and existing noncanonical-decimal refusal instead of treating
ordinary source arithmetic as compiler admission. Other compiler families cannot
acquire FPC conditional authority from this option.

Results own bounded exact declaration dependencies for navigation after the query,
plan and original caller retire. Cycles, unsupported types and missing modules/
primitive facts retain typed issues and the underlying related name. Evaluation
shares one recursion/work quota across declaration expansion, with independent
per-call binding-world quotas. Defaults retain 512 tokens/16 KiB per expression,
64 levels, 100000 value work units and at most 256 dependencies. Exhaustion and
cancellation publish no partial result; these budgets do not establish full
incremental or total CPU performance.

Ordinary ordered semantic translation now consumes supported FPC constant plans
through the same exact-prefix binding and sealed replay path. Existing supported
declaration/size conditions retain their earlier authority. Value work charges the
existing aggregate budget and missing facts never produce a guessed branch.
WORK.md owns the native whole-source/compiler oracle, compiled-only browser
consumer and unchanged real-RTL frontier. The next missing primitive packet,
class/property grammar and full source/type/incremental/editor/input/navigation/
quality/parity/adoption/delivery remain under original authoring criterion two.
This adds no MCP tool/schema or observing release.

## Typed missing compiler requirements

`nyx.pascal.requirements` separates a compiler evidence request from a diagnostic.
Optional `INyxPascalCompilerRequirements` is available on owned binding, constant
and ordered conditional results. It exposes bounded read-only requests; absent
capability means no request. Agents or hosts never dispatch from `RelatedName` or
English message text. Existing result interfaces and wire/tool inventory stay
unchanged.

An `INyxPascalCompilerRequirement` retains a typed primitive size/presence
reference, detached exact compiler context, context-owning source and observing
source. These sources can differ when an imported compiler-root primitive is
needed by a consumer. `ContextState = -1` identifies the owner's independent
bootstrap packet; other ordinals select exact retained source states. Virtual EOF
is a valid observing position and honestly has `OriginIndex = -1`. No physical
token or context authority is fabricated. Source leases remain immutable and point
outward, with no editor/query reference cycle.

Factories reject known facts, unsupported compiler identity, invalid source/state
ranges and context/owner mismatch. Alternate collection capture retains at most
64 complete requests in order, detaches producers and refuses atomically on an
invalid getter. Incomplete output metadata continues to return an inspection gap
without creating an invalid native job. Missing bound primitive facts also flow
through the older source `SizeOf` path.

The explicit native `FulfillNyxFPCCompilerRequirement` freezes the request and its
owning `INyxFPCCompilerCapture`, requiring exact prior facts before scratch is
created. It queries the selected bootstrap/pristine scope through the existing
configured compiler process/transcript gate. `MergeNyxFPCSizeCapture` verifies the
same observed default anchor, rejects incompatible/conflicting cohorts and adds
only the requested fact to the previous context. Verified absence is a valid reply;
it never invents a type. Source-local contexts cannot silently acquire pristine
or bootstrap provenance. The probe's output application is not executed.

```pascal
LRequests := CaptureNyxPascalRequirements(LConditionalSource);

if LRequests.RequirementCount > 0 then
begin
  LRequest := LRequests.RequirementAt(0);
  LReply := FulfillNyxFPCCompilerRequirement(LCompiler, LFreshScratch,
    LRequest, LOwnerCapture, TNyxPascalCompilerTraceOptions.Defaults);
  LModule := ResolveNyxPascalModuleRequirement(LFiles, LModule,
    LRequest, LOwnerCapture, LReply);
end;
```

Run this synchronous operation on an owning worker when UI responsiveness matters.
It is one explicit additional request job, separate from bounded symbol preparation;
it does not enlarge that operation's probe allowance. No compiler is required just
to launch Studio or inspect source. This source addition installs no compiler host,
MCP schema, default resolver or observing release.

`ResolveNyxPascalModuleRequirement` requires unchanged files/search order/bytes,
namespace/root and prior module context, plus an exact coherent single-fact delta.
It returns an independent descriptor. Compiler-root replies change the detached
bootstrap packet while preserving the unit's own mode/settings. Wrong owners,
changed defaults, stale source/context and unrelated reply changes refuse without
publishing a module. Original files, captures, descriptors and active projects
remain independently owned.

WORK.md owns portable/native fulfillment evidence and the unchanged real-RTL route:
the original probe list is retained, a binding-produced request advances the High
condition, and later declaration/scope failures remain explicit. Full grammar,
canonical types, module coherence, incremental budgets, ordinary editor/input/
navigation, quality, browser/LCL parity, adoption and delivery stay under original
authoring criterion two.

## Statement labels and initialized procedural variables

`nyx.pascal.syntax` retains statement-label declarations through optional
`INyxPascalStatementLabelSyntax`. Each immutable value has its lexical kind,
exact name spelling, module/routine scope, name/list spans and a checked
`DeclarationIndex` into the same syntax snapshot. Named and decimal labels are
distinct; decimal text keeps leading zeros without host numeric conversion.
Nested same-spelling labels retain independent owners. Record-variant labels
remain an entirely different existing contract.

The appended `psdkLabel` declaration role preserves the compiler's lexical
namespace: labels can shadow outer names and answer ordered `declared(...)`.
Ordinary binding returns their exact typed symbol and physical origin. Asking
for a label's value type or expression-member receiver refuses explicitly;
member completion cannot emit a label as a variable. Existing declaration-kind
ordinals and original result interfaces remain intact. This does not implement
GOTO target completion, statement reference binding or control-flow validation.

```pascal
if Supports(LSourceSyntax, INyxPascalStatementLabelSyntax, LStatementLabels) then
begin
  LTarget := LStatementLabels.StatementLabelAt(0);
  LTargetDeclaration := LSourceSyntax.Declaration(LTarget.DeclarationIndex);
end;
```

Ordinals are zero-based and checked. Retaining the optional interface retains
only its immutable syntax, with no editor or query cycle. Declaration and label
metadata share the existing node quota. Missing names/separators and unfinished
lists retain incomplete entries and scoped uncertainty, leaving subsequent bodies
and declarations available for recovery. Conditional entries retain their
uncertainty. `Complete` proves list delimiters only, never compiler keyword/range
admission, a target definition, a legal jump or runtime effects.

`TNyxPascalCompilerPolicies.RequestGoto` carries a typed module request.
`{$goto on/off}` participates in exact detached context identity and unknown-state
revocation. Local `PUSH`/`POP` preserves its current module value. Supported FPC
source modes retain the compiler's module-switch behavior; placement/eligibility
and other compilers remain compiler-admission questions. The analysis service
does not infer that a parsed label or GOTO is legal from this request alone.

Initialized procedural variables keep their type and outer initial value in
separate ranges, including `function(...): ResultType; stdcall = @Handler`.
Grouped parameter syntax owns its nested separators; calling conventions remain
part of the type. Existing sibling and type-alias delimiter guards stay active.
The ordinary declared-type reader consumes the complete convention without
receiving the initial value as a qualifier.

The maintained whole native unit/include executes named/numeric/nested labels,
initialized callbacks, a label-dependent conditional and module switch lifetime.
Its original source is separately analyzed with exact include navigation.
WORK.md owns the portable/refusal/regression and unchanged actual-RTL evidence.
Browser consumers compile only; complete grammar/canonical types, module and
incremental coherence, editor/input/navigation, quality, parity, adoption and
delivery remain under original authoring criterion two. This adds no MCP tool,
schema or installed resolver.

## Class-member declaration families

Class properties retain ordinary `psdkProperty` identity with explicit
`psdfClassMember` intent. Their result types flow through existing declared-type
binding and member queries for both class and record owners. Type access admits
class properties while retaining the existing refusal for instance-only members.
Accessor expressions remain authored syntax; this source boundary does not
evaluate a getter or prove accessor compatibility and assignment legality.

A `class var` section applies to every grouped and continued field declaration.
Visibility, another declaration section, a routine or a property ends that section.
Later instance fields/properties retain independent flags and owners. All entries
share the existing declaration quotas and immutable snapshot lifetime.

Supported FPC `generic class function`/`generic class procedure` prefixes retain
their generic signature and parameter scope. Prefixes are consumed once;
unsupported orders/duplicates retain explicit diagnostics. Ordinary class methods
and generic declarations keep their existing paths. A visible generic method
declaration does not provide an inferred specialization: unresolved generic
arguments remain explicit, and full call inference/constraints stay open.

Qualified name binding also follows imported receiver types/values. For example,
`TCounterSettings.State.Count` can resolve an imported class, its class property
and the returned record field. Local declarations retain priority; established
explicit unit namespaces keep their authority. When no such namespace applies,
ordinary import lookup retains use order and refuses an uncaptured potential
shadowing unit. This changes neither import authority nor compiler/SDK capture.
Calls, indexing and constructed receiver expressions still require further
inference rather than being treated as simple dotted names.

The maintained native companions execute class/record properties, all continued
class storage and actual generic-method specializations. The generic input unit
deliberately uses ObjFPC syntax as a compiler oracle; the product and test harness
remain Delphi dialect. Analysis reads the exact whole units/include, and ordinary
completion preserves property/result origins and stages an exact detached insertion.
Small controlled base captures qualify lookup/refusal cases separately from the
unchanged real-System/ObjPas consumer. They do not substitute for actual SDK meaning.

WORK.md owns final source/consumer evidence, retained failures and remaining
compilerproc/CODEALIGN boundaries. Full property/generic/expression semantics,
canonical types, modules, incremental work, editor/input/navigation, quality,
browser/LCL parity, adoption and delivery remain under original authoring
criterion two. Browser consumers compile only; this installs no default resolver,
MCP tool/schema or observing release.

## Routine header directive boundaries

The declaration reader retains supported FPC routine headers whose first directive
follows the parameter/result signature without a semicolon. A configured-compiler
pair confirms both spellings of the actual remaining `compilerproc` boundary;
the unchanged System input is never rewritten to make analysis pass.

Result types stop before a known calling/modifier clause. Escaped and qualified
type names, generic arguments and grouped parameter syntax keep their own source
roles. Adjacent plain modifiers retain independent overload/forward intent;
compiler-helper, RTL, non-returning and intrinsic roles continue through their
existing typed flags/metadata. Member `public` remains a visibility section.
Original result, header and linkage ranges map through the retained physical
occurrence; projection separators never replace authored source text.

The lookahead recognizes closed supported clauses rather than granting arbitrary
identifier text directive authority. Genuine missing final terminators, malformed
role operands and unfinished declarations remain diagnostics/incomplete syntax.
Existing node/depth/work/cancellation quotas and immutable ownership remain active.
This parses authored intent; it does not prove ABI compatibility, intrinsic opcode
meaning, general operand expressions or compiler-helper call eligibility.

The native companion executes ordinary routines using these calling boundaries,
a preserved forward declaration and an escaped contextual result type. It also
compiles the exact adjacent compiler-helper spelling without treating that helper
as an ordinary callable API. Whole-source analysis separately retains every
original unit/include scalar, declared result type and included directive origin.
Portable refusal/ownership cases and the unchanged RTL route remain separate
qualification consumers. WORK.md owns exact source/remote identity, native results,
browser compilation and remaining compiler-context requirements.

Complete grammar/canonical types, modules, incremental work, editor/input/navigation,
quality, browser/LCL parity, adoption and delivery remain under original authoring
criterion two. This adds no default resolver, MCP tool/schema or observing release.

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
admission. Concrete unconstrained generic substitution is described below;
general expression/call inference and overload selection remain explicit gaps.

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
only. There is no default-provider/Studio/MCP rollout here. General constraint/helper/
overload meaning, complete compiler/SDK/grammar capture, completion-provider adoption,
incremental dependencies/budgets and full editor/navigation/quality/parity/delivery
remain under original authoring/service ownership.

## Compiler scopes and intrinsic identity

`TNyxPascalImplicitScopePolicy` separates declaration-only symbolic names from
compiler-unit lookup. Low-level binding options retain the former default for
compatibility; owned completion options now select `pispCompilerUnits`. Callers
can choose through the typed `WithBinding`/`WithImplicitScopes` contracts. This
does not install a default language engine in the observing Studio release.

Compiler-unit lookup checks lexical declarations and explicit imports first,
then the implicit units appropriate to the captured family/mode. FPC Delphi,
DelphiUnicode and ObjFPC contexts consult captured `ObjPas` before `System`; FPC
and TP omit `ObjPas`. A unit does not import itself, and `System` does not import
`ObjPas`. Unsupported modes and unknown compiler context refuse. The pas2js
branch retains its System-only implicit chain; the installed upstream parser/
compiler source identifies that initialization, separately from browser execution.

FPC documents [automatic ObjPas loading](https://www.freepascal.org/daily/doc/rtl/objpas/index.html)
and [the separate System.Integer meaning](https://docs.freepascal.org/docs-html/current/rtl/system/integer.html).
The native compiler oracle independently compiles two whole owned units and
checks last-import, local/nested shadowing, System qualification and explicit
qualification of implicit ObjPas. Source binding reads those same whole imported
units; small synthetic System/ObjPas views isolate ordering without replacing or
claiming qualification of a real SDK.

Missing implicit modules return the existing typed dependency gap. Indexed
completion loads them through its ordinary captured catalog, preserving load/
module/token limits, cancellation and detached analysis receipts. Explicit
namespace lookup keeps its own authority; a missing unrelated implicit unit
cannot change an explicitly selected known System declaration. No host directory
search, invented export catalog, compiler execution or new wire tool is added.

`INyxPascalIntrinsicIdentity` optionally exposes a builtin type's checked
reference, named/System origin and immutable compiler context. Its getters
require a builtin; it does not compute a size or layout. Alias nodes retain their
actual declarations. A terminal System primitive uses the captured System unit's
context, independently of the consumer's or ObjPas alias's source mode. The
existing builtin category alone is insufficient for type identity: generic
environment comparisons also retain reference/origin/context. This avoids treating
unqualified Integer and System.Integer as interchangeable symbolic categories.
Full canonical type equivalence, compiler-injected declarations, assignment/
overload rules, and layouts remain required.

Whole actual ObjPas is separately retained in an unconfigured native inspection.
The current ObjFPC lexical-policy guard refuses without a known compiler family;
that inspection preserves the original source but produces no declarations. It
does not prove actual RTL Integer binding. The configured real RTL consumer still
has the six recorded size/declared conditions open. Next connect sound source-
scope semantic queries with the ordered conditional reader, preserving prefix
visibility, imports, source positions and unknown settings effects. Do not infer
`sizeof` or `declared` results from pristine measurements alone. Full compiler/
grammar/incremental/default-adoption/input/navigation/quality/parity/delivery
remains under the original authoring owners. Browser evidence here is compilation.

## Ordered semantic conditions

`nyx.pascal.conditions` connects source scopes to the conditional reader.
`NewNyxPascalSemanticTranslation` owns frozen files and initial context, then
resolves supported source expressions during one ordered traversal. Each query
uses only its detached pre-directive token prefix. It returns
an immutable `INyxPascalSemanticTranslation` with the final ordinary translation,
work/pass/answer counts and a bounded pending site, binding issue and related name.
This is source analysis, not compiler admission or application execution.

`TNyxPascalDeclarationRef` distinguishes unqualified declaration names from
compiler defines and named size operands. `AnalyzeNyxPascalCondition` optionally
uses `INyxPascalConditionSemantics` for named `declared()` and `sizeof()` queries.
Without semantic authority their meaning stays unknown. Qualified/escaped
declaration expressions and general size expressions remain unsupported.
The original planning interface stays intact; optional
`INyxPascalConditionDeclarations` exposes deduplicated declaration requests.
Malformed/unsupported expressions publish no speculative partial request list.

`NewNyxPascalTranslationPrefix` retains original physical files, lexical views,
macro expansions and state provenance while bounding tokens, occurrences and
state/occurrence exits at an exact source site. It adds no closing syntax or
authored EOF token. Prior diagnostics and unknown participation remain explicit;
the unseen suffix cannot provide declarations to the prefix. Ordinary syntax
recovery supplies an analysis projection, never an edited or compiler candidate
buffer. Repeated visits to an include retain separate occurrence identities.

Each query prepares its exact file, occurrence, physical span, token count,
expanded expression and compiler context. A detached prefix is checked against
that prepared authority before its answer can select the next branch. The live
include/branch/macro/lexical/settings state continues in its original call stack;
no earlier translation or unknown suffix is replayed. The first unanswered query
blocks later source answers. No translation retains the callback afterward, and
receipts retain no binding world or temporary scope authority. Records contain
no interfaces; owned class graphs point outward to immutable source/context data.

`INyxPascalLexicalPrefixCapture` is a revocable construction borrow. Its frozen
`INyxPascalLexicalPrefix` owns copied admitted rows and exact source, with no
invented EOF or authority over unscanned suffix tokens. It does not implement
`INyxPascalLexicalSnapshot`. Active readers' complete-lexical getters refuse in
the detached translation prefix; ordinary finished translations still expose
their completed occurrence views. A retained capture refuses after success,
callback failure or cancellation, while previously frozen values remain valid.
The portable optional `INyxPascalPrefixConditionalFacts` and
`INyxPascalTranslationWork` extend existing serialized construction callbacks;
neither callback can mutate the producer through a prefix.

Declaration presence follows real scope lookup, including missing import and
unsupported-scope refusals. Size lookup follows bound aliases/distinct types to
an intrinsic's checked reference and declaring context, accepting only its
explicit captured measurement with matching mode/H policy. Host `SizeOf`, raw
name-to-width tables, opaque record layouts and pristine alias substitutions
provide no answer. The supplied world and context must belong to the same
captured compiler/output configuration; this factory does not establish full
SDK/target coherence. General layouts, canonical typing and expressions stay open.

`TNyxPascalConditionalOptions` defaults to 64 logical refinement rounds and four million cumulative
work units: translation/prefix tokens and constant-evaluation steps.
Caller-selected full-view translation/syntax/binding limits still apply. Passes
retains Answers + 1 for refinement accounting, rather than counting physical
traversals. `INyxPascalSemanticWork` separately exposes actual newly translated
tokens, analyzed prefix tokens and constant steps, whose sum equals WorkTokens.
Exhaustion and cancellation publish no partial construction receipt.
This is bounded refinement; incremental/project-scale performance is unqualified.

An `AddToken` full-view refusal carries `ENyxPascalTokenLimit` with its admitted
token count and reached cap. Streaming aggregate refusal uses
`ENyxPascalConditionalWorkLimit`: the translation/prefix/constant phase, admitted
work, next request, aggregate cap, source tokens and logical round. Both retain
the original translation-limit category and own only scalar observations. The
earlier replay-specific `ENyxPascalConditionalTokenLimit` value shape remains
available for existing callers; the streaming constructor does not emit it.
No source truncation, ignored directive, fixed compiler probe or raised quota
substitutes for actual avoided translation work.

Owned and indexed completion use this constructor by default, through typed
`WithConditions` options. Every new source token/prefix consumes the ordinary aggregate
budget. Missing semantic imports use the captured dependency loader; active root
cycles refuse. Once all supplied roots exist, conditional exports are revisited
under the same limits. Another sweep requires fewer unresolved conditions, so
caller root ordering does not become a configuration prerequisite. Compilation
and editor history/source publication remain independent operations.

The exact whole handwritten native companion and selected-compiler measurements
qualify forward visibility, one physical include visited twice, source-local and
System aliases, nested shadowing and unchanged source/scalar positions. A further
whole FPC companion qualifies header-scoped module lexing, local restoration,
type replacement and separate scope at repeated includes. Small
explicit System/ObjPas scope models isolate the rules; they are not a replacement
SDK. The unchanged full RTL consumer passes under its original budgets and reaches
22 System answers, retaining a pending record-scoped constant in heap.inc.
The real SysUtils construction still reports its external scope prerequisite
and the full six size/declared decisions remain open. WORK.md owns exact current
native/semantic evidence and retained failures. Browser consumers compile only;
full RTL/grammar/incremental/default-adoption/input/navigation/quality/parity/
delivery remain required under the original authoring owners.

## Concrete generic binding and member pages

The binding kernel now carries immutable concrete arguments through supported
unconstrained generic classes/records, aliases, ancestors, fields/properties,
method results, nested ordinary aliases/records and callable signatures. A member
handle retains its receiver's substitutions across later `TypeOfSymbol` queries;
the original declaration and physical include origin stay unchanged. Different
specializations of the same declaration have independent frames. A fluent self
result can retain its arguments or explicitly select different ones.

Names other than placeholders still bind in the template's declaration scope,
following FPC's [generic scope contract](https://www.freepascal.org/docs-html/current/ref/refse59.html).
Generic aliases own a parameter scope even without a nested member scope. An
unbound placeholder cannot fall through to a same-named outer declaration.
`INyxPascalSubstitutedSymbol` optionally exposes the retained frame as
`INyxPascalGenericEnvironment`; its arguments are semantic types. Frames point
outward to raw declarations, earlier frames and existing arguments. The world
caches none of them, preserving reference-counted ownership without cycles.

Argument-count mismatch reports `pbiGenericArity`; unmodeled constraint headers
report `pbiGenericConstraint`. Constraints are never assumed satisfied. Generic
routine inference, overloaded generic names/arity selection and complete
compatibility/constraint checking remain open. The current native compiler
rejects a generic declaration inside another generic; this resolver explicitly
refuses that form. An ordinary nested type can still capture the outer argument.
The kernel is analysis, not a substitute for compiler admission.

`INyxPascalMemberQueries` is an optional world capability; it leaves the existing
exact-name interface intact. Queries return 20 rows by default, at most 50 and
no more than the world's candidate budget. Prefix matching uses Pascal ASCII
case folding. Ordering is folded name, then declaration order within a resolved
overload family. Offsets count visible rows; hidden fields and wrong receiver
categories do not consume them.

```pascal
if Supports(LWorld, INyxPascalMemberQueries, LQueries) then
begin
  LPage := LQueries.Members(LReceiver, LCaller,
    TNyxPascalMemberQuery.Defaults.Matching('With').Take(10));

  if LPage.ResultSet.Status = pbsBound then
  begin
    // Each row retains its exact declaration, physical origin and substitutions.
    // Use NextOffset only in this same immutable world when HasMore is true.
  end;
end;
```

The captured inheritance graph is checked before publishing a page. Missing or
ambiguous ancestor context produces no partial rows; a required module name stays
available for focused capture. `HasMore` means a further visible candidate was
checked, not a guessed total. Empty successful pages are distinct from unresolved
queries. Step/depth/candidate budgets, cancellation and exact-world handles apply
to enumeration as well as exact lookup. Inherited overload deduplication includes
concrete argument context rather than declaration identity alone.

WORK.md records 127 native generic/page checks plus 62 maintained binding checks,
all with clean heaps. The exact compiled generic fixture and a real
`TBindingBox<INyxBadge>` continuation execute natively; the badge result still
navigates into `nyx.controls.types.inc`. Both browser consumers compile only.
No default provider/editor/MCP integration or installed release changes here.
Complete helper/overload/expression/compiler/SDK/grammar meaning, incremental
dependencies, project-scale budgets and full ordinary-editor/navigation/parity/
quality/delivery remain under the original authoring/service requirements.

## Nyx-owned completion provider

`nyx.pascal.completion.owned` now connects the owned lexer/context/syntax/binding
layers to `INyxPascalCompletionProvider`. It captures explicit compiler facts,
virtual SDK files and module roots once. Each call overlays all exact current
project buffers, including independently edited units/includes, then creates its
own immutable semantic world. Nothing reads host files, starts a compiler or
executes an authored application. The host runs this synchronous adapter on its
existing worker/service boundary; no output selection is required by the API.

```pascal
LProvider := NewNyxOwnedPascalCompletionProvider(LFacts, LCapturedFiles,
  LSDKRoots, TNyxOwnedPascalCompletionOptions.Defaults.UpToTokens(1000000));
LReply := LProvider.Complete(LQuery);
```

Supported receiver queries cover lexical dotted names and declared property
chains, including concrete generic results. The caret/replacement stays in exact
physical scalar coordinates; the semantic caller uses the member dot's explicitly
mapped occurrence. An identifier tail after a middle caret is replaced whole.
Escaped keywords retain legal escaped insertion. Literal/comment/directive
interiors, unmodeled calls/indexing/constructed expressions and unresolved type/
helper context produce no insertion rows. This is a supported declared-receiver
provider, not the complete expression/language engine.

Project units keep their already admitted `.pas` leaf. `NyxPascalProjectUnit`
separately derives the typed namespace; includes require an explicit main/unit
owner. A repeated include in that owner refuses because the existing request has
no occurrence selector. Inactive/unknown participation cannot borrow another
context. Declared namespaces must match captured project membership; duplicate
module identities refuse. Drafts are inspected without acquiring compiler
admission or changing accepted comparison text.

Member pages opt into `CountAll` under the same semantic work budget, retaining
the completion contract's exact `Total` without extra response rows. The optional
`INyxPascalMemberTotal` reports -1 when the ordinary early-stop page has no exact
total. Aggregate translated-token, receiver-scan, per-module syntax/translation
and existing binding budgets all refuse atomically. The optional
`INyxCancellablePascalCompletionProvider` supports call-scoped cancellation.

`INyxOwnedPascalCompletionResult` exposes typed gaps, binding issues/related names,
and physical definition locations. Each ready row retains its immutable physical
lexical source through `DefinitionSource`, so a later provider replacement does
not reopen changed SDK bytes. Definition roots/files/occurrences confer no edit
permission. The existing result/wire factories still guard complete source
identity, middle-word replacement, 50-row/128 KiB limits and detached Preview.
These optional origin/gap capabilities are in-process; the existing wire schema
does not transport them. Its bounded message includes the related virtual name.

WORK.md records 86 native provider/menu checks and 81 production in-process
semantic/private-editor checks. Exact helper/Unicode/NUL buffers, include ownership,
generic property chains, paged totals, source-location lifetime, ordinary text
Undo/Redo, permissions, revision/provider retirement and concurrent observation
are qualified. The production engine is explicitly owned and never listens;
these are not HTTP authentication or ordinary rendered-input receipts.

The real captured `TNyxPascalCompilerVersion.FullNumber` record member resolves
through both production paths. Enumerating the real badge with the supplied
partial capture still requires `nyx.model`; that retained typed gap is not repaired
with fabricated exports. Complete compiler/SDK dependency capture, lazy/incremental
invalidation, full expressions/helpers/overloads/constraints, project-scale budgets,
default Studio adoption and complete input/navigation/quality/parity/delivery remain
required. Four browser consumers compile only. No default provider, new MCP tool/
schema, active-project mutation or installed release is implied by this factory.

## Captured SDK modules and dependency loading

`nyx.pascal.modules` supplies an immutable namespace-to-root catalog independent
of disk paths and compiler-project admission. A module descriptor inherits the
completion profile or owns explicitly captured per-unit compiler facts and
translation options. Catalog replacement retains earlier snapshots. Capture
requires actual frozen files, bounded unique namespaces and no implicit physical
root aliases; declared identity is checked again when a root is translated.

```pascal
LModules := NewNyxPascalModuleCatalog.WithModule(
  NewNyxPascalModuleSource(NyxPascalModule('example.widgets'),
    NyxPascalPhysicalFile('sdk/widgets.pp')));
LProvider := NewNyxIndexedPascalCompletionProvider(LFacts, LFiles, LModules,
  TNyxOwnedPascalCompletionOptions.Defaults.UpToDependencyLoads(128));
```

The indexed provider initially parses project roots. A typed missing-module
response loads only that explicit indexed root and restarts binding with fresh
world-bound handles. Earlier parsed roots and aggregate token/dependency counters
remain; the receiver expression is parsed once. No retry loads a previously
loaded/unindexed root or searches the filesystem. Reverse direct-import order,
qualification and alias/generic meaning remain the original binder's decisions.
Unused SDK metadata can therefore describe roots that are never parsed.

Absence requires more evidence than a missing entry in a partial AST. An imported
interface with unknown/unfinished participation cannot fall through to an earlier
unit or intrinsic. The binder checks a real unescaped implementation boundary,
root-scope placement and interface diagnostics before publishing export absence.
Known prefix declarations remain usable; this does not claim full grammar or
overload completeness in unfinished source.

Optional `INyxPascalCompletionAnalysis` retains detached loaded namespace/root
locations, translated-token accounting and successful dependency-load count.
These are in-process receipts, not new wire fields or execution provenance.
Existing source/revision/permission/preview/Undo and response limits stay owned
by their previous contracts. No default provider or MCP schema is installed.

The native-only `nyx.pascal.files.native` adapter reads explicitly selected
directories into frozen files/module metadata. It uses byte streams, explicit decoding,
Unicode Windows file paths and per-file read leases, without global codepage
changes. It scans bounded unit headers rather than guessing names from filenames;
program/unknown roots remain explicitly unindexed. The caller serializes SDK edits
across the multi-file capture. Recursion, depth, file/byte/enumeration/header work
are explicit; linked entries, identity changes and IO/budget/cancellation failures
refuse without mutating seed captures. Strict UTF-8 refuses undecodable input;
the optional opaque policy retains its original bytes without text. Native host
paths and read handles do not enter the returned catalog. Selecting other actual
SDK encodings and broader filesystem/platform qualification remain open. Header recognition is
metadata preparation under the current default lexer, not conditional-wrapper or
mode-aware declaration proof; loaded root identity remains independently checked.

WORK.md records 31 native dependency checks, 22 native SDK/file checks and 84
production in-process semantic/private-editor checks. The actual repository
capture has 493 files/381 modules. A badge query loads `nyx.controls` and
`nyx.model`, then names the missing `SysUtils` dependency. Adding 20 exact real
Windows RTL files supplies that root (513 files/386 modules). Capturing all 48
SysUtils include files with the explicit opaque policy gives 561 files; translation
passes ObjFPC/OUT/MACRO ON/INLINE ON and reaches later conditional/target-size gaps,
including sizeof(Integer) and its unresolved FATAL fallback in syshelph.inc.
The imported interface remains incomplete; full textual macros, other actual
encoding/default/target/define/grammar meaning remain prerequisites. No export
stub or source rewriting turns this qualified boundary into readiness.

The real `TNyxPascalCompilerVersion.FullNumber` record member resolves through the
indexed production semantic/private-editor paths, now using the configured
compiler's captured facts. Current protocol/profile checks pass 24/18, with 82
semantic checks and maintained provider/dependency/modes 86/31/41, clean native
heaps; two browser consumers compile only. Earlier binding/generic receipts remain
in WORK.md. Full RTL lexical/default/define/include capture, complete grammar and useful
expression meaning, dependency invalidation/caching, measured project-scale work,
default Studio adoption and complete input/navigation/quality/parity/delivery
remain required. This directory capture is source input preparation, not browser
rendering, HTTP execution, source admission or an observing release.

## Actual Windows namespace capture

`TNyxPascalDirectoryOptions.OnlyFile` selects one exact checked Pascal source
leaf inside the explicitly supplied native directory. It retains original bytes
and the ordinary header/module/encoding/read-lease contracts without reading or
indexing other files. Enumeration, payload and cancellation budgets still apply.
An absent leaf refuses atomically; recursive selection and directory paths refuse.
This prepares explicit inputs, not an implicit filesystem dependency search.

The configured Win32 SDK consumer captures the whole Windows root and wininc
directory, then selects its two actual shared RTL includes. Forty-seven explicit
compiler queries leave the future define set open. The unchanged 28-file capture
translates all 498,239 tokens across 23 include visits with zero translation
diagnostics. The initial syntax capture contained 7,652 declarations/110 grammar
diagnostics. The [anonymous continuation](#anonymous-structured-type-ownership) now retains
29,823 declarations/zero supported declaration diagnostics;
this does not establish complete namespace/type meaning.

The same capture feeds ordinary indexed completion with its explicit per-unit
context/options and a bounded whole-SDK declaration quota. The Windows root loads
once beside the consumer and returns its actual wYear suggestion/navigation.
Handwritten compiled input separately executes TSystemTime and anonymous INPUT_RECORD
member access. These observations do not prove complete namespace/type meaning:
the inspected WORD field type still requires uncaptured ObjPas. Full remaining
grammar, actual implicit System/ObjPas scope and SysUtils's six semantic
conditions remain original authoring prerequisites. WORK.md owns current receipts,
retained failures and the return path; browser evidence remains compilation only.

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
