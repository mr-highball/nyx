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
