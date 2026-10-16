# Executable Logic and typed graphs

[Logic owner](../TODO/NS-3_logic-components_01.md) ·
[Graph owner](../TODO/NS-1_canvas-flow_01.md) ·
[Tool Canvas](../TODO/NS-3_tool-canvas_01.md) · [Evidence](../WORK.md)

Logic components perform work without normal UI chrome. They expose specialized,
reference-counted interfaces and the same immutable operation contract used by
graph runs. A future Tool Canvas represents these operations with discoverable
cards and ports; drawing a connector alone never executes behavior.

The current standalone operations are:

| Interface/factory | Inputs → outputs | Meaning |
| --- | --- | --- |
| INyxJSONParse / NewNyxJSONParse | Text → JSON | Strict JSON admission, preserving exact number spellings, Unicode and NUL |
| INyxJSONSelect / NewNyxJSONSelect | JSON → JSON | Typed Field/Item selection, sharing the resource selector contract |
| INyxJSONFormat / NewNyxJSONFormat | JSON → Text | Compact or indented output with field order and exact numeric/text values retained |
| INyxTextJoin / NewNyxTextJoin | Text + Text → Text | Caller-defined separator; exact text without behavioral string interpretation |
| INyxJSONFilter / NewNyxJSONFilter | JSON array → JSON array | Typed path/scalar comparison, including ordinal text search and explicit null equality |
| INyxJSONSort / NewNyxJSONSort | JSON array → JSON array | Stable path ordering with an explicit Text, Boolean or exact decimal key family |
| INyxJSONMap / NewNyxJSONMap | JSON array → JSON array | Select a typed Field/Item path from every item, retaining structured results |
| INyxJSONSlice / NewNyxJSONSlice | JSON array → JSON array | Nonnegative skip/take pagination without changing source order |
| INyxUTF8Decode / NewNyxUTF8Decode | Bytes → Text | Explicit strict UTF-8 decoding; malformed input refuses |
| INyxHTTPCall / NewNyxHTTPCall | Trigger → Bytes, status, headers, error, success | Asynchronous host operation with explicit injected transport; toolbox/source and actual application contracts qualified with substituted transport |
| INyxUTF8Encode / NewNyxUTF8Encode | Text → Bytes | Exact owned UTF-8 bytes, independent of native machine codepage |
| INyxSaveFile / NewNyxSaveFile | Trigger + Bytes → outcome, name, count, written/delivered/cancelled/activation flags, error | UI-affine asynchronous operation with explicit runtime file host; native write and browser download request retain different meanings |
| INyxOpenFile / NewNyxOpenFile | Trigger → bytes, metadata, name, count, read/cancelled/activation flags, outcome, error | Explicit runtime host returns one complete bounded snapshot; decoding and resource conversion remain separate typed operations |
| INyxJSONSet / NewNyxJSONSet | JSON + replacement JSON → JSON | Typed path upsert, replace or insert; existing parents required |
| INyxJSONRemove / NewNyxJSONRemove | JSON → JSON | Typed path removal with explicit require/ignore-missing policy; arrays close the gap |
| INyxJSONMerge / NewNyxJSONMerge | JSON + JSON → JSON | Recursive object overlay or explicit JSON Merge Patch; arrays replace as complete values |
| INyxJSONGroup / NewNyxJSONGroup | JSON array → JSON array | Stable typed-key groups with original key/items/count |
| INyxJSONDistinct / NewNyxJSONDistinct | JSON array → JSON array | First complete row per equal typed path key |
| INyxJSONJoin / NewNyxJSONJoin | JSON array + JSON array → JSON array | Inner/left/right/full keyed joins with explicit partner presence |
| INyxJSONAggregate / NewNyxJSONAggregate | JSON array → JSON scalar | Exact decimal sum/average/minimum/maximum or row count, with closed rounding/empty policies |
| INyxFlowJoin / NewNyxFlowJoin | Optional alternatives of one exact family → selected value + source reference | Deterministic conditional branch joins with explicit ambiguity/empty policies |
| INyxRegexMatch / NewNyxRegexMatch | Text → captures JSON, matched, count | Shared Pascal scalar matching with typed flags, exact spans and bounded work |
| INyxRegexReplace / NewNyxRegexReplace | Text → Text, matched, count | Structural literal/capture parts with explicit unmatched-capture policy |
| INyxRegexExtract / NewNyxRegexExtract | Text → rows JSON, matched, count | Table-ready named/numbered captures with explicit null/skip/error policy |

Pure direct Pascal calls need no canvas, interpreter, document or scheduler:

```pascal
LProfile := NewNyxJSONSelect(NyxDataPath.Field('profile'))
  .Select(NewNyxJSONParse.Parse(LSourceText));
LPretty := NewNyxJSONFormat(njlIndented, 2).Format(LProfile);
```

The [portable regex contract](regex.md) defines the shared grammar, Unicode 17
simple folding/categories, scalar offsets, structural replacement parts, tunable
limits, cancellation and current unsupported features. Direct callers, source
constructors, the public Nyx editor and semantic step configuration use the same
specialized managed operations. This does not establish full regex/Unicode
standards conformance or install these kinds on the protected observing host.

JSON editing lives in `nyx.logic.json.editing`. Direct callers retain specialized
managed interfaces; saved graphs and generated Pascal use the same constructors:

```pascal
LRequest := NewNyxJSONMerge(njmpMergePatch).Merge(LDefaults, LOverrides);
LRequest := NewNyxJSONSet(NyxDataPath.Field('profile').Field('name'), njsReplace)
  .SetValue(LRequest, NyxData('Grace'));
LRequest := NewNyxJSONRemove(NyxDataPath.Field('debug'), njmIgnore)
  .Remove(LRequest);
```

`njsUpsert` adds/replaces an object member. `njsReplace` requires an existing
member, and `njsInsert` requires an absent member. For arrays, Insert shifts at
an index in `0..Count`; Upsert replaces an existing item or appends at `Count`;
Replace requires an existing item. Parents must exist and have the correct
container family. There is no implicit object creation, dotted path parser or
sparse-array filling. The root exists: Upsert/Replace can replace it, Insert
refuses, and removal refuses even with `njmIgnore`. Assign `NyxNull` explicitly
to replace a root with JSON null. Ignore-missing suppresses only absent members
or out-of-range items/ancestors, never a container-family mismatch.

`njmpOverlay` recursively overlays objects and preserves literal null values.
`njmpMergePatch` applies [RFC 7396](https://www.rfc-editor.org/rfc/rfc7396/):
an object patch starts from an empty object when the target has another family;
object null members remove matching members; non-object patches replace the
whole target. Arrays replace as complete values under both policies. Nyx also
retains existing object order and appends newly introduced keys in right-hand
order, preserving exact admitted number spellings, Unicode and NUL. No HTTP
request is implied by choosing Merge Patch.

Every result is an independent immutable value. Wrong paths, malformed input,
output budget violations and graph cancellation refuse the complete result;
neither input is modified. Shared JSON byte/depth/member limits still apply to
the combined output. The at-most-32-step path limit is shared with resource
selection. Runtime failures remain inspectable through the ordinary graph/route
report and never partially publish bound controls.

Studio's public Nyx Logic form exposes these choices as ordinary selects and
typed paths. Replacement values enter through a JSON port, so another operation
or a route constant supplies them without embedding runtime data in configuration.
Version-8 unfinished drafts retain the three policy fields. Earlier draft
versions keep their exact bytes, and forms that do not use/change the new fields
retain their existing version. Existing bounded `nyx_logic` discovery and grouped
`nyx_transaction` operations share the same typed factories and paired history.
These are current-source capabilities; the observing release remains separate.

`tools/test-logic-json-editing.ps1 -EvidenceDirectory <fresh-directory>` runs
portable algorithms/source/form/semantic checks, older draft/catalog consumers,
the compiled native request builder and ordinary native configuration controls.
It compiles matching browser consumers and a source worker. Optional
`-StaticJobsDirectory <preflighted-jobs> -BuildsBaseURL <existing-builds-url>`
executes desktop/CSS-390 HTTP consumers through the maintained Pascal browser
driver. It starts no server and writes only its unique owned static child.
Semantic contract tests are local; these do not establish an authenticated
installation of the newer catalog. Current tool inventories remain unchanged.

Collection operations live in `nyx.logic.collections` and reuse the ordinary
Nyx LINQ Where/OrderBy/Select/Skip/Take pipeline:

```pascal
LActivePeople := NewNyxJSONFilter(NyxDataPath.Field('active'),
  njcEqual, NyxData(True)).Filter(LPeople);
LOrderedPeople := NewNyxJSONSort(NyxDataPath.Field('priority'),
  njkDecimal, njdDescending).Sort(LActivePeople);
LProfiles := NewNyxJSONMap(NyxDataPath.Field('profile')).Map(
  NewNyxJSONSlice(0, 20).Slice(LOrderedPeople));
```

These operations capture immutable input/configuration and expose no partial
result on failure. Missing paths, wrong array/key families and malformed settings
refuse. Sort validates every key, including singleton input, and keeps original
order for equal keys. Text order follows Unicode scalars; contains/prefix/suffix
are ordinal and an empty search matches any text. Numeric comparison explicitly
uses Nyx's 96-bit decimal domain (scale 0..28), with exact conversion and refusal
on overflow/precision loss; untouched JSON values retain their original numeric
spellings. It does not silently convert arbitrary JSON numbers to Double.
Null equality is explicit; null is not empty text, false or numeric zero.

Invoke checks cancellation during capture, projection, filtering, ordering and
materialization. These are pure operations and can run under the ordinary native
worker or browser event-loop scheduler policy. This does not establish parallel
partitioning of a single sort, resource budgets or interrupting entered host work.
The public Logic workbench supplies typed path/comparison/key/direction/page
fields. Editor draft version 2 adds those fields; legacy version 1 keeps its exact
encoding and restores across catalog growth when its accepted graph context still
matches. Accepted graph changes continue to invalidate unfinished proposals.

Relational components live in `nyx.logic.relations` and reuse Nyx LINQ's typed
lookup, Distinct, joins, extrema and decimal accumulation. For example:

```pascal
LCustomers := NewNyxJSONDistinct(NyxDataPath.Field('id'), njkText)
  .Distinct(LCustomers);
LOrders := NewNyxJSONJoin(NyxDataPath.Field('customer'),
  NyxDataPath.Field('id'), njkText, njjInner).Join(LOrders, LCustomers);
LRegions := NewNyxJSONGroup(NyxDataPath.Field('right').Field('region'), njkText)
  .Group(LOrders);
LTotal := NewNyxJSONAggregate(NyxDataPath.Field('left').Field('amount'),
  njaSum, ndrExact).Aggregate(LOrders);
```

Keys have the existing Text/Boolean/exact Decimal family. Text equality is
ordinal and case-sensitive; decimal equality groups equivalent values such as
`1`, `1.0` and `1e0` without rounding their retained payloads. `njnReject` refuses
null keys. `njnMatch` explicitly gives null its own equality class, including
matching null across joins. Null never becomes empty text, NUL, false or zero.
Missing paths, wrong scalar families and decimals outside the exact 96-bit,
scale-0..28 domain refuse, even singleton inputs and either side of an empty join.
Keys are admitted/cached once per invocation before LINQ indexing. No query,
lookup, live collection or UI receiver is retained between invocations.

Group output is an ordered array of `{key, items, count}` objects. The key keeps
the first encountered spelling, groups follow first-key encounter order, and
items keep original row order. Distinct keeps the first whole row for each key.
Inner/left/full joins follow left order and right match order; full appends
unmatched right rows. Right joins mirror that behavior in right order. Duplicates
emit all matches. Join rows contain `left`, `right`, `hasLeft`, `hasRight`:
an absent partner has a null payload and a false presence flag, distinct from a
present literal null. Output bytes are counted while materializing; an oversized
Cartesian result refuses before allocating the complete fan-out. Ordinary shared
JSON byte/depth/member admission still validates the final result.

Aggregate Sum/Average use the existing wide decimal accumulator, with rounding
chosen explicitly through `TNyxDecimalRounding`; default `ndrExact` refuses digit
loss. A wider intermediate permits an average whose sum exceeds the final
decimal range. Minimum/Maximum keep the first tied input's numeric spelling.
Count requires the root path and counts whole rows. Empty sum/count are zero;
empty average/minimum/maximum use `njaeNull` or `njaeError`. Numeric operations
do not skip null or coerce text. Every component is pure; the ordinary scheduler
can run it on native workers or the browser event loop and publish complete
scalar/collection frames through the existing guarded UI courier.

The public Logic form has independent left/right path editing, null/join choices
and explicit aggregate/rounding/empty controls. Draft version 9 retains those
unfinished values and the separate right path; unused new controls preserve
earlier draft versions and bytes. Existing bounded semantic queries and grouped
transactions use these same constructors. Conditional execution uses the separate
typed branch contract described below.

## Joining conditional branches

`nyx.logic.joins` rejoins outputs whose producers may have been skipped by
Boolean step conditions. Each alternative is an exact application-defined port
reference; the recipe declares one payload family for every alternative:

```pascal
LJoin := NewNyxFlowJoin(
  NyxFlowJoinOptions(nftJSON)
    .Input(NyxFlowPort('primary'))
    .Input(NyxFlowPort('fallback'))
    .Select(nfjExactlyOne)
    .WhenEmpty(nfjeSkip));
```

The managed `INyxFlowJoin` exposes detached options and `TrySelect` for direct
callers. One to fourteen distinct inputs leave room for the closed `value` and
`source` outputs, addressed by `NyxFlowJoinPort(nfjpValue)` and
`NyxFlowJoinPort(nfjpSource)`. Those names are reserved as inputs. The selected
payload retains its exact family and value; source is the selected input's name.
False, zero, empty text/bytes, JSON null and a signal are present values. Absence
is represented by an omitted input, never by a fabricated default.

`nfjExactlyOne` refuses multiple available inputs. `nfjFirstAvailable` and
`nfjLastAvailable` use the recipe's declared order, independently of frame,
wire, step registration or completion order. All active dependency operations
complete before selection. A losing producer's failure or cancellation fails or
cancels the invocation; priority does not race work or provide error recovery.
`nfjeSkip` records a whole skipped step when no input is available. Its required
descendants skip and mapped state/collection outputs retain their prior values.
`nfjeError` reports an empty-input error. No successful prefix is published.
Direct `TrySelect` returns false only for empty-skip and clears both out values;
direct `Invoke` on that case refuses because there is no complete output frame.

Creator descriptors can declare `OptionalInput`. A present input still needs
its exact type and one producer; unwired or skipped-producer absence is allowed.
Creators read `Has` before `Value`. Optional `INyxConditionalLogic` supplies a
pure synchronous admission predicate after typed gathering, with the operation's
declared affinity. Returning false records skip provenance before Invoke or an
asynchronous host starts. Output ports remain required. Required consumers of a
skipped output retain the original propagation behavior.

Graph version 3 carries optional input markers and its conditions array.
Required-only graphs retain exact versions 1/2 and four-field port descriptors;
removing the final optional step restores the appropriate older encoding.
Markers are canonical Boolean true on inputs only; legacy versions cannot
silently admit optional meaning. Crafted source uses specialized factories,
typed families/references and named policies. Saved source admission and grouped
edits share these signatures.

The public Nyx Logic form edits payload family, priority and empty policies plus
an ordered alternative list. Add/up/down and confirmed removal change the local
recipe; graph wires still require complete grouped admission. Unfinished text
and even a temporarily empty list belong to draft version 10. Untouched controls
preserve earlier draft versions. Existing bounded semantic port queries expose
`optional: true` only for optional inputs; ordinary expected-revision transactions
edit the same operation configuration as one paired Undo command.
`tools/test-flow-joins.ps1` maintains direct/shared/source/semantic checks and
compiled ordinary application/configuration consumers for both targets.
Current-source capability is separate from the frozen observing LAN release.

`tools/test-logic-relations.ps1 -EvidenceDirectory <fresh-directory>` builds the
shared source/form/semantic consumer, exports its exact semantic-authored Pascal,
and executes the compiled native sales workbench. It also runs maintained JSON
and earlier authoring/draft checks, including ordinary relation configuration.
Optional preflighted static-directory/URL arguments execute matching HTTP browser
consumers at desktop/CSS-390. No service is started/replaced and no installed
project is edited. Local semantic fixture evidence does not imply installation
of these newer kinds on the observing host.

The current coverage ledger is deliberately separate from the full collection:

| Group | Executable coverage | Remaining requested coverage |
| --- | --- | --- |
| JSON | Parse, Field/Item selection, compact/indented format, typed insert/replace/upsert/removal, recursive overlay and Merge Patch | Schema validation; broader construction recipes and production budgets |
| Collections | Scalar-path filter/sort/map/page, grouped rows, first-by-key distinct, inner/left/right/full joins and exact decimal/row-count aggregates | Richer mapping/group-join/windowing recipes, production budgets and observing delivery |
| Text | Exact two-input join, strict UTF-8 decode/encode and shared typed regex match/replace/extract with a declared portable grammar | Broader regex/Unicode semantics, split, trim/case, formatting and remaining encoding operations |
| Files | Typed open/save forms/source/history; independent application/Studio host configuration; bounded byte snapshots/metadata; actual generated Win32/browser opening applications and scoped saves/downloads/picker writes; UI-authored conditional read routes | Broader filesystem operations, focused-view execution, other browsers/widgetsets, authenticated observing Studio and preserving delivery |
| Network | Typed HTTP/UTF-8 toolbox, crafted source, semantic editing and actual Table/application controls with substituted transport; native/Fetch adapters compile | Real network qualification, tunable retry/cache, authenticated current-schema observation and delivery |
| Data / Flow | Saved typed row routes, Boolean step conditions, typed optional-input branch joins and skip provenance, public Nyx forms, crafted source/paired history, bounded semantic queries/edits and selected state/collection publication | General route/input creation and richer selection UX; conversion/mapping/validation/resource lookup; timer/debounce, racing/recovery/feedback and observing delivery |

`tools/build.ps1 -Target logic-collections -LogicCollectionsEvidenceDirectory
<fresh-directory>` runs shared/native/source/history and authenticated semantic
composition, then stages matching browser input consumers and the source worker.
Actual HTTP execution remains a separate step. Current-source tool count stays
24; the installed 23-tool release does not gain these operations without delivery.

Every operation publishes a copied descriptor: distinct kind reference, English
title/help, intent group, creator labels, affinity and exact input/output ports.
The immutable INyxLogicCatalog captures descriptors/default configuration without
retaining executable creators. WithOperation returns an independent catalog;
creator help, labels and intent participate in Unicode AND-term search.
Creators implement INyxLogicOperation on their own reference-counted base, with
immutable configuration and schema.

The twenty-four-kind default collection includes open and save intent in **Files**.
Opening has its own managed interface, portable options and closed port enum:

```pascal
LOpen := NewNyxOpenFile(
  NyxFileOpenOptions.Title('Choose a report')
    .Allow('json')
    .MaximumBytes(4096),
  LReadHost);
```

`NewNyxBrowserFileOpenHost` and `NewNyxLCLFileOpenHost` are target adapters.
`NewNyxOpenFileLogicResolver` injects that explicit authority when a saved graph
is built, delegating other operations to an optional next resolver. Stored graph
configuration, generated companions and toolbox discovery remain inert.

`INyxFileSnapshot` owns detached bytes and copied `TNyxFileMetadata`: portable
leaf name, complete byte count, optional media type and optional integral Unix
modification milliseconds. Unknown metadata stays null. Browser media/time are
advisory File observations; native Win32 retains the opened handle's modification
time and leaves MIME unknown. No result exposes a path, OS handle or permission.
`AsResource` explicitly uses the existing resource kind's validation and packing
budget. Selection does not silently decode bytes or add a project resource.

The byte budget defaults to 1 MiB, accepts zero for empty files and is capped at
4 MiB; graph value/run budgets apply independently. Extension hints guide a
selection without prohibiting another user choice. Native selection stays on UI,
then worker reads use bounded chunks and a pure result channel. Retired requests
never keep widgets or their receiver alive through a worker. Browser selection
requires a current trusted user action and reads raw bytes through FileReader.
Run cancellation revokes delivery; user cancellation remains a separate usable
output. A Read outcome alone denotes a completed snapshot, including empty files.

`NewNyxFileOpenEditor` is a public Nyx compound used by the Logic workbench. It
captures a typed title, extension hints and integer budget. Used/changed fields
have an optional version-6 Logic draft child. Unused pristine controls preserve
older draft encodings, and refused captures keep unfinished text exactly. The
existing bounded `nyx_logic`/transaction/history operations expose this built-in
in current source; the observing server has not acquired this catalog/schema.

`tools/test-file-open.ps1 -EvidenceDirectory <fresh directory>` runs snapshot/
lifecycle guards, source/form/paired-history/local semantic checks, regression
consumers and real native reads through the exact generated companion. Optional
preflighted `-StaticJobsDirectory`/`-BuildsBaseURL` arguments also execute contracts
and compiled browser reads at desktop/CSS-390. The Pascal input driver chooses
actual disk files via the browser file input; it does not qualify physical input
or OS chooser appearance. It starts no server and changes no active design.

The original read journey connects Open → strict UTF-8 decoding and formats JSON
in its handwritten receiver. The conditional continuation below now runs Open →
Decode → Parse → Format through an ordinary saved event route and Nyx's standard
application publisher. Other browsers/widgetsets and observing delivery remain
separate. An empty file remains a valid raw Read; parsing empty JSON fails without
publishing the successful read prefix.

Both application adapters accept an explicit runtime host before mounting:

```pascal
LApplication.ConfigureLogic(LCreatorResolver)
  .ConfigureFileOpening(LFileOpenHost)
  .ConfigureFileSaving(LFileSaveHost);
```

Opening/saving compose independently with the creator resolver regardless of
configuration order. A nil host removes only its own configured layer; it does
not erase a separately supplied resolver. Post-mount authority changes refuse.
Retained hosts must not retain the application/document; pending routes retire
before those hosts and their widgets. Browser activation remains a host outcome,
including ActivationRequired for delayed routes without a current user action.
This contract does not fabricate a gesture or add an automatic opening prompt.

Studio's optional Outputs panel exposes both policies independently of selecting
a target or installing compilers. These machine-local choices affect future
generated launchers; they do not enter portable design/source/history. Private
profile version 3 requires Boolean opening/saving values. Disabling opening
preserves exact legacy default/save-only encodings. The current frozen observing
server does not yet admit this new profile version.
Native's explicit Apply/Reload button routes remain a recorded Studio authoring
gap; build-time reconciliation is present. These checks establish checkbox edits
and generated execution, not the complete connected configuration lifecycle.

`tools/test-file-open-runtime.ps1 -EvidenceDirectory <fresh directory>` checks
configuration composition, strict profiles, ordinary native Studio controls,
six real application compiler jobs and actual generated native executable reads.
Its optional independently preflighted static arguments also execute ordinary
Studio, actual generated browser applications and the read regression at desktop/
CSS-390. Opening disabled, opening alone and both hosts are distinct cases.
Substituted-host configuration checks remain separate from the actual file input/
Win32 picker consumers. WORK.md records receipts and qualification limits.

Boolean step conditions derive independent immutable graphs:

```pascal
LReadGraph := NyxFlowConditions(LBaseGraph)
  .When(NyxFlowStep('decode'),
    NyxFlowEndpoint(NyxFlowStep('choose'), NyxOpenFilePort(nofpRead)))
  .Build;
```

`When` selects a step when an exact Boolean output equals its Boolean argument
(True by default). False selects the opposite choice. It deliberately replaces
that step's previous condition. `Always` removes it. Data and condition
dependencies share deterministic topological ordering and cycle admission.
If a condition's producer is skipped, its consumer skips for either comparison;
absence never turns into a fabricated False. Any data consumer of a skipped step
also skips. Required invocation values and their budgets remain admitted before
execution, including values for potentially inactive branches.

Only selected steps invoke their implementations and contribute output values.
The result becomes available after every selected step succeeds; selected errors
and run cancellation expose no successful prefix. `NyxFlowOutputSkipped` validates
the exact declared output before reporting absence. Direct Value reads still
refuse missing values. The optional capability leaves original graph/result
interfaces intact; alternative graph implementations may provide their own
condition derivation. `NyxFlowSignatureGraph` derives inert definitions for source
inspection; it supplies no executable creator or runtime host.

Ordinary application publication retains destinations whose source was explicitly
skipped. Such row destinations are neither reserved nor replaced, even if the
user edited them while the graph ran. Selected state/rows still obey complete
type, revision, generation and atomic-frame checks. A misspelled source cannot
masquerade as a skipped branch. No fake JSON null, blank text or empty collection
is invented. An all-skipped destination group creates no data revision.

Only nonempty conditions use graph wire version 2. Clearing the last restores
exact version-1 bytes. Crafted source uses typed NyxFlowConditions/When/Build;
Boolean strings refuse. `nyx_logic` scope `conditions` pages a single exact graph
at expectedRevision. `flow-condition-set` takes step, source and Boolean `equals`;
`flow-condition-clear` takes step. The optional `expected` baseline stays separate
from the Boolean comparison. Grouped edits validate only the complete candidate,
so an intermediate condition cycle may be repaired within one transaction.
Step removal refuses connected conditions unless detachWires explicitly covers
both data and condition dependencies. These source schemas are not installed.

`tools/test-flow-conditions.ps1 -EvidenceDirectory <fresh directory>` qualifies
shared source/history/local semantic contracts, selected/skipped grouped rows and
actual generated native file routes. Its optional preflighted static-host arguments
execute the matched browser consumers at desktop/CSS-390. Native picker cancel
and page retirement preserve the old report; browser input uses actual disk files
through the file input, without qualifying OS chooser appearance or physical
device input. This adds no listener or deployment. Richer connection editing,
explicit joins/merge, feedback, broader operation coverage and observing delivery
remain required; conditional skipping does not establish those outcomes.

Studio's **Run conditions** section consumes the reusable public
`NewNyxFlowConditionEditor`. Choose a saved step, load its current condition,
select a declared Boolean output and check **Run when this output is True** for
the True branch, or uncheck it for False. The saved-condition caption describes
accepted meaning independently of unfinished choices. Display labels map to
exact typed references, including creator names with Unicode or slashes; the
editor never reconstructs references by splitting a caption. Step cards also
describe their accepted condition.

**Apply condition** captures a complete validated candidate and exact graph
baseline for the ordinary paired source command. It creates no runtime operation
or file authority. **Clear condition** requires explicit confirmation because
removing a dependency can permit additional work to run. Both operations use
the same source/design history as other edits. Pending Pascal still requires
explicit resolution before a design edit can publish.

`TNyxFlowConditionEditorDraft` owns unfinished field data and its exact inert
definition; restore refuses a different graph before writing any fields. The
parent Logic draft uses optional version 7 only after these controls are used;
pristine forms retain the previous encoding and versions 1–6 still admit.
Output/compiler selection remains optional. A graphless form renders disabled
condition actions instead of making a compiler a prerequisite for designing.

`tools/test-flow-condition-editor.ps1 -EvidenceDirectory <fresh directory>`
maintains shared form/draft checks, actual ordinary native Studio input/history
and an exact UI-authored compiled report application. With independently
preflighted static-host arguments it also runs the ordinary browser Studio,
source worker and compiled application at desktop/CSS-390. These are owned input
fixtures, not an installed observing release or physical-device qualification.

Portable save
options contain a suggested leaf, title, mode, media reference, extension filters,
Boolean all-types choice, byte budget and download-activation choice. They contain
no directory, native handle, browser file capability or host implementation:

```pascal
LOptions := NyxFileSaveOptions('report.json')
  .MediaType(NyxMediaType(nfmtJSON))
  .Allow('json')
  .Mode(nfsmPreferPicker);
LSave := NewNyxSaveFile(LOptions, LFileHost);
```

`NewNyxBrowserFileSaveHost` and `NewNyxLCLFileSaveHost` belong to target adapters.
Callers inject their runtime host directly or through `NewNyxFileLogicResolver`;
the optional next resolver composes with HTTP/creator authority. Importing,
cloning, generating or inspecting a graph never supplies that authority.
Ordinary browser/LCL application hosts also expose the same typed
`ConfigureFileSaving(INyxFileSaveHost)` method before mounting:

```pascal
LApplication.ConfigureFileSaving(LFiles).ConfigureLogic(LCreatorResolver);
```

The application retains this explicit host independently of its creator resolver.
Either configuration order works; the file layer handles saved file-save steps
and delegates other operations to the creator. Nil disables this layer, without
revoking authority the creator separately supplies itself. Mounted applications
refuse host changes. Route/view retirement cancels pending requests before
controls or the application are released. A custom host must not retain its
application/document, and borrowed platform owners must outlive that host.

Studio's Outputs workspace offers **Allow file saving in built applications**.
This is a machine-local opt-in for generated application and focused-view
launchers, independent of target selection. Enabled launchers explicitly compose
the target byte-save host with the default fresh-action wrapper; document data
never enables it. Disabled profiles retain their exact legacy version-1 encoding;
the optional version-2 profile requires a real Boolean `fileSaving`. The portable
design, companion and paired history stay unchanged, and old observing servers
are not implicitly upgraded by this setting.

`tools/test-file-runtime.ps1 -EvidenceDirectory <fresh directory>` runs the
profile/source guards, two real compiler jobs, actual native application saves
and Studio checkbox input. The optional preflighted static host arguments also
execute the generated browser application through its real OS picker and exercise
Studio at desktop/CSS-390. The current evidence separates generated-native
launcher compilation from the actual native application-host consumer; native
generated-launch execution, authenticated profile-save/launch observation and
preserving delivery remain open. No service is started by this qualification.

`NewNyxFileSaveEditor` is a public Nyx compound used by the Logic workbench.
It captures complete typed options or preserves invalid unfinished fields.
An optional version-5 Logic draft owns used/changed save fields; unused pristine
save controls leave version-1/2/3/4 encodings intact.

`Written` requires successful host write and close; it is not a crash-durability
guarantee. `DownloadRequested` reports browser dispatch, independently of eventual
disk completion. Both account for the complete admitted byte payload. User cancel,
activation required, unavailable and failed are distinct usable output frames;
explicit run/scope cancellation silently revokes publication. Invalid foreign
receipts fail the invocation. Result diagnostics are capped at 8192 UTF-8 bytes.
The direct host budget is at most 4 MiB, defaults to 1 MiB and includes zero;
ordinary graph value/run budgets still apply independently.

Browser PreferPicker falls back to download only when the picker API is absent.
Cancellation or picker failure never triggers a second delivery method. Picker
availability and user activation remain browser capabilities. Short connected
UI-queue graphs execute under a trusted click in the maintained browser consumer;
long computation may outlive activation. Direct callers can prepare bytes first
and expose a fresh Save action when ActivationRequired is returned. The optional
activation host below supplies that presentation. Uncommitted picker streams
are aborted when possible, and late promises retain only their request cleanup
lease after cancellation. An external effect already committing is not rolled
back by graph cancellation.

The native writer exclusively stages a sibling file before replacing the selected
destination. Failure/cancel removes only its own uncommitted stage. Actual Win32
qualification covers a real dialog, exact UTF-8 disk bytes, user cancel and scope
retirement while the modal picker is open. Win32 captures/tags that request's
dialog window through a UI-thread hook; LCL's opaque handle is never treated as an
HWND. Other native widgetsets remain separately unqualified.

`tools/test-logic-files.ps1 -EvidenceDirectory <fresh-directory>` runs native
contracts, source/history/local semantic checks, the exact compiled constructor
companion and actual Win32 controls. It stages browser counterparts and a Pascal
input/download consumer. Supplying an already preflighted `-StaticJobsDirectory`
and its `-BuildsBaseURL` also executes desktop/CSS-390 HTTP browser checks. The
driver uses trusted input and observes one private Chromium download through the
[Browser protocol](https://chromedevtools.github.io/devtools-protocol/tot/Browser/#method-setDownloadBehavior),
then checks the actual downloaded bytes separately. It never starts a server or
changes a Studio project. The generated companion checks construction/source
meaning; actual control/host execution is a separate maintained consumer.

Add `-BrowserPicker` for the separate real browser picker journey. This runs a
fresh full Edge profile against the same HTTP fixture, following a trusted Nyx
button action into the actual Windows save picker. The Pascal driver addresses
only the held browser's current child process tree, requires a visible/enabled
dialog, and selects a new leaf in its private output folder. It independently
reads the saved UTF-8 bytes, cancels another picker, then retires a graph while a
third picker is open. A successful late selection must write no payload or graph
frame; the browser may create an empty placeholder as part of path selection.
After dismissal the driver also checks that no download fallback appeared.
PreferPicker is deliberately retained for the cancel
check. Desktop/CSS-390 execution qualifies this browser/OS combination; it does
not establish physical phone input, OS dialog visual quality or other browsers.
Headless remains the default driver mode. No browser API, user activation,
certificate policy or existing browser profile is substituted. Picker availability
still follows the [File System Access draft](https://wicg.github.io/file-system-access/#api-showsavefilepicker)
and the browser's actual capabilities. This is a Community Group draft, not a
W3C Standard. Application/Studio runtime host configuration and preserving
delivery remain separate outcomes.

Long-running computation can opt into `nyx.files.save.activation` without
changing saved graph data or rerunning the computation:

```pascal
LFiles := NewNyxBrowserActivatedFileSaveHost(
  NewNyxBrowserFileSaveHost, nsapWhenRequired);
LResolver := NewNyxFileLogicResolver(LFiles);
```

The corresponding native adapter is `NewNyxLCLActivatedFileSaveHost`, with a
borrowed owner that must outlive its host and pending requests. Shared application
code still uses `INyxFileSaveHost`. Default `nsapWhenRequired` passes through to
the underlying host; only ActivationRequired offers a fresh Nyx Save action.
`nsapAlwaysPrompt` lets callers request that presentation every time. Save retries
inside a sequential Nyx confirmation callback, retaining the same detached bytes
and pending graph. Other outcomes keep their original meanings; cancelling a real
picker never triggers a download. This does not bypass browser activation or file
permission rules. No decorator is inferred automatically from serialized data.

`NewNyxActivatedFileSaveHost` accepts a creator's `INyxSaveActivationPrompts`.
`CreatePrompt` supplies a fresh independent confirmation; `PromptOptions` controls
its typed title, geometry and initial focus on each open. The reusable
`NewNyxSaveActivationContent` recipe exposes ordinary named parts for customization.
Already-open/reused presenters refuse before the wrapper subscribes to or closes
them. Factories must not retain their owning decorator/request in a cycle.

While a prompt waits, optional scheduler delay/turn capabilities observe scope
retirement. Unsupported/failed initial admission resolves a failed receipt before
UI opens; a failed later observation closes the prompt and resolves usable failure
while retaining the scheduler diagnostic. Shutdown or explicit cancellation is
silent. Request-owned weak callbacks/subscriptions are revoked before target
cleanup. Two pending requests have independent bytes, choices and lifetimes.

`tools/test-logic-files.ps1 -Activation -EvidenceDirectory <fresh-directory>`
qualifies this optional host. With an existing preflighted HTTP/static host, the
browser consumer captures input, waits 6.5 seconds before graph submission, checks
that the original activation has actually expired, then uses trusted Save now,
Escape and scope retirement. It verifies one exact download even when the memo
changes during preparation. This is HTTP browser desktop/CSS-390 input, not a
physical phone, real browser picker UI or installation in the observing Studio.

Text, Boolean, Integer, exact decimal Number, structured JSON, Bytes and Signal
have separate value families. Port and step references compare exact Unicode
names independently; punctuation has no delimiter meaning. Signals are invocation
tokens, never fabricated from Boolean/null data. Inputs have one producer per
invocation; outputs may fan out. No implicit JSON/text or numeric conversions occur.

NewNyxFlowGraph builds a reusable immutable DAG:

```pascal
LGraph := NewNyxFlowGraph
  .WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
  .WithStep(NyxFlowStep('format'), NewNyxJSONFormat)
  .Connect(
    NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpJSON)),
    NyxFlowEndpoint(NyxFlowStep('format'), NyxLogicPort(nlpJSON)))
  .Build;
```

Build captures independent vectors, configuration and complete port descriptors.
It validates topology; cycles currently refuse and require a future explicit
feedback runner. Each step executes once in deterministic topological/declaration
order. Each run supplies all unwired required inputs. Unknown, mistyped, duplicate
or wired invocation inputs refuse before operations enter. Alternative operations
must return exactly their declared output frame. Failure identifies the step and
exposes no partially successful result. Previously completed results stay independent.

Graph.ToData and NyxFlowGraphFromData form a versioned standalone persistence
boundary. Built-in recovery invokes only the typed factories above. Custom kinds
require an explicit INyxLogicResolver; kind, configuration and complete descriptor
must agree, so recovery cannot silently reinterpret a saved port signature.

Documents own named immutable declarations through `Document.Flows`. A declaration
contains copied schema/configuration and connections, without retaining executable
operations, a resolver or host authority. `Definition(...).Build` explicitly creates
a runtime graph; unknown creator kinds require the caller's resolver. Clones and
focused page/reusable builds retain independent registry membership. A retained
definition can outlive its document.

Admitted definitions keep private typed read snapshots of step identities,
descriptors and configuration. Reading one step no longer reparses the complete
serialized step vector. Returned metadata is detached: `TNyxLogicDescriptor.Copy`
copies every port record and label vector on both compilers, and configuration
values retain their independent-copy contract. Invalid indices retain the data
boundary's exception family. Foreign definitions still pass complete inert graph,
port and condition validation before registry publication; cached metadata grants
no resolver, file or executable authority. `Build` remains a separate boundary.

`tools/test-flow-metadata.ps1 -EvidenceDirectory <fresh-directory>` runs the
portable metadata/admission regressions and actual LCL label comparisons against
an independent fresh widgetset oracle. Supply an independently preflighted
`-StaticJobsDirectory` and matching `-BuildsBaseURL` to execute the metadata checks
over HTTP at desktop and CSS-390 widths. The script creates a unique static child;
it starts no service and modifies no active project. Ordinary paired edits remain
covered by `tools/test-flow-condition-editor.ps1`.

The matched ordinary Studio journey in WORK.md improves on native and browser
targets, with pass-local native glyph reuse and admitted metadata snapshots.
These observations are not a latency budget or a claim of responsive authoring.
For comparable native profiling, compile
`tests/nyx_flow_condition_studio_controls.lpr` with `-O2 -Sa -Cr -Co -Ci -gl
-dNYX_STUDIO_PROFILE`, identical toolchain/target paths and a fresh owned project
per run. Keep `-gh` allocation/retirement runs separate from elapsed comparisons;
their tracing changes the workload cost. Browser driver elapsed time includes
startup and the complete journey. Physical DPI/input and other widgetsets remain
separate from the native oracle's explicit per-font DPI comparisons.

```pascal
LDocument.Flows.Define(NyxFlowRef('Format profile'), LGraph);
LRuntimeGraph := LDocument.Flows.Definition(NyxFlowRef('Format profile')).Build;
```

Design version 14 saves these declarations alongside preceding typed families.
Documents without graphs retain their earlier minimal encoding. Older opaque
`flows` fields stay opaque; conflicting promotion refuses before publication.
The registry admits at most 64 definitions, each at most 1 MiB of UTF-8 wire data.

Generated Pascal uses typed factories, enums, paths and fluent graph construction.
Creator-only configuration is an explicit data boundary; its generated declaration
cannot execute until resolved. Literal admission reconstructs declarations without
invoking operations. Targeted handwritten continuation changes only named graphs
whose configuration changed, preserving unrelated helpers/computed definitions;
unexpressed registry reordering refuses whole-candidate equality. Ordinary paired
history retains accepted source/design and pending draft guards. This integration
is consumed by the public workbench and semantic boundaries described below.

TNyxFlowLimits.Defaults supplies 128 steps, 512 wires, 1 MiB per encoded value and
8 MiB per run. Fluent limits are caller-tunable within admitted finite ceilings.
The run counts invocation values and each produced value once, independent of
fan-out. Limits do not establish peak-memory, frame latency or arbitrary host-work
deadlines. JSON formatting cooperatively checks cancellation between emitted pieces.
Entered custom/blocking operations must cooperate themselves.

TNyxFlowExecution.Submit reuses Nyx's scheduler/query execution. UIQueue defers on
both targets; native Threaded uses a real worker, and browser Threaded refuses.
Asynchronous follows scheduler capability. UI-affinity operations refuse real
worker policies. Pure affinity is a creator promise about independent immutable,
thread-safe implementation state, not something a flag proves.

TNyxFlowApplication.Submit captures an active weak application context, exact
state identity/revision and page generation. Typed NyxFlowAssign overloads map
outputs to existing scalar state references. One ordinary State.Apply publishes
the complete group, and normal bindings update existing controls. Workers retain
only immutable graph/inputs; the UI courier owns the weak publication capability.
Destination admission reads an owned state snapshot. Creator getters are retained
and copied in a defined order; reentrant application retirement refuses before
live state is dereferenced or work is adopted.
JSON and bytes need explicit transformations before scalar publication. Exact
decimal outputs require canonical IEEE spelling for Number state; other spellings
or precision loss refuse instead of silently rounding. Explicit conversion coverage
remains required.

Cancellation combines explicit run cancellation and owning view retirement.
Pending work skips entry; entered graph/operation checkpoints revoke later steps
and publication. Cooperative cancellation is a cancelled outcome rather than a
compute failure. Stale state refuses the whole publication; navigation/disposal
revokes it. Dropping the run does not cancel it. Grouped output atomicity does not
roll back explicit host/UI side effects or a State observer that commits then throws.

The maintained command executes native shared and actual LCL application checks
and stages matched browser consumers for explicit HTTP execution:

```powershell
./tools/build.ps1 -Target logic-flow -LogicFlowEvidenceDirectory build/logic-flow/review
```

The English application connects parse/select/pretty/compact/join to real buttons,
a bound memo and result controls. It exercises multiple callback registrations,
native worker/browser event-loop publication, independent applications, failures,
pending/entered cancellation, stale state and page retirement. WORK.md records
exact counts, inspected desktop/narrow captures and limits.

Full coverage remains required: files/dialogs, HTTP, LINQ sorting/filtering/mapping,
regex/text transforms, data conversions, flow/timers, explicit feedback, script and
control-port discovery, embedded-canvas route attachment, interactive Tool Canvas wiring,
complete semantic authoring and preserving delivery. Hosted resource and user
activation/authority semantics belong to their existing adapters. This foundation
does not close those criteria or replace functional results with metadata cards.

`tools/build.ps1 -Target logic-document -LogicDocumentEvidenceDirectory <directory>`
qualifies saved declarations, source/history, exact compiled base and handwritten
continuation companions, and ordinary native controls. It stages matching browser
consumers; WORK.md distinguishes their actual HTTP execution from compilation.

Current-source Studio exposes **Project → Logic** as a dedicated workspace.
`NewNyxLogicEditor` is a public compound of ordinary Nyx cards, disclosures,
inputs, selects and buttons. It discovers operations, loads exact saved steps,
configures built-ins with typed layouts/numbers and structural Field/Item path
controls, and chooses typed output/input endpoints. Creator configuration uses
an explicitly labeled JSON data extension editor. Loading a creator retains that
step's own configuration-dependent schema. Removing a graph, step or wire needs
warning acknowledgement; removing a step explicitly detaches its connections.
These are accessible select/button alternatives, not canvas pointer wiring.

The form captures immutable guarded proposals. `NyxFlowPatch` and `EditNyxFlows`
stage related operation/wire changes before final graph admission, permitting
repair of intermediate signatures. Failure preserves original membership and the
accepted pair. Studio uses its ordinary source worker/compiler/publication queue;
one grouped change is one paired Undo. Draft forms retain malformed input, selected
schema and typed paths. Optional presentation version 17 parks those proposals per
project; untouched presentations retain exact version-16 bytes. Broader recovery/
navigation/ownership and large-workload qualification remain separate.

Current-source MCP adds the read-only `nyx_logic` tool. Scope `catalog` searches
intent/help/labels; `graphs`, `steps`, `ports` and `wires` return at most 32 rows
(8 default). Exact graph/step names scope configuration windows to at most 1024
Unicode scalars (512 default). Responses carry revision; pin expectedRevision
when paging. No query resolves runtime code or returns the complete document.
Compose flow-define/remove, flow-step-add/set/remove and flow-connect/disconnect
through the existing nyx_transaction tool, with expectedRevision and operationId.
Built-in step changes accept kind plus configuration; creator changes require
explicit schema data. Group guards, operator permissions, retry identity, draft
refusal and paired Undo/Redo remain ordinary Studio behavior. The source inventory
is 24 tools; the preserved installed release still advertises 23.

Ordinary application controls can now carry saved typed event-to-graph routes:

```pascal
SetNyxFlowRoutes(LFormatButton, NewNyxFlowRoutes.WithRoute(
  NyxFlowRoute(NyxFlowRouteID('Format JSON'), NyxFlowRef('Formatting'),
    ntClick, nfcmExclusive)
    .InputState(
      NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpText)),
      NyxTextState('source'))
    .OutputState(
      NyxFlowEndpoint(NyxFlowStep('format'), NyxLogicPort(nlpText)),
      NyxTextState('formatted'))
    .Using(neUIQueue).Concurrent(nfrLatest)));
```

Routes use distinct identities, exact input/output endpoints, typed state
references, constants, accepted scalar event values, structured event Field/Item
paths, or an explicit invocation signal. Every unwired input must be mapped once;
outputs map to existing scalar state without conversion. The complete effective
control/event schema admits before mounting. Disabled routes retain referential
integrity. Missing local routes inherit; an explicitly empty set masks inheritance.
`InheritNyxFlowRoutes` removes only that local declaration.

Browser/LCL application hosts attach ordinary realized page/reusable routes after
authored callbacks and before mounting. `ConfigureLogic` supplies optional explicit
resolver authority before mounting; its resolver must handle built-ins too.
Resolution is lazy on the first invocation, with retirement checks before reading
state. A saved creator declaration alone cannot gain host authority. `FlowRoutes`
exposes bounded per-route reports, exact cancellation and revocable Close without
retaining the application/document. No-output graphs still use guarded completion;
explicit host side effects already performed cannot be rolled back.

`nfcmExclusive` refuses matching ordinary callbacks, including registrations added
after mounting. `nfcmBeforeCallbacks` and `nfcmAfterDispatch` explicitly allow mixed
behavior. AfterDispatch means after invocation/submission, never after completion
of asynchronous callbacks. Optional event observers leave ordinary stream policy
unchanged and cannot consume physical defaults. Dispatch snapshots all phases
before entering any callback. Route policies are independent scheduler choices.

`nfrLatest` cancels superseded runs; `nfrEvery` admits each event up to caller-tuned
MaximumRuns (8 default, 1..64), and `nfrIgnoreWhileRunning` explicitly skips a busy
route. Cancelled workers cooperate; revocation prevents delivery rather than
forcibly interrupting entered host work. Graph output groups retain exact state
revision and view-generation guards. Reports distinguish succeeded, failed,
discarded, cancelled, skipped and active runs.

Current-source `nyx_logic` scopes `routes`, `route-inputs` and `route-outputs`
inspect an exact authored control and optional exact route. Mapping rows use
bounded Unicode scalar windows, including large structured constants. Query
revision pinning applies. `flow-routes-set` and `flow-routes-inherit` compose with
graph/property operations in one expected-revision `nyx_transaction`; typed
`NyxFlowRoutesPatch` supplies the same ordinary candidate/source/history path.
No new tool is added and no query executes a graph.

`tools/build.ps1 -Target flow-routes -FlowRoutesEvidenceDirectory <fresh-directory>`
qualifies saved routes, source/history, exact native base/handwritten companions,
actual native input and authenticated HTTP edits. It stages actual browser
counterparts for HTTP execution; FlowRoutesMCPPort defaults to unused localhost
9031. Visual route editing, control/script port breadth,
file/regex/HTTP operations, activation/host authority, full budgets and preserving
delivery remain separate requirements. This batch does not complete Tool Canvas.

`tools/build.ps1 -Target logic-authoring -LogicAuthoringEvidenceDirectory <fresh-directory>`
executes shared authoring, real native controls and authenticated HTTP semantic
checks. It stages browser counterparts **and their matching source worker**.
`-LogicAuthoringMCPPort` selects an unused localhost qualification port (9029
default); its fresh private runtime never enrolls or replaces the live project.
WORK.md owns actual HTTP browser execution, retained failures and scoped limits.

## Embedded controls and Tool Canvas

`NewNyxToolCanvas('data-tool').WithScene(NyxCanvasRef('tools'))` is a specialized
managed compound control. `WorkspaceCanvas`, `ToolsCard`, `ToolsTitleHeading` and
`ToolsPortHostScroll` expose its ordinary Nyx parts. Independent factory instances
own independent parts; required workspace/scroll roles are checked at admission.
The normal renderers mount the public port inspector into that scroll host.

Each runtime canvas item now binds its own authored callbacks and saved event
routes. The attachment shares the exact application state/collections and
explicit Logic resolver, with a separate weak context and generation. Camera
changes preserve that attachment. Removing an item, replacing its content or
leaving its page revokes it before disposing controls. Design previews do not
execute it. An explicit runtime mount with executable declarations requires an
application context; it refuses missing authority or a different state store.

The optional `INyxCanvasRuntimeView` exposes its runtime scene and exact mounted
item behavior. That scene is an independent materialization; runtime edits do not
rewrite the saved document. `INyxViewBehavior.EnableGraphs(False)` cancels graph
work while leaving ordinary callbacks enabled. Resuming accepts future events;
it does not replay paused input. Completed external side effects remain external.

`NyxToolControlPorts` captures immutable, exact item/control addresses: invocation
signals, available scalar/structured event payloads and real direct state-binding
inputs. Signal and optional data are distinct. Cleared, computed and resource
projections do not become writable state ports. Physical keyboard/pointer/editing
snapshot projections remain separate work. Discovery invokes no creator code.

The public inspector selects items/controls, searches and pages twelve ports,
shows their families/help, toggles port chrome and graph execution, cancels work
and refreshes bounded execution reports. Physical inspector publication runs on a
later UI turn: disposing a checkbox inside its own change notification is unsafe.
Its revocable queue receiver retires before the renderer/host. The private form
document excludes application scenes. One effective item snapshot is reused
until exact authored content or scene revision changes. Initial context validation
and full form reconstruction remain expensive; no production latency or memory
budget is claimed.

`tools/build.ps1 -Target tool-canvas -ToolCanvasEvidenceDirectory <fresh-directory>`
runs portable/source/history and actual native generated-companion checks, then
stages matching browser consumers for HTTP execution.

### Runtime connection editing

The optional `INyxToolCanvasConnections` capability adds a Connections tab beside
Control ports. Its `INyxToolConnectionEditor` uses real graph/script descriptors,
typed endpoint identities, help and twelve-entry pages. Select an output and an
input with ordinary buttons, then choose Connect; browser drag/drop supplies the
same operation. Occupied inputs require explicit Replace, and replacement/removal
require warning acknowledgement. Whole graph and route admission refuse wrong
families, missing inputs, cycles and stale selections before changing runtime
meaning. No label or transferred text is evaluated as Pascal or host authority.

For example, an application can use the same public controller without widgets:

```pascal
LConnections := LInspector as INyxToolCanvasConnections;
LEditor := LConnections.ConnectionEditor;
LEditor.SelectFlow(NyxFlowRef('Profile'));
LEditor.Apply(
  NyxFlowEndpoint(NyxFlowStep('script'), NyxFlowPort('Caption')),
  NyxFlowEndpoint(NyxFlowStep('choose'), NyxFlowPort('primary')),
  ntcaConnect);
```

`INyxCanvasFlowEditing.RuntimeFlows` returns detached declarations for explicit
application export. `ApplyFlowChanges` pins scene and graph revisions and uses
the existing typed flow edits. Graph revision is independent of panel resizing,
camera movement and ordinary refresh; an admitted no-op still invalidates older
wire selections. The private source and every affected mounted fragment admit
together through the existing prepared-publication group. The new route scope
inherits the existing observer registrations in place, retaining their order
relative to independent clients. The exact control/route topology must remain
unchanged for this strict graph-only capability. The optional dynamic route
capability below additionally admits explicit event registration changes.
Static callback context, editor faces and authored defaults remain retained.
Every graph run in an affected fragment retires; unrelated fragments retain their
pending work. Future mounts read the new private runtime source. New views and
nested independent workspaces retain their own authored defaults and authority.

Add `-ToolCanvasConnections` to the maintained command for prepared-replacement
checks and the extended generated-companion native input journey. It stages
`tool-connections-contract.html` and the Pascal `nyx_tool_connection_browser`
driver. Execute the staged HTTP control page with that driver at desktop/narrow
widths; compilation alone does not qualify keyboard or drag input. The fixtures
own their application stores and never replace an enrolled Studio project.

WORK.md separates real browser host drag/keyboard, native host keys and mounted
drag slots, source/history regression, and refused native foreground captures.
Current evidence does not establish a full native mouse-drop journey, hardware,
assistive technology, native visual quality or production cost. Narrow captures
retain a cramped canvas layout. Full canvas-anchored connectors, authored export
of these runtime graph edits, script palette/editor, independent
project breadth, operation coverage and preserving delivery remain required.

### Runtime control event routes

The Tool Canvas **Event routes** tab composes the same public
`INyxControlRouteEditor` used by Studio. Select an authored item/control in
Control ports, choose its event and pipeline, stage typed input/output mappings,
then Apply. Execution choices stay in a disclosure. Local typing synchronizes
the existing native/browser editor, retaining focus and caret. Hiding/reopening
the same selection retains unfinished fields. A stale proposal refuses Apply;
reloading current routes requires acknowledgement before discarding unfinished
input. Saved route removal uses the form's existing warning contract.

Applications can use optional portable `INyxCanvasRouteEditing` directly:

```pascal
LRouteEditing := LCanvas as INyxCanvasRouteEditing;
LChange := NyxCanvasRoutes(NyxCanvasItem('Greeting'),
  NyxControl('name-input'), LTypedRoutes);
LRouteEditing.ApplyRouteChanges([LChange], LRuntime.Scene.Revision,
  LRouteEditing.RouteRevision);
```

`LTypedRoutes` is an ordinary `INyxFlowRoutes` recipe. An explicit empty set
masks that control's authored routes. Apply admits one to sixty-four unique
item/control replacements at an exact scene and shared graph/route revision.
Whole candidate validation precedes coordinated fragment/overlay publication;
a later invalid item cannot publish an earlier successful prefix. Identical
route intent retains its fragment's pending work. Reusable instances have
independent overrides; controls bound to shared application state intentionally
continue sharing that value. `RouteContext` returns an owned inert document and
its borrowed root; callers free that document after constructing their form.

`INyxViewRouteTopology` and `INyxFlowRouteTopology` keep static callback contexts
and actual controls while changing event subscriptions.
`INyxEventObservationEditing` reserves additions inactive, invisible to dispatch
and subscriber queries until installation. Surviving slots transfer in place,
additions append within their event group, and removals disable before later
notifications cancel old work. Survivor reordering refuses instead of changing
external observer order. Abandonment, changed generation, cancellation and later
participant refusal revoke staged authority. Existing strict graph-replacement
interfaces and their identifiers retain their original contracts.

Each override belongs to a scene content lifetime. Removing/recreating an item
cannot resurrect old routes, even with the same root/name. The next candidate
reclaims dead entries before its 2,048 active override budget; dynamic publication
also bounds observation slot changes to 2,048. Preview hosts, retired views,
unsupported creator scenes and computed-only targets refuse. `CaptureRoutes`
returns detached typed records with exact `ToData`/`FromData` boundaries, never
widgets, tokens, credentials or execution provenance. Runtime edits leave the
authored design/source/history untouched. Explicit application adoption uses
ordinary authoring commands; new views start with authored defaults.

`tools/test-canvas-routes.ps1 -EvidenceDirectory <fresh-owned-directory>` runs
the Pascal observation/topology/overlay contracts and actual native input, then
stages browser counterparts and the Pascal ready driver. It starts no server.
Serve its owned `web` directory through the Pascal static host; use markers
`data-observation-publication`, `data-tool-connection-contract`,
`data-route-overlay` and `data-program-logic-controls`, with actual controls at
desktop/narrow widths. WORK.md owns executed evidence and retained failures.
The optional anchored connector surface below advances direct pointer/keyboard
wiring. Multi-selection drafts, full visual/accessibility quality, production
budgets and preserving delivery remain open.
Existing semantic authored-route edits do not grant access to a running
application's private controller; that gap remains with the workflow owner.

### Canvas control connectors

The ordinary Tool Canvas inspector's **Canvas connections** button toggles Nyx
port strips beside the selected item's mounted controls and a movable pipeline
card inside the canvas. Existing inputs keep their normal renderer, callbacks,
editing and bindings. Hiding the chrome retains its unfinished wire proposal.
Controls bound to the same application state continue sharing that state.

Choose an output and compatible input with Tab and Space/Enter, then **Stage
selected**; dragging an output to an input stages the same typed mapping. Staging
does not execute anything. **Apply route** admits the complete event-to-pipeline
inputs and bound-control outputs through the existing per-item route publisher.
Each proposal has one exact control/event owner. Different value families need
an explicit pipeline transform. Inputs supplied by existing graph edges are not
advertised as additional writable inputs. Control destinations require real
existing direct state bindings; computed-only projections are not writable ports.

**Connection options** exposes the pipeline, route name, replacement/removal
acknowledgement and staged-wire list. Removing a staged wire or the connector's
applied route requires acknowledgement. Removing execution does not disable
ordinary callbacks. Changed scene/graph revisions retain a stale proposal and
refuse Apply; Reload requires explicit draft discard. A failed discovery or
admission leaves the preceding context intact. Runtime intent never silently
changes authored design, Pascal or paired history.

Drag the pipeline header to reposition it; arrow keys move eight logical pixels,
Shift+arrows move thirty-two, and Home restores its default placement. Position
clamps to the current canvas allocation. Optional portable
`INyxToolCanvasConnectorPresentation` also accepts typed viewport points. Camera
and canvas resize frames queue geometry updates through a revocable observation,
without remounting embedded editors or a permanent polling timer. LCL preserves
pointer-up before capture-loss observation when its native button releases capture
inside the mouse-up message; independent interruption remains cancellation.

Applications may obtain `INyxToolCanvasControlConnectors` from the public
inspector. `NewNyxToolControlWires` supplies an independent managed controller with
explicit `TNyxToolWireOptions` for discovery, limits, callback/execution policy
and concurrency. Its copied ports and staged edges retain no widget authority;
Disconnect precedes application retirement. Target-specific optional inspection
interfaces borrow connector renderers only until rebuild/disconnection.

`tools/test-canvas-routes.ps1 -EvidenceDirectory <fresh-owned-directory>
-ControlConnectors` runs the Pascal contracts and actual native connector
journey, then stages the browser fixtures and Pascal input driver. It starts no
server. Serve that owned web directory with the Pascal static host, then run
`nyx_canvas_connector_input` with its URL, fresh output directory and CSS width/
height. That driver supplies real browser keys/touch; it does not compose designs.
WORK.md records executed checks separately from compilation and visual evidence.

This is a runtime wiring surface, not a complete diagram editor or semantic
project-adoption workflow. Broader port/graph authoring, arbitrary creator-part
geometry, nested scrolling/reflow tracking, full native visual/accessibility
quality and production budgets remain open. Desktop and narrow host fixtures do
not qualify phone hardware, pen, IME, assistive technology or other widgetsets.
The existing workflow owner retains bounded runtime inspection/adoption; authored
MCP route operations remain distinct. No installed tool/schema or release changes.

### Authored component event routes

Both Studios mount the public `INyxControlRouteEditor` in Logic for the selected
component. Its specialized managed card exposes typed parts such as EventSelect,
GraphSelect, NameInput and ApplyButton. The form borrows the document only during
construction/admission, retaining inert signature metadata and independent Nyx
children. Opening a creator graph does not reconstruct executable authority.

Choose an event and pipeline, stage input/output mappings, then Apply once.
Inputs accept exact declared event values, structured event details with typed
Field/Item paths, existing state/control bindings, constants or invocation signals.
All seven Flow value families retain their exact meanings. Named creator events
use their declared scalar/JSON payload schemas. Bytes use canonical Base64 at
the form boundary; large JSON numbers and decimals retain exact numeric text.
Incompatible families require an explicit graph transform.

Outputs select actual direct control state bindings or explicit typed state.
Computed/resource projections are not fabricated as writable ports; create a
binding in the existing Bindings inspector first. State publication updates every
control bound to that state. Existing Table mappings survive ordinary route edits;
the separate Table editor remains their recipe editor. Clearing all mappings and
removing routes require explicit acknowledgement. Renaming edits the loaded route
in place, retaining registration order instead of leaving a second active route.

Execution policy, concurrency, bounded active runs and ordinary callback order
live in a collapsible section. Its public Nyx disclosure toggles synchronize the
same form. Unfinished fields, paths, staged mappings and disclosure state survive
closing/reopening Logic and serialized parent drafts. Untouched forms preserve
legacy parent-draft bytes. Changed schemas/owners/revisions refuse stale drafts
before modifying any parent or child input.

`CaptureNyxControlRouteEditor` supplies one detached complete route-set proposal.
Studio turns it into the existing `NyxFlowRoutesPatch` and ordinary paired source
command. Missing inputs, invalid values and pending Pascal refuse without changing
the accepted pair/buffer/base; Undo/Redo retain the exact design and Pascal.
Bounded `nyx_logic` route/input/output queries and the existing authenticated
`flow-routes-set` transaction provide the same semantic publication. No new MCP
tool/schema or installed capability is introduced by this form.

`tools/test-control-routes.ps1 -EvidenceDirectory <fresh-owned-directory>` runs the
Pascal contract, ordinary native Studio and its exact generated Tool Canvas
application, then stages the browser counterparts/worker and Pascal ready driver.
Serve its `web` directory through an owned Pascal HTTP host. Execute
`control-routes.html` with marker `data-control-route-contract`,
`program-studio.html` with `data-program-studio`, and
`program-logic-controls.html` with `data-program-logic-controls` at desktop/narrow
widths. The Studio driver option `--control-route-export` retains exact authored
bytes for independent compiler/runtime checks. `nyx_program_logic_mcp` accepts
`--control-routes` only after its four explicit isolated-host arguments, exercising
grouped revision-aware publication and both real application compilers.

WORK.md records actual executed checks and retained failures. These fixtures do
not establish full native visual quality, hardware/assistive input, production
cost, automatic binding creation, canvas-anchored control wires, full interpreter
capability or preserving delivery. Runtime sandbox graph rewiring and authored
project/source route publication remain distinct operations.

## Asynchronous host operations

`INyxAsyncLogicOperation` is an optional capability beside the original operation
interface. `BeginInvoke` belongs to the supplied UI scheduler and may reply inline
or later. Its managed completion port accepts one complete frame or failure; its
invocation token disconnects pending work. Implementations marshal replies to the
UI scheduler. Cancellation and retirement suppress output rather than fabricate a
successful result. Already entered external side effects cannot be rolled back.

Built-in graphs expose optional `INyxResumableFlowGraph` continuations. A cursor
owns exact inputs, completed frames and budgets for one run. `Accept` validates
the complete pending output before advancing; `Result` refuses until every step
succeeds. Synchronous `Evaluate` refuses async graphs before entering any step.
Pure graphs keep the existing query-worker execution path.

`TNyxFlowExecution.Submit` coordinates async graphs on the UI scheduler. Each host
reply resumes on a later UI turn. Asynchronous pure steps use native workers or
the browser event loop; Sequential/UIQueue keep them on the UI queue. Threaded
refuses an async host graph. A temporary owned run lease and revocable weak ports
retire on explicit cancellation, generation retirement or scheduler shutdown,
including when the caller drops its run without polling. No partial result
reaches the ordinary final publication port.

`nyx.logic.http` supplies `INyxHTTPCall` and `INyxUTF8Decode`. The HTTP component
accepts a data-only `TNyxHTTPRequest` and optional explicit `INyxHTTPTransport`:

```pascal
LRequest := NyxHTTPRequest(NyxResourceURL('https://example.invalid/api/orders'))
  .Method(nhmPost)
  .Header('Content-Type', 'application/json')
  .WithBody(NyxEncodeUTF8(LJSONText))
  .WholeRequest(15000)
  .MaximumBytes(262144);
LRequestLogic := NewNyxHTTPCall(LRequest, LTransport);
```

Methods have enum choices and a distinct validated extension reference. Headers
are ordered immutable name/value pairs; responses retain duplicate observable
fields. Protocol token/Latin-1 admission, field/byte limits, request persistence
and detached body bytes share native/browser behavior. GET/HEAD bodies and
host-managed headers refuse at this portable boundary. These choices follow the
[Fetch method/header contract](https://fetch.spec.whatwg.org/). HEAD response
framing follows [HTTP semantics](https://www.rfc-editor.org/rfc/rfc9110.html#name-head):
its Content-Length describes the GET representation, not a received body.

Typed HTTP ports expose trigger, body bytes, status, headers, error, success and
header visibility. Header visibility carries `TNyxHTTPHeaderVisibility`'s ordinal,
distinguishing native headers from Fetch's observable filtered subset. A complete
non-2xx response retains its status/body and empty transport error. A transport
failure is usable output data; success requires both complete transport and 2xx
status. An unconditional downstream decode/parse can still fail on an error body;
connected outcome handling remains the caller's explicit choice.

`NewNyxHTTPLogicResolver` reconstructs exact graph configuration while injecting
transport authority; it also resolves the decoder and existing pure built-ins.
Saved request/graph data never supplies that authority. Direct `BeginInvoke`
requires a completion receiver and scheduler. Ordinary synchronous Invoke refuses.
`NewNyxBrowserHTTPTransport` and Win32 `NewNyxNativeHTTPTransport` extend the
existing resource byte/deadline/cancellation pipeline through an optional interface,
preserving the original resource GET interface. Request bodies remain owned until
native handle retirement. The current one-shot transport inherits the resource
adapter's cache bypass behavior. Tunable cache/retry, other native widgetsets,
real network qualification and full HTTP acceptance remain required.

The default Studio catalog contains ten operations. HTTP and UTF-8 decoding
participate in intent grouping/search/help, typed configuration, saved connections
and ordinary paired commands. `nyx.http.editor` supplies a public reusable Nyx
request form: method enum/custom token, URL, individual header rows, UTF-8/Base64
body presentation and numeric deadline/response limits. Changing body presentation
preserves bytes. Malformed bytes and embedded NUL remain in Base64 presentation;
invalid conversions preserve the previous presentation and unfinished content.
Header insertion/replacement validates before changing admitted rows. The initial
saved header is selected after publication so an admitted row is visible.

`Header` replaces matching request fields; `AppendHeader` preserves an additional
ordered occurrence. Loading/capturing a request and generated source retain
duplicates. Generated HTTP uses specialized constructors, typed methods/ports,
readable `NyxEncodeUTF8` text bodies and explicit Base64 for binary data. The
literal reader admits these data-only constructs without constructing transport
authority. Imports are included for flow-bearing projects even without resources.
Handwritten continuation retains the corresponding public imports.

Logic editor draft version 3 owns the request form's unfinished values and header
rows beside existing fields. Versions 1/2 retain their original encoding and
restore against the existing exact graph-context guards. An incomplete version-3
child refuses rather than silently downgrading. Local source/semantic evidence
does not imply authenticated current-schema HTTP observation or installed rollout.
The frozen release remains unchanged.

`tools/build.ps1 -Target logic-http-authoring
-LogicHTTPAuthoringEvidenceDirectory <fresh-directory>` executes shared typed
source/editor/semantic checks, emits an exact companion and exercises its actual
native application and public Logic form through ordinary Studio source commands.
It stages matching browser consumers **and their source worker** for explicit HTTP
execution. The application provider is deliberately substituted and starts no
listener. Real network qualification, tunable retry/cache, saved row-route authoring,
file capabilities and full operation/quality/delivery acceptance remain required.

Typed application code can now send a JSON output to an existing runtime
collection with `nyx.data.rows` and `NyxFlowAssignRows`:

```pascal
LTeamRows := NyxDataRows
  .Identity(NyxDataPath.Field('id'))
  .Text(NyxTextField('name'))
  .DecimalText(NyxTextField('rank'));

LRun := TNyxFlowApplication.Submit(LContext, LGraph, LInputs,
  [NyxFlowAssign(NyxTextState('summary'), LFormattedOutput)],
  [NyxFlowAssignRows(NyxCollection('people'), LProfilesOutput, LTeamRows)],
  neAsynchronous);
```

The immutable recipe maps structural paths to exact typed fields and stable text
identities. It must cover the destination's complete schema by name/family;
runtime field order, defaults and domains remain owned by that store. Missing,
null, wrong-family, duplicate-identity or domain-invalid data refuses the complete
candidate. Text/Boolean/Integer/Number retain their ordinary typed meanings.
DecimalText explicitly projects a JSON number's original spelling into text.
Number defaults to canonical IEEE round-trip spelling: even an equivalent
noncanonical `1.0` refuses. `Numbers(nrnpApproximate)` opts into ordinary AsNumber
conversion; there is no hidden precision loss. Existing resource-row recipes
delegate to the shared mapper while retaining their original version-one bytes
and existing explicit numeric-conversion behavior.

The mixed Submit overload captures exact state/collection revisions and owning
generation, prepares all candidates, validates all, installs all, then notifies
all. A later invalid recipe cannot publish earlier rows or companion state.
Rows-only publication is supported. A committed observer error remains an error
about committed models, never a fabricated rollback. Dropping/retiring prepared
work releases every reservation. Scalar-only Submit retains ordinary State.Apply.

State.PrepareApply supplies the same revocable-owner protocol as collections.
The original preparation interface/GUID remains available through
nyx.publication; independent nyx.publication.types avoids a state unit cycle.
An optional readiness capability is captured before validators and checked after
all validators, before any installation. This catches a scalar owner disposed
by a later validator, including nested groups. Readiness/installation contracts
for creator implementations prohibit external callbacks or candidate allocation.

`tools/build.ps1 -Target logic-table -LogicTableEvidenceDirectory <fresh-directory>`
executes portable mapping/group checks, emits an exact generated document and
runs its actual native Table consumer. Matching HTTP-served browser consumers
qualify desktop/narrow controls separately. The useful chain is HTTP → UTF-8 →
JSON → select → filter → exact sort → map → Table, with bound formatted output
published alongside rows. Its provider is explicitly substituted, with no
listener or network I/O. The handwritten callback supplies this direct runtime
mapping. Saved routes below use the same public publication boundary.

Saved control/event routes expose the corresponding fluent contract:

```pascal
LRoute := LRoute.OutputRows(LProfilesOutput, NyxCollection('people'), LTeamRows);
LRoute := LRoute.WithRows(LProfilesOutput, NyxCollection('people'), LEditedRows);
LRoute := LRoute.WithoutRows(NyxCollection('people'));
```

OutputRows refuses duplicate destinations. WithRows replaces in place or appends
a new destination. WithoutRows preserves every other mapping. Scalar OutputCount/
Output access remains unchanged; RowOutputCount/RowOutput describes collection
outputs separately. The original route-set interface/GUID stays intact. Legacy
scalar-only route descriptors keep their exact eleven fields. A nonempty optional
rowOutputs array carries independent source/collection/typed-recipe data; removing
its last entry restores the old encoding. Both families share the 256-output
limit. Whole model/effective-control admission checks JSON output direction,
existing collection identity and complete field families before subscription.

Ordinary application and embedded route attachments submit the saved mappings
through the same grouped state/collection runtime. Generated Pascal emits
NyxDataRows, typed field references, structural Field/Item paths, DecimalText and
closed numeric policies. Literal admission retains these descriptions without
runtime authority. Public generated/handwritten imports include nyx.data.rows.

Named Pascal programs are also specialized Flow operations through
[`INyxProgramLogic`](programs.md#saved-event-to-program-routes). A document-aware
catalog exposes an inert program template; the public Nyx configuration card
offers saved names and typed ports. Saved control/event routes resolve independent
program definitions at runtime and publish through the existing scheduler and
revision/generation guards, including controls inside Tool Canvas items. Pure
scalar/JSON/byte programs are supported; scene handles and host proposals require
their owning adapter. The same typed constructor and explicit number policy
survive crafted source and paired history. This does not add a new MCP tool or
qualify the frozen observing release.

The public nyx.flow.rows.editor component edits outputs on an existing local
control route. Studio mounts it in the Logic workspace for the selected control.
It loads saved mappings, uses the destination schema, edits Rows/Identity/Field
paths with Field/Item segments, configures text projections/numeric policy and
requires explicit removal confirmation. Field selection displays the admitted
projection. Open names are exact text; graph/collection admission validates them.
The component event route form above supplies general event/input creation;
richer searchable choices and broader Table mapping UX remain open. The Table
form owns no document, transport, renderer or session.

Apply/removal goes through ordinary paired Studio source commands. Scoped route/
collection-field context and selected ownership guard capture; the command seals
the complete current pair/draft independently. An optional caller revision guards
standalone hosts; Studio uses its exact scoped baseline and existing command
context. Unfinished paths/projection choices remain disposable form drafts.
Logic draft version 4 owns the nested Table draft; versions 1/2/3 retain their
original bytes when no nested Table input exists. Pending Pascal drafts reject
grouped edits while preserving the full accepted pair, draft text and base.

nyx_logic adds scope route-row-outputs at an exact control/route/revision, with
bounded scalar text windows. Route summaries include the collection output
count. The existing flow-routes-set transaction admits the optional typed array
through the same complete model checks, exact retry and one paired Undo step.
This adds no tool: source inventory stays 24, installed inventory 23. Local
semantic qualification is separate from authenticated current-schema observation.

`tools/build.ps1 -Target logic-row-routes
-LogicRowRoutesEvidenceDirectory <fresh-directory>` runs shared saved/source/
draft/semantic checks, actual native mapping input through source commands, and
the exact UI-authored compiled Table application. It stages matching browser
form/application consumers and source worker. Actual HTTP browser checks cover
desktop/narrow inputs separately. The application qualifies every scalar row
family, exact large-number text and supplementary/NUL structural member names.
HTTP remains explicitly substituted; network/host-file outcomes, full quality/
budget/accessibility/parity, interactive wiring and preserving delivery stay open.


`tools/build.ps1 -Target logic-hosts -LogicHostsEvidenceDirectory <fresh-directory>`
runs native continuation/component checks with an explicitly substituted transport.
It compiles/stages matching HTTP browser checks and real network consumers; it
starts no server. `logic-async.html` and `logic-http-contract.html` qualify the
portable contracts when executed. The separate Pascal `nyx_http_logic_fixture`
producer and `logic-http-network.html` consumer require an independently admitted
loopback host. Compilation/staging never counts as network execution. WORK.md
records the current rejected producer launch and exact remaining boundary.
