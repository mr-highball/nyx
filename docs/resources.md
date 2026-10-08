# Resources and data bindings

[Project](../PROJECT.md) · [Open resource task](../TODO/NS-1_resources_01.md) ·
[Images](images.md) · [Collections](collection-views.md)

Resources are named immutable files owned by a document. PNG/JPEG images,
JSON, UTF-8 text and arbitrary bytes share creator metadata, exact persistence
and independent catalog membership. Hosted declarations carry an HTTP(S) URL,
typed cache policy and an optional embedded fallback through the same contract.
Neither loading a project nor replaying its builder opens files or fetches URLs.

## Captions and prompts

Use `nyx.resources` alongside the ordinary specialized controls. These examples
belong inside a document builder, after creating its page. Resource and JSON
field names are application data; property behavior uses typed fluent calls.

```pascal
LDocument.Resources.Define(NyxResourceRef('workshop-copy'),
  NyxJSONResource('{"headline":"Create something wonderful","prompt":"Project name"}')
    .Describe('Workshop copy', 'Shared captions and input prompts.'));

LHeadlineLabel := NewNyxLabel('headline');
LHomeColumn.Add(LHeadlineLabel);
LHeadlineLabel.Binds
  .Text(NyxResourceValue(NyxResourceRef('workshop-copy')).Field('headline'))
  .Done;

LProjectNameInput := NewNyxInput('project-name');
LHomeColumn.Add(LProjectNameInput);
LProjectNameInput.Binds
  .Placeholder(NyxResourceValue(NyxResourceRef('workshop-copy')).Field('prompt'))
  .Done;
```

Text is the default scalar projection. `.AsBoolean`, `.AsInteger` and `.AsNumber`
select other closed scalar kinds. Binding admission checks that the selected kind
matches the target property; wrong JSON types are refused without coercion.
`.Field('address').Field('city')` and `.Item(0)` append structural steps. A dot
inside a field name stays a literal dot. A text file binds at its root; binary
files and images are obtained through their typed definition accessors.
Resource bindings are read-only projections; editing a control does not rewrite
its packed file. Existing typed state bindings supply writable application data.

## Locale variants and view reload

```pascal
LDocument.Resources.Define(NyxResourceRef('workshop-copy'), NyxLocale('en-GB'),
  NyxJSONResource('{"headline":"Your resource workbench","prompt":"Programme name"}'));

LRuntimeResources := LDocument.Resources.Clone;
LRenderer.ReloadResources(LRuntimeResources, NyxLocale('en-GB'), NyxDefaultLocale);
```

Both renderer adapters expose the same reload method. A realized view privately
copies catalog membership; it can outlive the source document. Reload validates
every scalar selector and concrete property on a detached candidate, then updates
existing controls. Invalid fields or values preserve the accepted projection.
Control synchronization failures are reported after publication, as with ordinary
state updates. Reload remains a UI-thread operation confined to that mounted view.

Lookup tries the exact selected locale, the explicit fallback locale and then
the unlocalized default. Missing resources refuse. A selector can fix its own
locale with `.Localize(NyxLocale('en-GB'), NyxDefaultLocale)`; otherwise it uses
the view's explicit runtime locale. No operating-system locale is selected
implicitly. Full application hosts retain their own accepted catalog and locale
through navigation/remounts; view-only reload remains confined to that view.

## Application loading and lifetime

Both application hosts expose the same portable managed runtime contract from
`nyx.application.resources`. Embedded files are immediately available. Hosted
variants start automatically after a complete mount, with at most four concurrent
requests by default. Configure before mounting to change loading, concurrency,
whole-request deadline or explicit initial locale:

```pascal
LApplication.ConfigureResources(NyxApplicationResourceOptions
  .ConcurrentRequests(2)
  .Request(NyxResourceLoadOptions.WholeRequest(10000))
  .Localize(NyxLocale('en-GB'), NyxDefaultLocale));

// Use Loading(nrlOnDemand) to defer transport until the caller requests it.
LApplication.Resources.Reload(NyxResourceRef('workshop-copy'), NyxDefaultLocale);
LApplication.Resources.Localize(NyxLocale('en-GB'), NyxDefaultLocale);
LStatus := LApplication.Resources.Status(NyxResourceRef('workshop-copy'), NyxDefaultLocale);
```

The default browser adapter uses fetch and browser Cache Storage; the current
native adapter uses WinHTTP and a bounded private temporary cache. Each resource's
typed cache policy still controls every hit/store, including caller override.
Supply a replacement `INyxResourceResolver` as ConfigureResources' second
argument to replace that path. No DOM/LCL/HTTP types enter the portable contract.
Decoding a document or constructing its source builder never performs loading.
Bound hosted values currently need an admitted same-kind authored fallback to
support the first synchronous mount; unbound files can load without one.

`Resources.Context` is an immutable snapshot frame: Snapshot returns independent
catalog membership, with read-only locale/fallback. Loading replaces private
runtime entries; Declaration retains the original immutable URL/policy for retry.
Sibling applications and saved defaults stay independent. Embedded Reload is a
no-op. Loading phases distinguish queued/loading/waiting/ready/failed/rejected/
cancelled; Origin distinguishes network/cache/embedded/fallback. Error,
CacheWarning and NotificationError report separate boundaries. Ready with
fallback origin retains the original loading error.

Results publish through Nyx's deferred UI scheduler, after all application page
prototypes and the current target projection accept the candidate. Hidden pages
use their concrete platform overrides and the accepted catalog during later
state validation. Wrong paths/types preserve the previous entire catalog.
A busy receiver retains its result until Wake; renderer completion/input queues
supply that wake. Ordered borrowed validator/change subscriptions must Disconnect
before their objects are freed; Stop/Disconnect can also retire receivers inside
a callback. Loading/locale reentry refuses. A custom busy receiver supplies its
own idle Wake.
Localize is synchronous and refuses busy/invalid candidates. Notification failures
occur after accepted publication and are reported separately, as with state updates.

Application loads span page navigation and apply to the current mounted view,
without retaining retired nodes. Cancel retires requests/pending results; Stop
also revokes subscriptions and queued weak ports before host destruction. Neither
shuts down a borrowed scheduler. Retained stopped owners remain inspectable and
refuse new work; retained immutable frames remain independent. These are
UI-thread operations. Different resource variants publish individually; related
authored file/binding changes use Studio's grouped paired transaction.

Maintained `tools/build.ps1 -Target application-resources` qualifies actual
Win32 application controls, hidden pages, reusable scopes, state validation,
locale/navigation, synchronous/deferred replies, bounded/replaced requests,
retirement, observer errors and the default real HTTP path. Browser counterparts
compile/stage without execution evidence. Saved recipes now seed independent
runtime tables; automatic row loading, combined scalar/table publication and
Studio/MCP runtime load-status surfaces
remain open; this application lifecycle does not establish those outcomes.

## JSON rows and ordinary tables

Use `nyx.resources.rows` with the existing collection/view engine:

```pascal
LRows := NyxResourceRows(NyxResourceRef('team-data'))
  .Field('people')
  .Identity(NyxResourcePath.Field('id'))
  .Text(NyxTextField('name'))
  .Integer(NyxIntegerField('score'));

LStore := NewNyxCollection(LRows.Read(LRuntimeResources, NyxCollection('people'),
  NyxDefaultLocale, NyxDefaultLocale));

LTableView := NewNyxCollectionView(LStore, NyxCollectionView(NyxCollection('people'))
  .Column(NyxTextField('name'), 'Name')
  .Column(NyxIntegerField('score'), 'Score', cmEditable), cpTable);
LMount := LRenderer.BindCollection('people-table', LTableView);

LRows.Reload(LStore, LRuntimeResources, NyxDefaultLocale, NyxDefaultLocale,
  LStore.Snapshot.Revision);
```

The referenced JSON object contains a `people` array. Each row requires a unique
text `id`, a text `name` and an Integer `score`. An optional second argument to
each typed field supplies its structural source path. Missing fields, nulls,
duplicate IDs, wrong scalar kinds, stale revisions and collection budgets refuse
the complete row candidate. Stable row identities retain table selection;
notifications use the ordinary collection engine. Keep the mount token for the
control's lifetime and disconnect it before destroying its receiver.

`Read` and `Reload` are explicit runtime operations. Static collection defaults
preserve materialized rows without inventing a resource relationship. Use the
saved recipe contract below when the project should retain its source mapping.

## Saved row recipes

Declare the relationship on the document, then bind an ordinary specialized
table through its typed collection view. Import `nyx.resources.rows` alongside
`nyx.resources`, `nyx.controls`, `nyx.collections` and
`nyx.collections.view.types`:

```pascal
LDocument.Resources.Define(NyxResourceRef('team-data'),
  NyxJSONResource('{"people":[{"id":"ada","name":"Ada","score":9}]}'));

LDocument.ResourceCollections.Define(
  NyxCollection('people'),
  NyxResourceRows(NyxResourceRef('team-data')).Field('people')
    .Identity(NyxResourcePath.Field('id'))
    .Text(NyxTextField('name'))
    .Integer(NyxIntegerField('score')));

LPeopleTable := NewNyxTable('people-table');
LPeopleTable.Binds
  .Collection(NyxCollectionView(NyxCollection('people'))
    .Column(NyxTextField('name'), 'Name', cmEditable)
    .Column(NyxIntegerField('score'), 'Score'))
  .Done;
LHomeColumn.Add(LPeopleTable);
```

The managed source facade shares the document's owned collection registry. It
owns a copied immutable recipe and an **empty typed schema seed**, without a
catalog/document backreference. `Collections.Snapshot` describes that authored
seed; it does not report runtime rows. `ResourceCollections.Source` returns an
independent recipe. Ordinary static `Collections.Define` explicitly clears a
source relationship; Remove retires both seed and recipe. Clone preserves source
membership independently. A default Studio collection edit refuses a saved
recipe rather than silently converting it to static data.

Document validation resolves every recipe against authored resource data and
admits all collection projections. Missing/null/wrong-kind paths, nontext or
duplicate identities and invalid schemas/hierarchies refuse the complete
candidate. Structural `.Field`/`.Item` steps retain literal dotted names and
array identities. The four field methods enforce text/Boolean/Integer/Number
families; exact JSON number spelling remains in the resource while `.Number`
requests an explicit Double projection.

Both application hosts and standalone renderers initialize independent stores
from the accepted resource frame and explicit initial locale. Configure an
application's locale before mounting, or supply a standalone renderer's immutable
`ResourceContext` before rendering. Ordinary reusable `.Scoped(csInstance)`
bindings start independently from the captured resolved seed. Runtime editing
does not rewrite the resource file or another instance; navigation retains edits.
Full/page/reusable generated builders retain the recipe rather than emitting
copied rows. No constructor fetches URLs; initial hosted sources need fallback.

Manual hosts can call `NewNyxCollectionContext(defaults, resources, locale,
fallback)` or its immutable resource-frame overload. `MaterializeNyxCollectionDefaults`
returns independent static resolved seeds at an explicit locale. The raw
`NewNyxCollections(defaults)` snapshot factory has no catalog/locale and therefore
copies authored seeds, including empty source seeds; application code uses the
context/materialization path to resolve them.

Automatic/on-demand hosted completion and explicit localization now prepare one
joint resource frame in both application hosts. The catalog, mounted scalar
properties, shared rows, all resolved reusable rows and future instance seeds
install before any resource/store/view observer. A later invalid table receiver,
wrong row family or hidden-page scalar constraint rejects the complete candidate.
Busy readiness waits for Wake; a failed preparation releases all earlier holds.
Target painting is sequential notification work, so observers should query
accepted model/view snapshots rather than infer that every physical pixel has
already repainted. A receiver failure reports committed data separately; remaining
receivers still run, and destruction revokes borrowed target pointers.

Reload changes rows only when the resolved source dataset differs from its last
accepted seed. An unrelated resource, repeated locale or equivalent source
preserves local row edits. A changed source replaces its mapped runtime rows in
every scope, with normal query/selection reconciliation; it does not merge local
edits into the resource file. All stores/control identities remain retained.
New scopes refuse during the group and start from its installed seed afterward.
State writes, mounted view replacement and application navigation refuse until
the complete resource publication retires. Independent applications remain private.

Optional `INyxPreparedApplicationResources.SubscribePrepared` allows an embedding
host to contribute its own `INyxPreparedPublication`, with a readiness validator
and typed immutable context. Preserve nonthrowing Install and idempotent Retire;
never keep borrowed node/receiver pointers without a revocation mechanism.
`PrepareNyxCollectionContextResources` contributes a complete scope context;
`PrepareNyxGroup` composes its children without notifying early. Original owner,
collection and view-set interface GUIDs stay unchanged. Source-aware alternatives
must opt into prepared publication; static alternatives retain their contract.
Studio/MCP row recipe inspection/editing and runtime load diagnostics remain open.

Run `tools/build.ps1 -Target resource-mappings`: 69 checked shared/actual Win32/
source/semantic assertions, 112 live application assertions, and three exact compiled full/page/reusable builders
with eight table checks each pass leak-free. Matching browser consumers, Studios,
backend and worker compile with zero owned warnings. Browser/phone/observing
execution remains unqualified. Semantic resource replacement uses a fresh actual
suspended dispatcher with expected revision and one paired Undo, preserving the
active project. Late-scope seed and differently localized runtime attachment
checks are model oracles beside the actual mounted application journey, not
additional target/phone evidence. See
[current evidence](../WORK.md#current-return-path-joint-live-resource-frames--2026-10-08).

## Coordinated runtime row publication

Related datasets can prepare independently and publish through one portable
`nyx.publication` group. `TNyxResourceRows.PrepareReload` reads and normalizes the
resource into a detached snapshot, then reserves the runtime store at the
captured revision. Creating the preparation invokes no receivers. Callers retain
the returned `INyxPreparedPublication`; release or explicitly Retire abandoned
work, including when a later preparation fails.

```pascal
LTeamUpdate := nil;
LReviewUpdate := nil;
try
  LTeamUpdate := LRows.PrepareReload(LTeamStore, LRuntimeResources,
    NyxDefaultLocale, NyxDefaultLocale, LTeamStore.Snapshot.Revision);
  LReviewUpdate := LRows.PrepareReload(LReviewStore, LRuntimeResources,
    NyxDefaultLocale, NyxDefaultLocale, LReviewStore.Snapshot.Revision);
  PublishNyxGroup([LTeamUpdate, LReviewUpdate]);
finally

  if LTeamUpdate <> nil then
  begin
    LTeamUpdate.Retire;
  end;

  if LReviewUpdate <> nil then
  begin
    LReviewUpdate.Retire;
  end;
end;
```

The group captures its own managed participant vector, validates **every**
candidate, installs every accepted model, then notifies every participant.
Built-in collection views prepare complete query/selection/hierarchy projections
before installation. An invalid later tree or table preserves all stores/views
and synchronizes no mounted control. The first observer can read every accepted
store/view, including another participant. Competing dataset, selection, query
and subscription commands refuse while a store is reserved; defer them until
the group retires. Tokens remain borrowed and can disconnect during callbacks.

`INyxAtomicCollection` is an optional capability; the original collection GUID
and interface remain unchanged. Alternative stores may opt in, and ordinary
single-store stores remain supported by the original view path. Prepared adapter
subscriptions allocate/admit in their validator, adopt references without user
callbacks or allocation in their installer, and discard abandoned candidates in
their nonthrowing retirement callback. Custom participants must honor this same
contract: the coordinator cannot undo an extension's throwing installer.
Properly ordered Install is adoption only; out-of-order/repeated phase calls
refuse. Preparations are single-use, UI-thread-only and retain their model,
without owning documents, renderers or callback receivers. Groups accept 1..2048
distinct nonnil participants; oversized groups are not adopted. Callers must not
manually retire another participant or invoke its phases inside an active group.

Receiver failures occur after commit. The coordinator continues independent
notifications and raises `ENyxPublicationNotification`; ordinary collection
commands retain `ENyxCollectionNotification`. Empty error messages still count
as failures. Unchanged assignments reserve safely but produce no new revision
or notification. Stable row identities retain table selection; Assign remains
an explicit remove/insert replacement, so tree disclosure follows its existing
replacement rules rather than a new identity-preserving refresh policy.

Target controls synchronize during notifications after complete model admission;
this is not a promise of simultaneous physical painting. This explicit row group
does not save an automatic recipe, publish scalar resource contexts, or register
an application loader. Saved mapping/source replay, reusable-instance routing and
joint scalar/table publication retain the resource task's original scope.

`tools/build.ps1 -Target resource-publication` passes 43 checked shared/actual
Win32 assertions, leak-free. It covers grouped table updates, later tree refusal,
selection/control identity, cross-view revision visibility, reservation/reentry,
abandonment/retirement, Unicode/empty observer errors and release of the caller's
entire participant vector during notification. Browser counterparts, both
Studios, backend and worker compile with zero owned warnings; browser execution
and observing Studio remain separate qualification requirements.

## Hosted declarations and caller cache policy

Use `nyx.resource.sources` to declare an absolute HTTP(S) location:

```pascal
LDocument.Resources.Define(NyxResourceRef('remote-copy'),
  NyxHostedResource(nrkJSON, NyxResourceURL('https://example.com/copy.json'))
    .Fallback(NyxJSONResource('{"headline":"Ready to create"}'))
    .Cache(NyxResourceCache
      .Persistent
      .FreshFor(600)
      .StaleFor(60)
      .MaximumBytes(65536)
      .ServerPolicy(rcspRespect)));
```

Embedded fallback data has the same kind and supports immediate synchronous
binding. A hosted definition without a fallback refuses synchronous content
access until loaded. Current declarations, policy logic and storage adapters
are implemented. A resolver explicitly loads the declaration; decoding/replaying
this example still does not fetch its URL. Application hosts supply the automatic
runtime lifecycle above; the common Studio authoring and semantic tools manage
declarations rather than fetching preview resources.

## Loading and publishing

`nyx.resources.loader` supplies replaceable transport, UTC-clock and resolver
interfaces. A resolver owns private providers; it never retains a document or
widget. Both public-file adapters accept bounded HTTP(S) bytes, refuse redirects
and ambient credentials, and report failed loading rather than substituting an
opaque browser image or unqualified native string.

```pascal
// Native construction uses the existing bounded Nyx scheduler.
LScheduler := NewNyxScheduler;
LResolver := NewNyxResourceResolver(
  NewNyxNativeResourceTransport(LScheduler), nil, NewNyxFileResourceCache);

FLoad := LResolver.Load(FHostedDefinition,
  NyxResourceLoadOptions.WholeRequest(15000), ResourceLoaded);
```

The native factory lives in `nyx.resources.http.lcl`; the current implementation
is Win32 WinHTTP, including system certificate verification and TLS 1.2/1.3 where
supported. It uses asynchronous request handles on Nyx's bounded worker pool,
counts queued time toward the deadline and posts delivery with the parent's
cancellation token. Native callback state/read buffers remain alive through the
final closing notification; no UI thread joins network work. Other native systems
still need an adapter. See [WinHTTP concurrency](https://learn.microsoft.com/windows/win32/winhttp/concurrency-in-winhttp)
and [handle lifetime](https://learn.microsoft.com/windows/win32/api/winhttp/nf-winhttp-winhttpclosehandle).

For browser construction use `NewNyxBrowserResourceTransport` from
`nyx.resources.http.browser` and optionally `NewNyxBrowserResourceCache` as the
persistent provider. Abortable streaming fetch checks decoded payload bytes
before copying chunks. Nullable headers, native promise rejection and unavailable
Cache Storage become bounded diagnostics. CORS/mixed-content remain host policy.
When CORS hides `Age`, Respect conservatively fetches again; explicit Override
uses caller freshness. A host can expose `Age` for ordinary private-cache reuse.
See the [Fetch response-header rules](https://fetch.spec.whatwg.org/#cors-safelisted-response-header-name).

The callback receives a copied `TNyxResourceLoadResult`. Its enum distinguishes
embedded, network, fresh cache, stale-on-failure cache, authored fallback and
failure. `Succeeded` and `Definition` never mistake an empty/failed result for
admitted content. Error remains visible when fallback/stale content succeeds;
CacheWarning reports persistent/quota failure while valid network content remains
usable. Every cache hit/store is qualified against the requesting policy. A
failed persistent write can populate memory, and subsequent loads check that copy
before fetching again. Providers may complete inline; tokens remain stage-correct.

The receiver owns publication and request retirement. For example, inside its
`ResourceLoaded` method after checking the active view and `Succeeded`:

```pascal
LCandidate := FRuntimeResources.Clone;
LCandidate.Define(FResourceReference, AResult.Definition);
FRenderer.ReloadResources(LCandidate, FLocale, FFallbackLocale);
FRuntimeResources := LCandidate;
```

Existing selector/control admission runs before accepting the candidate catalog.
Call `FLoad.Cancel` before replacing its receiver/view or freeing it; a late
reply cannot then reach that receiver. Keep scheduling/control publication on
the UI thread. A successful file load is not a successful application build or
automatic admission of every table/label mapping. Saved declarations/fallbacks
remain independent of loaded runtime bytes and machine cache contents.

The default policy is persistent, fresh for 300 seconds, no stale reuse, at most
1 MiB, and respectful of server cache restrictions. `.Memory` selects private
memory storage; `.Bypass` disallows cache hits/stores. `.StaleFor` is opt-in
failure fallback, rather than a normal fresh hit. Durations accept 0..one year.
Fluent derivation copies policies without changing their baseline.

`.ServerPolicy(rcspOverride)` deliberately substitutes caller freshness and
allows storage despite server `no-store` in the **Nyx-managed private resource
cache**. It cannot change a browser's independent HTTP cache. Respect is the
default because `no-store` is an HTTP directive.
See [RFC 9111](https://httpwg.org/specs/rfc9111.html#cache-response-directive.no-store).

`nyx.resource.cache` separates immutable cache envelopes and policy evaluation
from `INyxResourceCacheStorage`. A loader must check `CanStore` before writing
and `StateAt` before reuse; raw storage intentionally has no request policy.
The current header boundary qualifies `no-store`, `no-cache`, `must-revalidate`,
`max-age` and `Age`. It is not a complete HTTP freshness/revalidation engine:
Date/Expires, Vary and validators/304 remain open; the resolver already selects
explicit stale-on-failure or authored fallback without inventing HTTP validation.

`NewNyxMemoryResourceCache` completes inline. Native
`NewNyxFileResourceCache` in `nyx.resource.cache.lcl` uses a versioned user-temp
directory by default, bounded UTF-8 envelopes and atomic file replacement.
Its file I/O is synchronous and requires one calling thread/provider;
cross-process quota isolation is not supplied. Browser
`NewNyxBrowserResourceCache` in `nyx.resource.cache.browser` uses private
Cache Storage asynchronously, with bounded envelopes and cancellation tokens.
Unavailable/quota/corrupt storage reports an error for explicit caller fallback.
Cache Storage needs a secure context and can be evicted; plain HTTP LAN access
must not imply availability. See the [Cache API reference](https://developer.mozilla.org/en-US/docs/Web/API/Cache).

Callbacks borrow their receiver. Retain each job and call `Cancel` before the
receiver is destroyed. Completion retires a callback once. A browser write
already submitted to Cache Storage may finish after cancellation, but cannot
publish into a document/control. Storage is bounded, without automatic eviction
or stale pruning yet. Persistent browser storage has compile evidence only.

## Studio Resources and copied proposals

Open **Resources** in Studio's Project area. The common public form is built
from ordinary Nyx controls and is consumed by both Studio controllers. Choose
New resource, an application name, a default or named locale, and one of Image,
JSON, Text or Binary. Creator title and description stay with the file. Opening
an existing variant fixes its name and locale; New creates another variant.

Import reads bounded bytes using the caller-selected kind. File extensions and
MIME never silently change that kind, and machine filenames do not enter a
design. Native import uses UTF-8 filenames and admits decoded image candidates;
browser import uses ArrayBuffer bytes and strict portable text/data admission.
Text and JSON can also be pasted. Base64 admits binary/images; an escaped JSON
string notation keeps embedded NUL text editable without losing bytes.

Preview validates proposed content and discovers structural scalar values from
JSON fields and array items. Literal dots remain part of field names. Discovery
stops at 256 choices, 2,048 visited values and depth 16. Select a supported
property on the currently selected control and a discovered value to apply the
file and its binding together. Apply rediscovers from current contents; saved
choice labels never authorize a missing or changed path. Resource bindings stay
read-only; writable state bindings retain their ordinary independent behavior.

Hosted URL mode exposes freshness, stale-on-failure, payload limit, memory or
persistent storage, and Respect/Override server policy. Import into this mode
sets an explicit same-kind fallback while keeping URL/cache choices. Preview
shows authored fallback bytes and says that network loading has not run.
Automatic application loading and runtime locale/navigation remain separate
from authoring a declaration.

Imports, previews and partial input remain copied proposals. One isolated Apply
validates the complete candidate and synchronizes crafted Pascal; one Undo
restores their pair. Exact catalog/control context guards old forms and queued
edits. Referenced removals or replacements with invalid consumer paths refuse.
Pending application Pascal must be resolved first. Private per-project
preferences retain unsubmitted content through chrome rebuilds and migrate older
versions. Late chooser replies refuse changed projects or changed form input.

Library hosts use `NewNyxResourceEditor`, typed field/action roles,
`TNyxResourceEditorDraft` and `CaptureNyxResourceEditor` from
`nyx.resources.editor`. `INyxResourcePicker` separates local file selection from
the portable form. Hosts own cancellation and check their captured context
before proposing a reply. The form borrows catalog/control inputs only while
constructing its independent owned children; it never modifies accepted work.
The prepared semantic resource API shares this form's final candidate admission.
Authenticated deployment and observing execution retain the MCP workflow owner.
Saved table row mappings and image-reference bindings are not yet exposed here.

## Semantic resource authoring

The common Resources area includes `NewNyxResourceRowsEditor`, consumed by both
ordinary Studio controllers. Open a saved relationship or enter a collection
name, choose a JSON resource, discover an array and inspect its first row. Choose
a structural text identity and add named Text/Boolean/Integer/Number fields.
Mapped fields can be edited or removed before Apply. Paths are copied descriptors;
literal dots, brackets and Unicode in JSON keys never become path expressions.
The existing collection binding inspector then attaches the named schema to a
table/list/tree. Source-backed definitions in the State panel are visibly schema
seeds with their resource origin; static editing is disabled there.

Discovery reads authored default JSON or a hosted embedded fallback, never the
network. It visits at most 2,048 values and offers at most 256 paths of up to 32
steps. First-row discovery is a convenience, not dataset admission: Apply checks
every row, identity, scalar family and retained consumer on an independent
candidate. Opening a saved recipe retains its paths even for an empty dataset.
New empty-array schemas and paths outside bounded discovery can currently be
authored through the fluent API/MCP and then opened in the form; a richer manual
path builder remains open. No discovery count is a performance qualification.

Per-project presentation version 9 retains unsubmitted mappings and partial text;
older versions retain their original strict shapes. Changed catalog/collection
context refuses restoration before writing fields. Apply uses the ordinary
isolated paired source processor, pending-draft guard and Undo/Redo. Static rows
require the explicit conversion checkbox. **Keep rows and detach** materializes
authored default/fallback data, retaining schema/key/control bindings; it does not
copy application edits or delete the resource. Both actions restore with one
paired Undo. The form proposal is copied through the existing resource command;
its row branch has a closed version-2 descriptor and exact baselines.

Studio's full canvas replacement explicitly requests `nrmAuthoredDefaults` from
`TNyxResourceRenderMode`. Runtime renderer calls default to `nrmConfigured` and
keep their accepted catalog/locale. Both adapters stage the chosen context before
retiring a mount. This fixes stale design captions after authoring while retaining
running application resource lifetime; an admission/constructor failure does not
change the previous context. Retained refresh still checks document context.

Current source exposes `nyx_resources` through the ordinary MCP dispatcher.
Queries preserve selection, accepted source and history. Every response includes
the revision; exact variant queries require both `name` and `locale` (empty
locale means the default variant). Authored modes never fetch a URL or inspect
runtime cache contents. Runtime modes below inspect separately enrolled copied
reports; they do not read cache payloads or acquire application handles.

| Mode | Bounded context |
| --- | --- |
| `list` | 8 variants by default, at most 16; metadata/title previews without payloads; case-sensitive search of names, titles and creator help |
| `details` | Exact kind, hosted URL/cache declaration, fallback presence and title/help windows of at most 1,024 Unicode scalars |
| `content` | Text/JSON source windows of at most 4,096 Unicode scalars, or image/binary windows of at most 4,096 bytes |
| `json` | Exact structural path; at most 16 immediate children with 80-scalar previews, or one exact scalar/text window |
| `bindings` | Supported properties and local/effective descriptors for an authored owner, including resource selectors and inheritance |
| `sources` | 8 saved collection/resource relationships by default, at most 16; case-sensitive collection/resource filtering without payloads |
| `rows` | Exact collection source/array/identity plus a page of typed field names/families/paths (8 default, at most 16); runtime rows excluded |
| `apply` | 1..32 typed resource/binding/row-source changes, one final paired publication and Undo checkpoint |

JSON field names are literal array steps: `["literal.dot", 0]` addresses that
field and its first item. Child pages return usable structural paths, without
dumping descendants. Numeric leaves preserve their original decimal token;
text windows preserve supplementary characters and NUL. Binary/image windows
are independently encoded Base64: concatenate the **decoded bytes**, rather
than their padded Base64 strings. Hosted payload windows explicitly identify
`authored-fallback`; a declaration without fallback refuses payload access.

Mutation operations are `define`, `remove`, `bind`, `clear-binding`,
`inherit-binding`, `define-rows` and `detach-rows`. Definitions use the existing strict embedded version 1 or
hosted version 2 contract. Bind carries the existing five-field resource selector
and an enum property name at this wire boundary. Pascal callers use
`NyxDefineResource`, `NyxBindResource` and `NyxResourcePatch` from
`nyx.studio.resourceedits`, with typed references, locale, property and selector.
Studio's copied form proposal consumes the same candidate implementation while
retaining its exact catalog/control baseline guards.

`define-rows` requires `collection`, a closed version-one `source` recipe and
Boolean `replaceStatic`. The source has `version`, `resource`, structural array
`path`, text `identity` path and 1..64 ordered `{name,type,path}` fields. Existing
static collections require explicit true consent; saved sources can be updated.
Pascal callers use `NyxDefineResourceRows` with `TNyxResourceRows` and typed
collection references. `detach-rows` requires only `collection`; Pascal uses
`NyxDetachResourceRows`. Define the related file and recipe in one group, including
when the recipe appears first. Final consumer admission owns the complete group.
`nyx_collections` marks source-backed authored queries `resource-schema-seed`
with their resource key; zero seed rows are never reported as loaded runtime data.

Put file replacements and all dependent selector repairs in one group. Its
final retained consumers must admit; an invalid final path, missing owner or
unsupported scalar/property refuses the complete group. Independent definition
budgets still apply as operations run. Clear masks reusable inheritance; inherit
removes the local descriptor. MCP additionally requires Allow edits, current
`expectedRevision`, a transport-scoped `operationId`, and no pending Pascal.
Success/refusal appears in ordinary activity; exact retries return their receipt.

An `op: "resources"` group also participates in `nyx_transaction` beside
design, state and collection groups. The transaction retains its 64 total leaf
limit and one paired Undo step. Each ordered group must admit before the next;
resource changes within one group validate their final consumers together.
Workspace/review routing, authority and recovery use the ordinary MCP engine.

The maintained `resource-workflow` gate qualifies its **suspended** actual
engine through public semantic dispatch in a new private runtime, plus exact
compiled emitted source and browser compilation. It starts no listener and
does not authenticate HTTP or deploy tools into the protected running server.
Automatic cross-process observation has separate actual HTTP qualification below.
Runtime reload/cancellation, hosted media and deployed ordinary Studio/phone
qualification remain open.

## Runtime resource observations

Authored declarations and application reports have separate lifetimes.
`NyxApplicationResourceDiagnostics(Application.Resources).CaptureRuntime` captures
immutable typed entries on the application's UI thread. The optional capability
preserves the original resource-owner interface. Snapshots safely outlive that
owner, retain exact declaration identity, and contain no scheduler, transport,
control or document reference. Reading a snapshot never initiates I/O.

Each entry separates the latest attempt's phase/origin/cache read/write/error
from `HasPublishedLoad`, `PublishedOrigin` and the installed cache read/write
tiers. Initial authored defaults and embedded fallbacks have no published load.
A rejected, queued or cancelled reload can retain a previous installed load.
Installed evidence exchanges with the catalog before resource/control/store
observers run. Notification errors describe a receiver failure after publication.
Summary cache counters count variants' latest attempts, not cumulative I/O.

`TNyxResourceCacheUse` records successful Nyx-managed operations: none, memory or
the injected persistent provider. Requested policy is reported separately.
Persistent failure followed by a successful memory write reports memory plus
the warning. Respected server `no-store` reports no write; explicit caller override
still uses the existing policy. The browser's independent HTTP cache is never
inferred from these fields. No freshness/storage policy was changed by reporting.

The trusted Studio host can enroll an exact run through
`ObserveResourceRuntime`, publish copied snapshots through
`PublishResourceRuntime`, and retire it through `RetireResourceRuntime`.
Distinct `TNyxStudioRuntimeRef` names and the typed application/view/resource-only
scope identify the consumer. Enrollment requires an exact accepted design
revision, matching complete authored declarations, a concrete target and no
pending draft. A view scope must name one root; other scopes omit a view.
The returned observation record has no wire codec or public token property.
It retains no application and grants no reload/cancel operation.

Eight runs bound membership. An inactive report can yield capacity to a new
distinct run without changing the design. A stopped final snapshot retires its
publication authority; explicit retirement keeps the last report inactive.
A paired design revision revokes all reports/tickets. In-process rollback copies
preserve detached metadata and immutable snapshots; durable recovery never
inherits runtime authority. Publishers must reacquire the current session owner
under its lock after a host rollback, and capture on the application's UI thread
before publishing. Trusted publishers attest the mounted scope; the broker does
not infer a successful full application from a resource-only preview.

`nyx_resources` adds two read-only modes:

| Mode | Context and authority |
| --- | --- |
| `runtimes` | Current `expectedRevision`; at most eight thin run/scope/target/sequence/active summaries |
| `runtime` | Current `expectedRevision`, exact `run` and `expectedSequence`; 8 entries by default, at most 16; exact `nextOffset` |

Pages exclude resource payloads and hosted URLs. Adapter diagnostics can contain
addresses or paths; each is clipped at 512 complete Unicode scalars. A 40 KiB
item-JSON budget can shorten a requested page, preserving progress and exact
pagination. A stale report sequence refuses rather than mix observations from
different captures. Public MCP can neither fabricate enrollment nor acquire a
runtime mutation handle.

Private editor observation carries only bounded summaries. The Resources area
in both Studios consumes the public `NewNyxResourceRuntimeView` compound card
through a strict typed summary codec. Empty reports visibly mean no host has
shared evidence; they never imply successfully loaded authored resources.

### Automatically launched Studio previews

The ordinary preview controllers request a private grant after rechecking a
successful current compiler job, accepted pair, output profile and revision.
They negotiate this capability; an older server retains ordinary preview launch.
Grants bind one exact workspace/job/target/application or page/reusable root and
do not enter public MCP status, exported companions or design properties.
Browsers receive context in the fragment; native launch changes only its owned
child environment. The Studio-generated wrapper consumes
`ObserveNyxStudioResources(Application.Resources)`; ordinary exported programs
retain no reporter dependency or credential. Refused optional reporter context
leaves the application usable.

`EncodeNyxResourceRuntime` and `DecodeNyxResourceRuntime` define a bounded
version-one status wire. Decoding requires complete exact trusted declarations;
the sender cannot substitute a source, policy, kind or variant membership.
Snapshots exclude resource bytes and declared URLs. The producer request ceiling
is 2 MiB, distinct from small paged semantic responses. Adapter diagnostic text
can still contain addresses/paths.

The UI thread captures state. Each adapter retains one exact in-flight request
until its matching acknowledgement; unchanged accepted captures send only a
heartbeat. The locked host reacquires current workspace owners and treats exact
delivery retries idempotently. Heartbeats extend liveness without changing the
semantic resource sequence or generating activity for an unchanged resource.
The native byte worker has bounded transport/cancellation; the browser uses
same-origin XHR with a private header. No global credentials or cache policy
change is needed.

The current host expires authority after sixty seconds without an accepted
delivery. Explicit retirement is best effort during shutdown; expiry covers
abrupt process loss. A design revision, pending draft or closed workspace
revokes/refuses the old context. At most sixteen grants exist globally, eight
live per project. Retired receipt tombstones keep their global slots until
expiry so an exact retirement retry remains safe. Producer retries do not extend
an expired lease. Durable recovery never restores these grants.

Run `tools/build.ps1 -Target resource-runtime` to compile the maintained Pascal
server/HTTP tools and both real compiler wrappers without starting a listener.
Actual HTTP qualification additionally takes `-ResourceRuntimeHome` and
`-HttpURL` for an explicitly owned isolated server started by
`nyx_resource_runtime_server`. It writes an origin-bound qualification marker;
the client refuses ordinary Studio homes before any claim/commit. Keep its
runtime and enrollment separate from user projects. The test uses semantic MCP
for composition/builds and read-only DOM protocol for rendered validation.

Actual HTTP qualification passes **55** checked assertions, leak-free, across
six separately launched browser/Win32 application, page and reusable previews.
It qualifies real hosted binding/cache evidence, unchanged heartbeats, observing
editor packets, real lost-process expiry and both-target nonfatal diagnostic
refusal. Existing application/common Studio checks pass **81**; suspended
semantic/protocol and exact-source checks pass **76/7**. Both Studios/backend/
worker compile with zero owned warnings; upstream warnings remain untouched.

The subsequent ordinary Studio lifetime journey passes **29** leak-free checks.
The public browser persistent mount retains the actual compiled child document,
typed input and exact active reporting run through editor polling, source modal,
compact panel hiding/return and desktop restoration. An accepted semantic edit
retires the old frame/authority; a new application build starts a new document
from authored defaults. This qualifies page/application launches inside the real
browser editor, extending the separate six-producer qualification above.

Frozen `960134f` is subsequently installed through the existing LAN executable/
runtime/enrollment, with all nine exact pairs/full history intact and 22 actual
authenticated MCP tools. Semantic runtime queries work on that installed service;
the ordinary observer reads its exact companion and switches modal/compact/
desktop presentation without modifying the primary. Same-host LAN bytes match
the 299-file sealed payload.

Physical phone, persistent browser cache/CORS/security contexts, background
restoration, transformed mount geometry,
accessibility/tab order and full runtime reload/cancel remain unqualified under
their original owners. A small observing journey does not establish complete
editor/parity quality. Semantic exact-job editor launch/adopt remains a recorded
workflow gap; this fixture's trusted operator buttons exercise the controller.

## Wire, limits and qualification

Document wire version 8 carries a nonempty resource catalog and typed scalar
selectors. Saved row recipes select version 9 and collection descriptor version
2; every entry then carries an explicit source descriptor or null. Recipe
version 1 strictly retains resource/path/identity/ordered typed fields. An empty
schema seed must exactly match its recipe. Older document/descriptor versions
cannot silently admit source metadata. Static designs and empty catalogs retain
the existing version selection. Embedded
definitions use strict version 1; hosted definitions use version 2. Older opaque
`resources` extensions retain their meaning and refuse conflicting promotion.
Machine cache locations and cache contents are never exported with a design.

Resources admit at most 1 MiB of packed content per file and 128 named/locale
entries under a 3 MiB catalog wire budget. Text/JSON require strict UTF-8;
binary bytes, supplementary Unicode, embedded NUL and original numeric JSON
spellings survive persistence. A control's own text restrictions still apply.
Source recipes share the existing 64-collection/64-field admission and aggregate
8 MiB collection-default payload budget, including their serialized metadata.
Resource paths retain the existing maximum 32 structural steps. Overall document
JSON admission remains bounded independently.

Run `tools/build.ps1 -Target resources`. Checked shared/actual Win32 qualification
passes 78 checks and the exact emitted builder passes eight, leak-free. Both
browser counterparts and ordinary Studios/backend/worker compile with zero owned
warnings; browser controls/cache/phone execution is not established by compilation.
The subsequent common Studio Resources form supplies import/proposals/scalar
binding choices; complete authoring and semantic resource operations remain open
under their existing task owners.

Run `tools/build.ps1 -Target resource-loading` against an existing Studio health
endpoint (`-HttpURL` changes only this qualification configuration). It starts no
listener/browser and edits no active project. Current shared/actual Win32 checks
pass 31, leak-free: real HTTP/HTTPS bytes, caption/prompt publication, persisted
cache restart/quota fallback, queued deadline, cancellation and worker retirement.
The matching browser program stages an actual same-origin/control/cache journey,
without claiming execution. Current browser/CORS/persistent-cache/phone execution,
in-flight native cancellation timing, negative TLS fixtures, redirects/compressed
responses and other native systems remain unqualified. Existing foundation
evidence remains applicable. The request deadline covers HTTP transport, including
worker queuing; cache-provider work has no whole-load timeout yet.

Run `tools/build.ps1 -Target resource-authoring` for the common form and ordinary
native Studio consumer. Current shared/Win32 checks pass 78, exact emitted Pascal
seven and workspace regression 252, leak-free. Both Studios/backend/worker and
matching browser counterparts compile with zero owned warnings. Desktop and
390-pixel native captures were inspected. These are independent public-library
fixtures: the deployed MCP still lacks resource operations. Actual browser,
trusted chooser, phone and observing execution remain required; compilation and
native synthetic input do not establish them. Complete resource authoring,
application loading and both-target parity retain their existing task owners.
