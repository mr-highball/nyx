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
implicitly. Application navigation/remount retention and application-wide locale
publication still need integration; view reload alone does not establish them.

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

`Read` and `Reload` are explicit runtime operations. Automatic saved row recipes,
generated recipe replay, reusable mapping scopes and a combined scalar/table
transaction are still open. Generated collection defaults currently preserve
their materialized rows, without inventing an automatic resource relationship.

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
this example still does not fetch its URL. Automatic application-wide resolution,
navigation/scopes and the common Studio workflow remain open.

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

## Wire, limits and qualification

Document wire version 8 carries a nonempty resource catalog and typed scalar
selectors. Empty catalogs retain the existing version selection. Embedded
definitions use strict version 1; hosted definitions use version 2. Older opaque
`resources` extensions retain their meaning and refuse conflicting promotion.
Machine cache locations and cache contents are never exported with a design.

Resources admit at most 1 MiB of packed content per file and 128 named/locale
entries under a 3 MiB catalog wire budget. Text/JSON require strict UTF-8;
binary bytes, supplementary Unicode, embedded NUL and original numeric JSON
spellings survive persistence. A control's own text restrictions still apply.

Run `tools/build.ps1 -Target resources`. Checked shared/actual Win32 qualification
passes 78 checks and the exact emitted builder passes eight, leak-free. Both
browser counterparts and ordinary Studios/backend/worker compile with zero owned
warnings; browser controls/cache/phone execution is not established by compilation.
The common Studio Resources area, import adapters, binding picker and semantic
resource operations remain open under their existing task owners.

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
