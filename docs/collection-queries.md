# Typed collection queries

[Collection views](collection-views.md) · [Stores](collections.md) ·
[Current integration evidence](../WORK.md#current-return-path-typed-collection-queries--2026-10-07)

`nyx.collections.query` supplies immutable filters and stable ordering for bound
lists, tables and trees. The portable contract contains no DOM/LCL types, locale
dependency or executable predicate callback. Application fields remain distinct
typed references; comparisons take Boolean, Integer, Double or `TNyxText` values.

```pascal
uses nyx.collections, nyx.collections.query;

var
  LTitle: TNyxTextFieldRef;
  LComplete: TNyxBooleanFieldRef;
  LPriority: TNyxIntegerFieldRef;
  LPolicy: TNyxCollectionQuery;
begin
  LTitle := NyxTextField('title');
  LComplete := NyxBooleanField('complete');
  LPriority := NyxIntegerField('priority');

  LPolicy := NyxCollectionQuery
    .Where(
      NyxWhere(LComplete).EqualTo(False)
        .AndAlso(NyxWhere(LPriority).AtLeast(2)))
    .OrderBy(LPriority, nsdDescending)
    .ThenBy(LTitle, nsdAscending, nqtAsciiInsensitive);
end;
```

`INyxCollectionPredicate` is an immutable managed interface. `AndAlso`, `OrElse`
and `Negated` compose independent trees. A field facade returns the matching
predicate family; it cannot apply text search to an Integer or numeric ordering
to a Boolean. `Where` replaces a query's filter. `WithoutFilter` clears only its
filter; `Unsorted` clears only ordering. `OrderBy` starts a new ordering and
`ThenBy` adds a distinct secondary field. Secondary ordering without a primary,
duplicate sort fields and default/uninitialized sort references refuse.

| Field family | Predicate methods |
| --- | --- |
| Text | `EqualTo`, `NotEqualTo`, `Contains`, `StartsWith`, `EndsWith` |
| Boolean | `EqualTo`, `NotEqualTo` |
| Integer / Number | `EqualTo`, `NotEqualTo`, `LessThan`, `AtMost`, `GreaterThan`, `AtLeast` |

Text uses Unicode scalar ordering/search. `nqtExact` is the default;
`nqtAsciiInsensitive` folds A–Z only. Other scripts, accents and combining
sequences remain exact. Neither mode normalizes text or promises locale-aware
dictionary ordering. Contains/prefix/suffix accept an empty needle. Numeric
comparison uses the admitted scalar family and never parses display text.
All four field families support `NyxSort`/`OrderBy`/`ThenBy`; Boolean orders
False before True, and Numbers retain the shared finite-Double contract.

## Authored defaults and independent runtime policies

An authored binding can include a reusable default:

```pascal
LTasksTable.Binds
  .Collection(
    NyxCollectionView(NyxCollection('tasks'))
      .Column(LTitle, 'Task', cmEditable)
      .Column(LPriority, 'Priority', cmEditable)
      .Selection(nsmMultiple)
      .Query(LPolicy))
  .Done;
```

A live `INyxCollectionView` copies that default. `ConfigureQuery` fluently
returns the view after admitting a complete policy. It changes runtime view
state, not document defaults, the source store's order or source revision:

```pascal
LTasksView.ConfigureQuery(
  LPolicy.Where(NyxWhere(LTitle).Contains(LSearchText, nqtAsciiInsensitive)));

// Restore the complete source without clearing selected item identities.
LTasksView.ConfigureQuery(NyxCollectionQuery);
```

Different views over the same store have independent policies. The result
snapshot retains stable item references and the actual source revision; a query
change can produce a different result at the same revision. Adapters must use
snapshot identity/order, not source revision alone, when refreshing controls.
Result lookup remains indexed. The projection retains the source, so `DataBytes`
reports retained source payload rather than discounting hidden rows.

Equal ordering keys retain source order, including descending sorts. Multiple
keys use a stable merge with cached typed scalar keys. Tree filtering includes
matching rows and their required ancestors; it does not include arbitrary
descendants of a matching parent. Parent indexes are remapped into the result,
and child-before-parent sources remain valid. Cycles or missing parents refuse.

Filtering retains selected source identities. Only a source removal prunes them.
An invisible keyboard focus moves to a nearby visible row; an empty result has
no focus. Anchors can remain hidden. Focus/toggle/additive range preserve hidden
membership, while replacement range and `SelectAll` replace membership with the
admitted result. `SetSelection` can include hidden members but requires visible
focus. `Selected` can name a hidden member; `CellText` still reads accepted text
for that source identity. Range positions use query order, not mutable source-store positions.

Sorting preserves an active unchanged scalar editor's item, draft and caret on
both adapters. Hiding its item ends editing and discards that uncommitted draft.
An accepted scalar change normalizes the editor instead of preserving stale text.

## Ownership, admission and persistence

Query records explicitly copy their sort arrays. They retain a closed immutable
predicate descriptor because pas2js cannot put COM interfaces in record fields.
`Filter` returns a separately owned interface tree; an evaluator retains it once
for the complete evaluation. The library projector does not parse a predicate
per row. External predicate implementations are normalized through `ToData`,
so attached policies retain no external callback/owner or reference cycle.

Wrong schema field families and missing fields reject before a view changes.
Invalid policy admission leaves its prior query, snapshot, selection and observer
count intact. View mutation is confined to the UI thread; reentrant mutations
refuse. Query/selection notifications have nil collection `Changes`; source
publications still carry their complete immutable change log. Observer failure
is reported after publication under the existing collection notification contract.

The closed wire boundary allows at most 64 predicate nodes, depth 16, eight
distinct sort fields and 65536 canonical UTF-8 bytes. Unknown members/operators,
inappropriate comparison families, unsupported enum names, wrong scalar types
and noncanonical empty objects refuse. A null query means an absent policy.

Query-free view descriptors retain their exact version-1 single/version-2
multiple encoding. A nonempty authored query uses view descriptor version 3,
with explicit selection and query fields. The outer document version remains
unchanged. Generated Pascal uses the public typed factories and readable logical
blocks; its admitted and ordinarily compiled builders reproduce the design.

`tools/build.ps1 -Target collection-query` checks the portable codec/source
contract, compiles its generated builder, runs actual Win32 controls using the
previous authenticated MCP companion, and stages both browser consumers. Supply
`GridSourceDirectory` for that existing export. Execute `collection-query.html`
and `collection-query-controls.html` over HTTP for browser evidence. This command
starts no listener and changes no project or Codex enrollment.

The current packet qualifies library policies, persistence/source and actual
controls. A Nyx-built query authoring form, authenticated current-backend semantic
query edits and an observing Studio journey remain required integration. The
existing deployed/frozen services are unchanged. Paging, virtualization, broader
large-data budgets, production styling and hardware/IME/assistive-technology
qualification keep their original task owners.
