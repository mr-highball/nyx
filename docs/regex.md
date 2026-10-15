# Portable regular expressions and text Logic

[Logic collection](logic.md) · [Logic owner](../TODO/NS-3_logic-components_01.md) ·
[Current evidence](../WORK.md)

Nyx uses the same Pascal parser, compiled instructions and scalar-based matcher
on FPC and pas2js. It does not dispatch to JavaScript RegExp or a native system
matcher. Pattern syntax is an explicit text language boundary; case, extent,
replacement parts, capture references, missing values and budgets use Pascal
types. This is a defined Nyx grammar, not a claim of complete compatibility with
another regex engine or Unicode Technical Standard #18.

## Direct use and ownership

```pascal
var
  LOptions: TNyxRegexOptions;
  LReplacement: TNyxRegexReplacement;
  LFormatter: INyxRegexReplace;
  LNames: INyxRegexExtract;
  LFormatted: TNyxText;
  LRows: TNyxDataValue;
begin
  LOptions := NyxRegex('^(?<first>[\p{L}\p{M}]+)[ \t]+(?<last>[\p{L}\p{M}''-]+)$')
    .CaseMode(nrcUnicode)
    .Multiline(True)
    .Scan(nrsAll);
  LReplacement := NyxRegexReplacement
    .Capture(NyxRegexGroup('last'))
    .Literal(', ')
    .Capture(NyxRegexGroup('first'));
  LFormatter := NewNyxRegexReplace(LOptions, LReplacement, nruError);
  LNames := NewNyxRegexExtract(LOptions, NyxRegexCapture(NyxRegexGroup('last')), nrxSkip);
  LFormatted := LFormatter.Replace('Ada Lovelace' + #10 + 'Grace Hopper');
  LRows := LNames.Extract('Ada Lovelace' + #10 + 'Grace Hopper');
end;
```

Import `nyx.regex.types`, `nyx.regex.replacement` and `nyx.logic.regex` for that
example, plus the ordinary text/data units. `NewNyxRegex` in `nyx.regex` returns
`INyxRegex` for direct `IsMatch`, `First` and `Find` calls. `NewNyxRegexMatch`
returns the specialized Logic interface, whose `Find` returns the same typed
snapshot. Compilation occurs at construction, including validation of named or
numbered replacement/extraction selectors even when an input would not match.

Compiled code and Unicode tables are immutable. Each call owns its cursor,
captures, alternatives, counters, budgets and optional cancellation scope.
Options, templates, capture records and complete match collections retain no
document, view, global last-match cursor or borrowed application input. Derived
replacement recipes detach their parts before modification. Separate calls can
share the managed compiled interface; the supplied cancellation scope must obey
its own scheduler/thread contract.

Capture zero is the complete match. Positions are zero-based Unicode **scalar**
offsets and lengths, independent of native UTF-8 bytes and browser UTF-16 units.
They are not grapheme indices or direct target-control caret offsets. A missing
capture has an undefined span; a participating empty capture has a defined
zero-length span. Malformed Unicode refuses rather than replacing characters.
All-match scans are leftmost, nonoverlapping and advance one scalar after an
empty match; a final empty match may occur at end of input.

Replacement text is structural. `.Literal('$1')` inserts those exact characters;
`.Capture(1)` inserts group one. The default initialized empty recipe deletes
matched spans; an uninitialized record refuses. An unmatched capture uses the
explicit `nruEmpty` or `nruError` policy. Unknown selectors always refuse.

## Supported grammar

| Feature | Nyx meaning |
| --- | --- |
| Literals, `.`, concatenation, `a\|b` | Scalar matching with ordered alternatives |
| `[a-z]`, `[^...]`, nested sets | Unions, ranges, complements, nested `&&` intersection and `--` difference |
| `\d`, `\s`, `\w` and uppercase complements | Unicode properties by default; `CharacterClasses(nrgASCII)` selects ASCII shorthands |
| `\p{...}`, `\P{...}` | General Category abbreviations/long names/major groups, `Assigned`, `Any`, `ASCII`, `WhiteSpace` |
| `(...)`, `(?:...)` | Numbered capture and noncapturing group |
| `(?<name>...)`, `(?'name'...)`, `(?P<name>...)` | Unique named captures; names use admitted Unicode letters/marks/digits and underscore |
| `\1`, `\k<name>` | Numbered/named backreferences; unmatched references fail the match |
| `*`, `+`, `?`, `{m}`, `{m,}`, `{m,n}` | Greedy by default; trailing `?` is lazy, trailing `+` is possessive |
| `(?>...)` | Atomic group; completed internal alternatives cannot be revisited |
| `(?=...)`, `(?!...)` | Positive and negative lookahead |
| `(?<=...)`, `(?<!...)` | Positive and negative lookbehind with a fixed scalar width |
| `^`, `$` | Strict input boundaries by default; `Multiline(True)` admits line boundaries without splitting CRLF |
| `\A`, `\z`, `\Z` | Input start, strict end, or end before one final line break |
| `\b`, `\B` | Boundaries under the selected word-class policy |
| `(?i)`, `(?m)`, `(?s)`, `(?x)` and scoped forms | Lexical case/line/dot/expanded flags; `-` disables a flag |
| `(?#...)`, expanded `#` comments | Pattern comments; expanded whitespace/comments apply outside quoted text/classes |
| `\xHH`, `\uHHHH`, braced scalar escapes | Valid scalar values; a nonbraced UTF-16 surrogate escape pair may represent one supplementary scalar |
| `\Q...\E` | Literal quoting outside classes; a following quantifier binds the last quoted character |

Defaults are exact case, all matches, Unicode shorthands, no multiline, no DotAll
and no expanded spacing. `nrcASCII` folds ASCII A–Z only; `nrcUnicode` applies
pinned Unicode default **simple** case folding. It can match supplementary letters
and case equivalence such as Kelvin sign/long s, but never expands sharp s into
two letters. No implicit normalization, locale tailoring or full case expansion
occurs. Unicode word characters include letters, marks, decimal digits,
connector punctuation, ZWNJ and ZWJ. Explicit property escapes remain Unicode
when ASCII shorthands are selected. Caseless class membership and word boundaries
apply the selected case-mode closure to those base sets; choose `nrcASCII` when
both folding and shorthand membership must stay ASCII.

Unsupported/malformed syntax refuses at construction. Variable-width lookbehind,
grapheme matching, normalization, locale-specific matching, recursion,
conditionals, Unicode scripts/script extensions and the broader binary-property
catalog remain open. No unsupported expression silently falls back to a host
engine. See [UTS #18](https://www.unicode.org/reports/tr18/) for the broader
standard; the supported table above is the current Nyx contract.

## Limits and cancellation

`NyxRegexLimits` creates initialized, caller-tunable positive limits:

| Fluent option | Default |
| --- | ---: |
| PatternScalars | 16,384 |
| InputScalars | 1,048,576 |
| Instructions | 65,536 |
| WorkSteps | 8,000,000 |
| BacktrackStates | 16,384 |
| Matches | 65,536 |
| ResultBytes | 16,777,216 |
| Groups | 128 |
| Nesting | 64, with a hard implementation ceiling of 256 |

For a pattern intentionally larger than the default, pass the limits to
`NyxRegex(LPattern, LLimits)` before pattern admission. Default records refuse.
Generated Pascal emits the complete typed limits before the pattern and then
its explicit options; a larger admitted pattern can therefore reconstruct.

Search work is shared across the complete input, candidate positions and nested
assertions. Backtracking uses explicit alternative states; simple greedy scalar
repeats retain shorter prefixes compactly. This is a bounded backtracking engine,
not a general linear-time guarantee. Limits are operation budgets, not elapsed
time deadlines. Cancellation is cooperative and checked during charged work.
Native worker calls can observe a concurrently cancelled scope. Browser
asynchronous flow work uses the existing event-loop queue; an individual matcher
call is synchronous and does not yield to another UI event midway through that
call. UI cancellation can retire queued/publication work, while the current call
finishes or exhausts its budget. A yielding/worker-backed browser matcher and
physical cancellation responsiveness remain separate scheduler/quality work.

`ResultBytes` conservatively budgets capture text, escaped JSON and metadata;
it is not an exact process-heap cap. Replacement/extraction/projection have their
own complete transformation phase under the configured work/byte limits after
matching. `IsMatch` avoids result capture materialization and its result-byte
budget. `ENyxRegexLimit` and `ENyxRegexCancelled` differ from an ordinary no-match.
A failed operation returns no partial result. Logic translates cancellation into
the existing flow cancellation outcome; application routes retain their prior
state/rows under the ordinary atomic publication guards.

## Studio, source and semantic use

The default Logic collection appends Match, Replace and Extract without reordering
existing kinds. Match emits JSON captures plus Boolean `matched` and integer
`count`; Replace emits text plus those status ports. Extract emits Table-ready
rows with `id`, `matched`, nullable `value`/`start`, `length`, `capture` and `name`.
`nrxNull`, `nrxSkip` and `nrxError` govern unmatched extraction captures. Count
describes matched spans, including empty matches, rather than retained row count
or whether replacement text changed.

Studio consumes the public `NewNyxRegexEditor` compound. Its typed controls expose
pattern, flags, captures, ordered literal/capture replacement parts, confirmation
before removal, missing-value policies and a collapsed limits section. The form
is authored before realization; input does not insert design children into a
runtime view. General deferred compound mounting remains a separate authoring/
performance gap. Version-11 Logic drafts wrap the existing versions 1–10 with
exact unfinished regex fields and replacement parts, including malformed text.
Restoring a draft does not compile or execute its pattern.

Crafted Pascal uses specialized interfaces/factories and enum/fluent options.
Closed persistence and bounded semantic step edits resolve through the same
constructors and final-graph admission. Current-source qualification uses an
owned local semantic broker with expected revisions, grouped publication, exact
retry and paired Undo/Redo. The protected installed MCP schema/release is separate;
this work does not claim an authenticated observing rollout.

## Unicode data and repeatable qualification

The checked-in Pascal include is generated from Unicode **17.0.0**
[UnicodeData.txt](https://www.unicode.org/Public/17.0.0/ucd/UnicodeData.txt) and
[CaseFolding.txt](https://www.unicode.org/Public/17.0.0/ucd/CaseFolding.txt).
The generated include retains the complete
[Unicode License V3](https://www.unicode.org/license.txt) notice. Handwritten
library/tools retain Nyx's MIT attribution. No dependency source is edited.
The generator accepts only the
pinned data bytes and emits 4,144 category ranges and 1,512 simple fold mappings
plus reverse lookup data. See the pins in `tools/nyx_unicode_regex.lpr`.

`tools/test-logic-regex.ps1 -EvidenceDirectory <fresh-directory>` runs native
core/source/semantic, actual compiled application and earlier Logic regressions.
Supply `-UnicodeDataDirectory <directory>` containing those two exact data files
and `license.txt` to reproduce the include byte-for-byte and independently compare
every valid scalar's category/folding: 2,224,128 comparisons. The shared browser
fixture also checks all-scalar fingerprints; fingerprints supplement the exact
native oracle and are not cryptographic evidence. Supplying the preflighted
`-StaticJobsDirectory` and `-BuildsBaseURL` adds actual desktop/CSS-390 HTTP browser
core/application/editor checks using the Pascal browser driver and an existing
listener. The tool does not start or replace the observing server.

WORK.md records final receipts, corrected failures and the qualified boundary.
Full regex conformance, arbitrary-pattern performance, physical input, broader
widgets/browsers, complete Logic breadth and preserving delivery remain open.
