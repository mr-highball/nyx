# Pinned Unicode case-fold data

Nyx's portable typeahead uses the official Unicode 17.0.0
[CaseFolding.txt](https://www.unicode.org/Public/17.0.0/ucd/CaseFolding.txt).
The exact 87,539-byte input has SHA-256
`ff8d8fefbf123574205085d6714c36149eb946d717a0c585c27f0f4ef58c4183`.
Its default full mappings are the `C` and `F` entries; `S` and Turkic `T`
entries are excluded. Unlisted scalars map to themselves. Folding does not
normalize text, remove accents or modify stored labels.

This is a data dependency, with no package or runtime dependency. The original
data file and [Unicode License V3](17.0.0/LICENSE.txt) remain unmodified.
The generated Pascal table embeds provenance and the full Unicode license;
Nyx's generator and search code retain the project's MIT attribution.

Compile `tools/nyx_unicode_casefold.lpr` with FPC in Delphi mode and `-Fusrc`,
then supply the pinned data, its license and an output include path, in that
order. The tool refuses an input with a different byte identity and validates
scalar order and expansion bounds. Its MD5 check is a reproducibility check,
not a security claim. It generates 1,585 mappings. The maintained
`tools/build.ps1 -Target typeahead` compares the generated include's SHA-256
with `src/nyx.text.casefold.inc` before compiling either target.

A Unicode version change needs an explicit new input, license/provenance,
generator pin and both-target qualification. It must not silently download
moving data during an application build.
