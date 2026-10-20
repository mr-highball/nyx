# Interface identity corrections

[Current work](../WORK.md) · [Architecture](architecture.md)

Pascal capability queries identify an interface by its GUID. Two incompatible
method layouts must never share that identity: a successful query could return
an unrelated vtable and turn an ordinary optional-capability check into a wrong
method call. The October 20 source review found four such pairs among 600
explicit production interface declarations in Pascal units, or 688 including
the maintained includes.

The following four identities are corrected deliberately. Their type names,
inheritance and methods remain unchanged. The primary interface listed in the
last column retains the old identity.

| Corrected interface | Old GUID | New GUID | Unrelated interface retaining old GUID |
| --- | --- | --- | --- |
| `INyxPascalLexicalObserver` | `98349E7C-C0ED-48DE-A518-427A04570B02` | `031EA0B8-3AD2-4E13-9CEA-5B067B7B8D67` | `INyxPascalSyntaxSnapshot` |
| `INyxPascalDeclaredTypeSyntax` | `98349E7C-C0ED-48DE-A518-427A04570D02` | `C5E05E21-14B3-49DF-8734-7DF63996E953` | `INyxPascalTranslatedSyntax` |
| `INyxPascalUnresolvedConditions` | `98349E7C-C0ED-48DE-A518-427A04570C14` | `80AB2615-589C-43A3-94AD-AB0D6EAB438F` | `INyxPascalSemanticTranslation` |
| `INyxBrowserToolCanvasView` | `D1411014-6120-4C84-A708-000000000022` | `BF5DBFA7-818D-4E03-964F-0527BF7F5D73` | `INyxJSONFilter` |

Rebuild Nyx and every dependent Pascal unit, extension and browser bundle
together. Source calls retain their signatures; already compiled callers of a
corrected capability must not be mixed with the repaired producers. Custom
implementations should use the interface declaration's new identity. Handwritten
IID tables must update the corresponding entry. There is no ambiguous old-IID
alias: accepting one would preserve the same unsafe query.

A unique GUID establishes a method contract. It does not prove that an alternate
implementation owns immutable source, physical include origins, a compiler
capture, a document or a renderer. Existing private ownership proofs, explicit
leases, cancellation and admission boundaries still apply.

The focused consumer uses production syntax, type, translation and collection
objects, plus an ordinary counting lexical visitor. The first three pairs are
queried in both directions before any optional method is called. The browser
case queries a real JSON filter for the unrelated view capability. It does not
mount a browser canvas or test that reverse direction. Native execution passes
34 checks with a clean heap; the portable consumer compiles. The frozen source
inventory contains 693 explicit interface identities, with zero collisions:
684 prior identities stay exact, four are corrected and five new optional
documentation capabilities are added. The common source rebuild passes 63
compiler jobs, including 30 native test programs with clean heaps, both Studios,
the backend and browser worker. Thirty browser/module artifacts compile; the
browser URL policy rejected the selected local-file loader before execution.
The browser-only filter/view case therefore remains compile-only, and no mounted
view reverse-query evidence is claimed. [WORK.md](../WORK.md) owns the exact
receipts. Compilation alone does not establish browser execution or mixed-binary
compatibility.
