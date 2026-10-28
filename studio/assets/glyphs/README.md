# Nyx Studio glyphs

## Editing and project commands

`nyx-studio-edit-glyph-atlas.png` is a companion original generated with the
built-in imagegen tool using the first atlas as a style reference. Its SHA256 is
`C52E4EB0EADAC5291CC90F67695CB1C1C970890275C1237059A44AF9F499D7FA`.
It is1254x1254 RGBA, preserves transparent PNG bytes and uses proportional4x4
cells. The Pascal
asset packer `tools/nyx_glyph_pack.lpr` emits `nyx.studio.editglyphs.inc` without
resampling; it refuses to overwrite an existing include. Both atlases remain
independent immutable resource definitions, retaining the original role ordinals.

Reading order: New project, Switch project, Duplicate, Move up; Move down,
Make reusable, Delete, Export; Properties, Events, Search, Add; Close, Help,
Apply, Refresh. Command labels remain visible; the graphic is decorative and
does not replace accessible names. Delete uses the foreground tint on its
danger face.

Generation specification: one transparent4x4 sprite sheet matching the existing
rounded, thick slate/violet outlines, equal centered cells and generous padding,
with exactly the reading order above. No labels, grid lines, shadows, gradients,
tiles or backplates. The original atlas was a style reference only; its symbols
and bytes were not replaced. Visual review checked every cell before embedding.
Toolbar review retains a padding follow-up: the first-row document's lower edge
can produce a thin neighboring-cell mark above Move down. A generated correction
did not solve the alignment and was rejected; it is not a project asset. This
sheet does not establish complete visual acceptance of Studio's action family.
The isolated replacement below now repairs the Move down source without changing
these original sheet bytes or role ordinals.

## Isolated Move down repair

`nyx-studio-move-down.png` is an original transparent PNG generated with the
built-in imagegen tool, using the editing atlas only as a style reference. It
replaces the Move down role's source; every other atlas selector remains exact.
The PNG bytes are embedded unchanged in `nyx.studio.movedownglyph.inc` through
the same Pascal packer. The selector uses the whole isolated image, so there is
no neighboring document edge to bleed into the button. The image is1254x1254
RGBA8; SHA256 is
`5815E64DADDA43892818D119C34529FC0BDCF7CEAF0597BDEB0BD23CEBD2DE5A`.

Selected built-in prompt:

> Use case: logo-brand. Asset type: one isolated production button glyph for Nyx Studio. Use the attached bitmap only as a style reference. Create ONE clean downward-pointing arrow matching the Move down glyph at column 1 row 2 of that reference: a simple straight vertical shaft and two diagonal strokes forming a downward chevron, consistent rounded ends and corners, thick dark slate (#4B4C64) strokes, flat crisp geometry. Center this ONE arrow on a genuinely transparent square PNG. The arrow should occupy about 60 percent of the square's width and 65 percent of its height, with ample symmetric transparent padding. No other symbols, neighboring icons, border, grid, lettering, background, texture, shadow, highlight or detached horizontal marks. The existing atlas remains unchanged; this standalone icon will replace only that glyph's source in the program. It must remain clean and legible at a 20-pixel button glyph size.

## Isolated Delete repair

The same final browser review exposed a document-edge mark above Delete too.
`nyx-studio-delete.png` is a second isolated built-in imagegen repair, embedded
unchanged in `nyx.studio.deleteglyph.inc`; it uses the whole image and retains
the existing foreground tint on destructive buttons. It is1254x1254 RGBA8;
SHA256 is `9BE5FD5C1A169F60A84E50F93238558E701C3281161389D0A6F841F56939785A`.

Selected built-in prompt:

> Use case: logo-brand. Asset type: ONE isolated transparent production Delete button glyph for Nyx Studio. Use the attached atlas ONLY as a style reference. Create one centered familiar trash-can symbol matching column 3 row 2 of that reference: rounded dark slate #4B4C64 thick outline, small centered lid handle, horizontal lid, tapered bin sides and three simple violet #7357E8 interior vertical bars. Crisp flat geometry, rounded caps, strong small-size readability. The single trash-can icon occupies approximately 60 percent of the square width and 65 percent of height with generous transparent padding on every side. Genuinely transparent PNG background. No adjacent artwork, detached stray horizontal line, labels, text, grid, tile, shadow, texture, glow or backdrop. This isolated source will replace only the Delete glyph, retaining the original atlas bytes and every other icon.

## Original toolbar commands

`nyx-studio-glyph-atlas.png` is the selected original generated with the built-in
OpenAI imagegen tool for this Studio facelift. No API-key script, package or
external icon/font dependency is used. The original bytes are preserved:
SHA256 `59981D9CB1D5C0426111300010F1DFBCFDC79AB6479838A1F09A1692B5F369AB`.
The actual PNG is **1254 × 1254**, RGBA8. Crop by proportional **4 × 4** cells;
the requested 1024 size in the prompt is not the delivered image size.

Reading order: Undo, Redo, Pascal, Open; Save, Outputs, Agents, Actions;
Build view, Build app, Desktop, Phone; Interact, Tools, Expand, Theme.
Studio embeds one image resource and shares typed selectors. The generated
Pascal include contains exactly the same PNG as base64; runtime ink may tint
its alpha for contrast. Two rejected light variants had poor transparency and
are excluded from project assets. Regenerate the include only from the selected
PNG, retaining provenance and checking its hash/dimensions.

Exact selected prompt:

> Use case: logo-brand. Asset type: production toolbar glyph atlas for Nyx Studio, a professional Pascal UI designer. Create ONE square transparent PNG atlas, exactly 4 columns by 4 rows of equally spaced 256x256 cells, ideally 1024x1024 overall. Each cell contains one isolated icon centered in the cell, occupying a consistent 112x112 region with generous transparent padding. This is an actual UI asset, not a mockup or screenshot. Style: exceptionally clean, distinctive rounded geometric outlines, consistent thick strokes readable when reduced to 20 or 24px; dark slate #4B4C64 main stroke with small restrained violet #7357E8 accents. Flat crisp geometry, no shadows, no gradients, no decorative background, no labels, letters, numbers, tiles, borders, or grid lines. Exactly sixteen distinct familiar symbols in this precise reading order. Row1: Undo curved arrow left; Redo curved arrow right; Pascal source/code angle brackets with slash; Open folder. Row2: Save document/floppy; Output target display with small gear; Agents two connected spark nodes; Actions lightning bolt. Row3: Build view single window with play triangle; Build application stacked windows with play triangle; Desktop monitor; Phone portrait device. Row4: Interact cursor arrow with small click spark; Tools crossed wrench and pencil; Expand four outward corner arrows; Theme paint palette with three small violet dots. Keep glyphs separated and precisely centered on the regular grid for deterministic cropping. Every exterior pixel is genuinely transparent. Avoid tiny interior detail and avoid any text. The atlas should feel like a thoughtfully crafted coherent icon family for a calm modern design application.
