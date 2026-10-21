# Nyx Studio glyphs

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
