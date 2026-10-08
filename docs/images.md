# Portable images

[Components](components.md) · [Responsive layouts](responsive.md) ·
[Public contract](../src/nyx.images.pas)

Image controls accept an immutable `TNyxImageSource` and closed fit/position
choices. Embedded PNG/JPEG uses the same bytes in browser pages and native LCL
controls, including independently owned named media parts in reusable recipes.
Images retain normal Nyx layout, accessibility, history and source ownership.

```pascal
function NewBanner(const APicture: TNyxImageBytes): INyxImage;
begin
  Result := NewNyxImage('welcome-banner');
  Result.Configure
    .Source(NyxEmbeddedImageBytes(nimPNG, APicture))
    .AlternativeText('A welcoming landscape')
    .Width(320)
    .Height(160)
    .ImageFit(nifContain)
    .ImageHorizontal(niaCenter)
    .ImageVertical(niaCenter)
    .WhenViewport(TNyxViewportWidth.Below(420))
    .ImageFit(nifCover)
    .Done;
end;
```

Use `nyx.controls`, `nyx.images` and `nyx.responsive` for this example.
The returned specialized interface retains its node lease; its parent/document
owns the node after attachment. Source values retain immutable text, never a
stream, document, control or borrowed byte array. Each `Bytes` read returns an
independent array. The specialized interface also exposes `Image`, `ImageFit`,
`ImageHorizontal` and `ImageVertical` properties.

| Fit | Behavior |
| --- | --- |
| `nifContain` | Preserve aspect ratio and fit inside the content box. |
| `nifCover` | Preserve aspect ratio, fill the box and crop excess. |
| `nifStretch` | Fill the box independently along each axis. |
| `nifNatural` | Use encoded pixel dimensions and clip excess. |
| `nifShrink` | Contain without enlarging natural dimensions. |

Anchors `niaStart`, `niaCenter` and `niaEnd` position remaining space on
each axis, including negative space when cropping. Defaults are contain/center.
`Clear(atImageFit)`, `Clear(atImageHorizontal)` and
`Clear(atImageVertical)` restore these defaults. `Source(NyxNoImage)`
deliberately clears the picture; the browser removes its source attribute.
Platform, width/height conditions and named presentation scopes use the existing
typed configuration contract.

Embedded source admission requires compact canonical base64, matching raster
headers and positive dimensions, within 1 MiB decoded bytes, 4096 pixels per
dimension and 16,777,216 pixels. PNG additionally requires bounded complete chunk
framing, pixel data and a terminal empty IEND. JPEG requires SOI/final EOI and a
supported eight-bit frame header. These checks raise `ENyxImage` before
publishing a value. They do not establish valid CRCs or decoded pixels.

Ordinary adapters delegate decoding to the browser or LCL. Native decoding uses
an independently owned candidate picture before replacing the accepted one;
decoder failure retains that picture and its source baseline and propagates
the decoder exception. Browser decoding/fetch errors remain asynchronous.
Generated source uses typed embedded constructors and enum methods, and the
managed reader admits the same contract. Persistence and explicit legacy
`Source(TNyxText)`/`FromWire` boundaries retain wire text.

For open locations, use `NyxImage(NyxImageLocation(AReference))`.
Browser locations use normal relative/HTTP resolution. The native standard
adapter retains existing local-file resolution; missing files produce an empty
picture, and HTTP/network locations require a future supplied resolver.
An exported design cannot make a machine-local file portable.

## Packed resources and Studio

Inline Base64 is a portable image source, not an external file reference. Both
saved projects and generated Pascal retain the exact PNG/JPEG bytes:

```pascal
LWelcomeImage := NewNyxImage('welcome-image');
LWelcomeImage.Configure
  .Source(NyxEmbeddedImage(nimPNG, CWelcomeImageBase64))
  .AlternativeText('A welcoming landscape')
  .Done;
```

`CWelcomeImageBase64` is application-owned canonical Base64 text. Use
`nimJPEG` for JPEG. SVG and other image formats are not yet admitted by this
contract. Existing decoded byte/dimension limits apply equally to pasted and
imported resources; no machine path is packed into the project.

The selected image's Properties form offers **Import PNG or JPEG**, or an
**Inline Base64** memo with a PNG/JPEG format choice. Paste canonical Base64 or
a complete matching `data:image/...;base64,...` URL, then choose **Preview
Base64**. Preview, alternative text, fit, anchors and **Clear image** remain
unsubmitted proposals. **Apply image** publishes all five image properties
through the ordinary isolated design processor as one paired Undo/Redo step.
Incomplete/invalid choices and stale owner/baseline contexts refuse. A pending
Pascal draft keeps the existing refusal rather than silently replacing source.
The advanced property view retains explicit source-reference/wire editing.

The public [Nyx form](../src/nyx.image.editor.pas) is built from ordinary
specialized Nyx controls. Both Studio controllers consume copied proposals;
chrome changes and per-workspace preferences retain pasted text and image bytes.
The [portable picker contract](../src/nyx.image.import.pas) delivers copied
source/status values without a file, path, document or host widget. Controllers
capture project/selection/baseline before opening a picker and cancel borrowed
callbacks before their receiver retires. Late delivery cannot edit another
project or selection. Browser adapters use FileReader; the native adapter reads
bounded bytes and validates its real pixel decoder before publishing a source.
Native inline preview uses the same decoder qualification. Browser pixel/fetch
failures remain asynchronous and require separate execution evidence.

Current checked shared/Win32 qualification covers typed source/geometry,
source/history refusal and restoration, exact emitted execution, real PNG/JPEG
decoding, retained fit updates, decoder-error recovery and reusable media parts.
Matching browser consumers and both Studios compile. Current browser/phone and
observing execution, complete asset registry/import UX, asynchronous load/error events,
orientation/color fidelity, other widgetsets/DPI and complete accessibility,
visual and performance acceptance remain open.

Run `tools/build.ps1 -Target image-authoring` after the existing
`image-presentation` semantic seed/raster prerequisite. It exercises the ordinary
Win32 Studio form, pasted/imported proposal, paired source/history and stale
refusal, then compiles and executes the exact emitted builder and stages browser
counterparts. Its picker substitutes only the OS chooser; trusted chooser,
browser/phone/observing execution and other widgetsets remain unqualified.

Fit/anchor semantics follow [CSS Images object sizing](https://www.w3.org/TR/css-images-3/#the-object-fit).
Embedded interchange follows [RFC 2397](https://www.rfc-editor.org/rfc/rfc2397);
PNG framing/header definitions are in [PNG Third Edition](https://www.w3.org/TR/png-3/#11IHDR).
