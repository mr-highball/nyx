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

Current checked shared/Win32 qualification covers typed source/geometry,
source/history refusal and restoration, exact emitted execution, real PNG/JPEG
decoding, retained fit updates, decoder-error recovery and reusable media parts.
Matching browser consumers and both Studios compile. Current browser/phone and
observing execution, asset registry/import UX, asynchronous load/error events,
orientation/color fidelity, other widgetsets/DPI and complete accessibility,
visual and performance acceptance remain open.

Fit/anchor semantics follow [CSS Images object sizing](https://www.w3.org/TR/css-images-3/#the-object-fit).
Embedded interchange follows [RFC 2397](https://www.rfc-editor.org/rfc/rfc2397);
PNG framing/header definitions are in [PNG Third Edition](https://www.w3.org/TR/png-3/#11IHDR).
