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
publishing a value. Standard admission also verifies every PNG chunk checksum,
including ancillary chunks and empty IEND. JPEG has no such container checksum.
Checksums establish chunk integrity, not decoded pixels or authenticity.

Validation is an immutable fluent value. The default below requires checksums;
derive an explicit framing-only policy when the caller wants host decoding to
decide what happens to damaged chunks:

```pascal
LValidation := NyxImageValidation.ContainerChecksums(False);
LWelcomeImage.Configure
  .Source(NyxEmbeddedImage(nimPNG, CWelcomeImageBase64, LValidation))
  .Done;
```

Declare `LValidation: TNyxImageValidationPolicy`. Derivation leaves the original
value unchanged; `Default(TNyxImageValidationPolicy)` requires checksums too.
The optional third constructor argument works for Base64 and byte input. Framing,
format and byte/dimension budgets remain mandatory for both choices. An unknown
wire policy or a string in place of the Boolean refuses.

At an explicit import boundary, `TNyxImageSource.FromWire(AWire, AValidation)`
re-admits recognized embedded bytes using the caller's selected policy and
canonicalizes the header. This can strengthen or relax a previously recognized
choice; unknown metadata still refuses. Empty/location values keep their ordinary
meaning. Typed import helpers and `INyxImagePicker.Pick(AReply, AValidation)`
copy that same policy for one request; the argument-free overloads use defaults.

Framing-only wire retains `nyx-validation=framing` as an exact data-URL media
parameter beside unchanged raster bytes. Saved designs/resources, paired history,
specialized `Image` values, generated fluent Pascal and managed replay retain it.
Ordinary standard URLs stay unchanged. The browser adapter validates this exact
portable boundary rather than maintaining a second literal prefix whitelist.
This setting covers embedded byte admission; a location validates no bytes until
its resolver loads them. It neither changes resource caching nor guarantees that
a host will decode a checksummed but malformed compressed stream.

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

Current checked shared/Win32 qualification passes 199 assertions covering typed
source/geometry, source/history, real PNG/JPEG decoding, retained fit updates,
decoder-error recovery and reusable media parts. Exact emitted reconstruction
passes eight on native and actual HTTP browser execution. The semantic workshop
is authored through native MCP tools; both service compiler inputs exactly match
the bounded source export, and a phone-sized semantic preview renders.

The current desktop and CSS-390 browser journeys pass 231 assertions each,
including actual PNG/JPEG samples, independent reusable images, the retained
square crop, the explicit policy data URL, default checksum refusal before source
replacement, correction and pending-decode invalidation on clear. Owned target
listeners/timers and views explicitly retire. A deliberately framing-only damaged
request still loads/decodes and paints transparent sample pixels on the checked
browser; the probe retains that outcome and verifies replacement identity. Native
refuses those same unchecked bytes. The old default corruption regression is
repaired by shared checksum admission, not by assuming the host is strict.
Checksummed malformed codecs and full decoded-pixel integrity remain open.
See [the policy evidence and remaining limits](../WORK.md#current-return-path-typed-image-admission-policy--2026-10-08).

Run `tools/build.ps1 -Target image-presentation -ImageSourceDirectory <directory>`
with the exact MCP-exported `nyx.generated.view.pas` in that directory. Omitting
the directory retains the existing seed default. The build runs native consumers
and stages browser programs; a successful build does not mean the browser journey
passed. Serve staged artifacts over HTTP and use the maintained ready/capture
driver for actual execution. This target starts no server and edits no project.

Studio's public Nyx image form now exposes **Verify PNG checksums** and
**Framing only**. Plain Base64, pasted complete URLs and file imports use the
selected policy. Apply re-admits the current preview when that choice changes,
without requiring another paste. A late import refuses a changed project,
selection, image baseline or current policy. Drafts retain incomplete choices
through chrome rebuilds/parked panels; version-1 preferences migrate their six
original fields and derive the added choice from their admitted source. Clear
preserves the unsubmitted choice. Only Apply publishes a paired, undoable edit.

Actual Win32 and ordinary HTTP desktop/CSS-390 Studio consumers exercise this
workflow, typed source, exact paired Undo/Redo and explicit retirement. Browser
file injection uses real FileReader with synthetic input; native substitutes
only the OS chooser. These checks do not qualify trusted physical chooser/phone
behavior. Complete observing Studio rollout, the common asset
registry/import UX, portable image load/error/status events, complete validation,
orientation/color fidelity, other widgetsets/DPI and full accessibility, visual
and performance acceptance remain open under their existing owners.

Run `tools/build.ps1 -Target image-authoring` after the existing
`image-presentation` semantic seed/raster prerequisite. It exercises the ordinary
Win32 Studio form, pasted/imported proposal, paired source/history and stale
refusal, then compiles and executes the exact emitted builder and stages browser
counterparts, including Studio's Pascal source worker. The optional
`-ImageSourceDirectory <directory>` consumes the exact semantic workshop export.
Serve the browser closure over HTTP and use the ready/capture driver: the
ordinary browser journey requests one live capture acknowledgement before
disposing. Trusted chooser/physical phone, observing deployment and other
widgetsets remain unqualified. See
[the authoring evidence](../WORK.md#current-return-path-image-policy-authoring-and-readiness--2026-10-08).

## Typed lifecycle observations

Images expose `OnImageLoading`, `OnImageReady`, `OnImageError` and
`OnImageCleared` through both runtime streams and `NyxCallbacks` authoring.
They use the existing ordered multiple registrations, removal, failure isolation
and execution policies. Studio's shared event metadata includes all four with
the `image` context; ordinary handler creation retains source navigation and
paired Undo/Redo.

```pascal
NyxCallbacks(LBanner).OnImageReady
  .Policy(neUIQueue)
  .Add(NyxHandler('TShowBannerDetails'), NyxCallbackID('banner.details'));

LApplicationEvents.OnImageError(NyxControlEvents('welcome-banner'))
  .Subscribe(LShowImageError);
```

Callbacks read `AEvent.HasImage` and the immutable `AEvent.Image` snapshot from
`nyx.image.lifecycle`. It owns the request identity, exact typed source, phase,
natural dimensions and typed failure/diagnostic. A retained snapshot survives
source replacement and view disposal without retaining a control or document.
IDs are monotonic within one mounted image face; identify them with the event's
origin and view lifetime, rather than comparing integers across remounts.

| Phase | Meaning |
| --- | --- |
| `nipLoading` | The adapter begins an admitted nonempty source request. |
| `nipReady` | The target accepts decoding with positive natural dimensions. |
| `nipFailed` | The request cannot supply that decoded image; owned failure text describes the target observation. |
| `nipCleared` | `NyxNoImage` withdraws the source, including an initially empty face. |

Observations begin on a UI turn after accepted view publication and retain phase
order through one FIFO pump. Callback policies then apply within each event.
Changing or clearing the source cancels older pending callbacks and their
`PostUI` descendants, including siblings not yet entered during sequential
reentrancy. Running native workers observe cancellation cooperatively. Disposing
the view retires its listeners, weak receivers and request generations. The
usual enabled/visible event routing applies; becoming visible does not replay
completed requests. An unchanged source emits no repeated notifications.

Native decoding still uses an independent candidate picture; decoder failure
preserves the prior picture and propagates the original `Sync` exception while
queuing an error observation. Missing/unsupported native locations retain their
existing empty-face behavior and report `nifUnavailable`. Explicitly calling
`Sync` after a decoder exception retries a source without an accepted physical
baseline. Browser readiness waits for the matching current request's `load` and
`decode()` completion; opaque host request/decode failures report `nifDecode`.
Neither readiness nor dimensions establish full decoded-pixel integrity or
replace the typed admission policy. Hosted native fetching remains the resource
resolver's responsibility. The browser adapter follows the
[HTML image request model](https://html.spec.whatwg.org/multipage/images.html)
and [image decoding contract](https://html.spec.whatwg.org/multipage/embedded-content.html#dom-img-decode).

The maintained `image-presentation` target now also executes the request/event
consumer and emits its exact ordinary-session companion. Its separate compiled
consumer checks authored registration identity, multiplicity and policy. Actual
browser execution, host retirement and visual checkpoints remain explicit checks,
as does deployment to an observing Studio. See
[the lifecycle packet](../WORK.md#current-return-path-typed-image-lifecycle-delivery--2026-10-08).

Fit/anchor semantics follow [CSS Images object sizing](https://www.w3.org/TR/css-images-3/#the-object-fit).
Embedded interchange follows [RFC 2397](https://www.rfc-editor.org/rfc/rfc2397);
PNG framing/header definitions are in [PNG Third Edition](https://www.w3.org/TR/png-3/#11IHDR).
