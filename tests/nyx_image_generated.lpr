{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

program nyx_image_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.model, nyx.images, nyx.responsive, nyx.types,
  nyx.generated.images, nyx.image.fixtures
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LImage: TNyxNode;

begin
  { Execute the exact emitted builder; do not reconstruct its image candidate.
    The document alone owns its pages, images and independent recipe sources. }
  LDocument := BuildNyxDocument;
  try
    LImage := LDocument.Find('hero-image');

    if (LDocument.Title <> 'Image workshop') or
      (LImage.Prop('src') <> NyxEmbeddedImage(nimPNG, ImagePNG).ToWire) or
      (LImage.Prop('image-fit') <> 'contain') or
      (LImage.Prop('image-position-x') <> 'center') or
      (LDocument.Find('typed-image').Prop('src') <>
        NyxEmbeddedImage(nimJPEG, ImageJPEG).ToWire) or
      (LDocument.Find('typed-policy-image').Prop('src') <>
        NyxEmbeddedImage(nimPNG, ImagePNG, NyxImageValidation.ContainerChecksums(False)).ToWire) or
      (LImage.Prop(NyxViewportKey(TNyxViewportWidth.Below(420), npfAny, atImageFit)) <> 'cover') or
      (LImage.Prop(NyxPlatformKey(npfNativeLCL, atImageHorizontal)) <> 'end') then
    begin
      raise Exception.Create('Compiled image builder differs from its exact typed candidate');
    end;
    WriteLn('PASS / compiled images / 8 checks');
    {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'passed');{$endif}
  finally
    LDocument.Free;
  end;
end.
