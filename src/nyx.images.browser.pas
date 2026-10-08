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

unit nyx.images.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Web, nyx.model;

{ Apply closed image sizing to an existing ordinary HTML image. The element and
  node are borrowed synchronously; this adapter retains neither. Pixel fetching/
  decoding remains the browser's asynchronous image contract. }
procedure ApplyNyxBrowserImage(ANode: TNyxNode; AImage: TJSHTMLImageElement);

implementation

uses
  nyx.images;

procedure ApplyNyxBrowserImage(ANode: TNyxNode; AImage: TJSHTMLImageElement);
const
  CPositions: array[TNyxImageAnchor] of String = ('0%', '50%', '100%');
begin
  AImage.style.setProperty('object-fit',
    NyxImageFitName(ReadNyxImageFit(ANode.Prop('image-fit', 'contain'))));
  AImage.style.setProperty('object-position',
    CPositions[ReadNyxImageAnchor(ANode.Prop('image-position-x', 'center'))] + ' ' +
    CPositions[ReadNyxImageAnchor(ANode.Prop('image-position-y', 'center'))]);
  { Portable sizing describes encoded pixels. EXIF orientation/color management
    still need their own qualified contract; do not silently infer a shared one. }
  AImage.style.setProperty('image-orientation', 'none');
end;

end.
