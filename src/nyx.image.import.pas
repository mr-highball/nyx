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
unit nyx.image.import;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.images;

type
  TNyxImagePickStatus = (ipsSelected, ipsCancelled, ipsFailed);
  { Reply owns only copied values. A picker borrows its method receiver until
    reply or Cancel. No path, file/stream, document or host control escapes. }
  TNyxImagePickReply = procedure(AStatus: TNyxImagePickStatus;
    const ASource: TNyxImageSource; const AError: TNyxText) of object;
  { One local UI-thread pick. Native modal reply can arrive before Pick returns;
    browser reply is asynchronous. Capture owner/context before calling Pick.
    Cancel silently detaches pending delivery; call it before retiring a receiver.
    Reentrant reply may cancel/release the picker: adapters retain themselves
    across notification. Repeated Pick cancels the preceding local delivery. }
  INyxImagePicker = interface
    ['{8B37D606-1B21-4526-91C5-104B1D4FDD78}']
    procedure Pick(AReply: TNyxImagePickReply); overload;
    { Copies the caller's embedded admission choice for this one delivery.
      Native pixel-decoder qualification remains independent and mandatory. }
    procedure Pick(AReply: TNyxImagePickReply;
      const AValidation: TNyxImageValidationPolicy); overload;
    procedure Cancel;
  end;

{ Detect encoded raster headers, independent of file extension/MIME. Borrowed
  input is copied into an immutable embedded source. Empty/oversized/unsupported
  bytes refuse before any target decoder or accepted document is touched. }
function NyxImportedImage(const ABytes: TNyxImageBytes): TNyxImageSource; overload;
function NyxImportedImage(const ABytes: TNyxImageBytes;
  const AValidation: TNyxImageValidationPolicy): TNyxImageSource; overload;
{ Browser FileReader data URLs may have an empty/incorrect MIME declaration.
  The canonical base64 payload is admitted using actual PNG/JPEG headers. }
function NyxImportedImageBase64(const ABase64: TNyxText): TNyxImageSource; overload;
function NyxImportedImageBase64(const ABase64: TNyxText;
  const AValidation: TNyxImageValidationPolicy): TNyxImageSource; overload;

implementation

function NyxImportedImage(const ABytes: TNyxImageBytes): TNyxImageSource;
begin
  Result := NyxImportedImage(ABytes, NyxImageValidation);
end;

function NyxImportedImage(const ABytes: TNyxImageBytes;
  const AValidation: TNyxImageValidationPolicy): TNyxImageSource;
var
  LFormat: TNyxImageFormat;
begin

  if (Length(ABytes) = 0) or (Length(ABytes) > NyxImageMaximumBytes) then
  begin
    raise ENyxImage.Create('Choose a PNG or JPEG up to 1 MiB');
  end;
  LFormat := nimPNG;

  if (Length(ABytes) >= 2) and (ABytes[0] = $FF) and (ABytes[1] = $D8) then
  begin
    LFormat := nimJPEG;
  end;
  Result := NyxEmbeddedImageBytes(LFormat, ABytes, AValidation);
end;

function NyxImportedImageBase64(const ABase64: TNyxText): TNyxImageSource;
begin
  Result := NyxImportedImageBase64(ABase64, NyxImageValidation);
end;

function NyxImportedImageBase64(const ABase64: TNyxText;
  const AValidation: TNyxImageValidationPolicy): TNyxImageSource;
begin
  try
    Result := NyxEmbeddedImage(nimPNG, ABase64, AValidation);
  except
    on ENyxImage do
    begin
      Result := NyxEmbeddedImage(nimJPEG, ABase64, AValidation);
    end;
  end;
end;

end.
