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

unit nyx.images;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, nyx.text;

type
  { PNG/JPEG are explicit embedded raster formats. A location is an open resource
    reference resolved by the target adapter; it never implies a shared filesystem. }
  TNyxImageFormat = (nimPNG, nimJPEG);
  TNyxImageFit = (nifContain, nifCover, nifStretch, nifNatural, nifShrink);
  TNyxImageAnchor = (niaStart, niaCenter, niaEnd);
  TNyxImageSourceKind = (nisEmpty, nisLocation, nisEmbedded);
  ENyxImage = class(Exception);

  { Distinct open location. Owns text only, never a stream or platform object.
    Relative/HTTP references suit browsers; native local files need a resolver
    for network references. Embedded sources require no external resolution. }
  TNyxImageLocation = record
    Name: TNyxText;
  end;

  TNyxImageBytes = array of Byte;

  { Immutable admission choices. Default and NyxImageValidation require every
    PNG chunk checksum. ContainerChecksums(False) deliberately retains bounded
    framing admission only; derive it without changing the original policy.
    Neither choice proves pixel decoding. JPEG has no PNG-style chunk checksum.
    Policies own scalar values only and never retain callers or target objects. }
  TNyxImageValidationPolicy = record
  private
    FSkipContainerChecksums: Boolean;
    function GetChecksumsRequired: Boolean;
  public
    function ContainerChecksums(ARequired: Boolean): TNyxImageValidationPolicy;
    property ChecksumsRequired: Boolean read GetChecksumsRequired;
  end;

  { Immutable image source. FromWire is the explicit legacy/persistence boundary;
    authored code uses NyxImage/NyxEmbeddedImage. Embedded admission checks strict
    base64, matching raster headers, PNG chunk framing and allocation dimensions
    before publishing. Standard admission also verifies PNG chunk checksums;
    explicit framing-only policy is retained by a data-URL media parameter.
    It does not decode pixels: checksummed malformed codecs remain possible.
    Every Bytes read returns an independently owned array. Default means no image. }
  TNyxImageSource = record
  private
    FWire: TNyxText;
    FKind: TNyxImageSourceKind;
    FFormat: TNyxImageFormat;
    FWidth: Integer;
    FHeight: Integer;
    FPayload: TNyxText;
    FValidation: TNyxImageValidationPolicy;
  public
    class function FromWire(const AWire: TNyxText): TNyxImageSource; static;
    function ToWire: TNyxText;
    { Bytes/Format/Width/Height require an embedded source and raise ENyxImage
      otherwise. Dimensions describe encoded pixels, independent of metadata. }
    function Bytes: TNyxImageBytes;
    function Format: TNyxImageFormat;
    function Width: Integer;
    function Height: Integer;
    function Encoded: TNyxText;
    property Kind: TNyxImageSourceKind read FKind;
    { Embedded sources retain their exact authored policy through wire/history.
      Empty/location values expose the standard policy but validate no fetched
      bytes: target/resource resolvers own that separate admission boundary. }
    property Validation: TNyxImageValidationPolicy read FValidation;
  end;

  { Fractional logical destination within a control's content box. Cover/natural
    can extend beyond the box; adapters clip painting to the control. Integer
    widgetsets round at their boundary, not in the portable sizing algorithm. }
  TNyxImageRectangle = record
    Left: Double;
    Top: Double;
    Width: Double;
    Height: Double;
  end;

const
  NyxImageMaximumBytes = 1048576;
  NyxImageMaximumDimension = 4096;
  NyxImageMaximumPixels = 16777216;

{ Locations refuse empty/NUL text. NoImage is the deliberate empty source.
  Embedded input is compact canonical base64 (no whitespace or unused pad bits);
  wrong format, truncated headers and exceeded byte/dimension budgets raise. }
function NyxImageLocation(const AName: TNyxText): TNyxImageLocation;
function NyxImage(const ALocation: TNyxImageLocation): TNyxImageSource;
function NyxNoImage: TNyxImageSource;
{ Standard checksum admission. A default-initialized policy means the same.
  Derive ContainerChecksums(False) explicitly to delegate corrupt-pixel behavior
  to each host after mandatory framing/byte/dimension admission. }
function NyxImageValidation: TNyxImageValidationPolicy;
function NyxEmbeddedImage(AFormat: TNyxImageFormat;
  const ABase64: TNyxText): TNyxImageSource; overload;
function NyxEmbeddedImage(AFormat: TNyxImageFormat; const ABase64: TNyxText;
  const AValidation: TNyxImageValidationPolicy): TNyxImageSource; overload;
{ Byte interchange, useful for Pascal resource/import adapters. Input is borrowed
  during this call; the resulting value retains only copied immutable text. }
function NyxEmbeddedImageBytes(AFormat: TNyxImageFormat;
  const ABytes: TNyxImageBytes): TNyxImageSource; overload;
function NyxEmbeddedImageBytes(AFormat: TNyxImageFormat; const ABytes: TNyxImageBytes;
  const AValidation: TNyxImageValidationPolicy): TNyxImageSource; overload;
function NyxImageFitName(AFit: TNyxImageFit): TNyxText;
function NyxImageAnchorName(AAnchor: TNyxImageAnchor): TNyxText;
function NyxImageFormatSymbol(AFormat: TNyxImageFormat): TNyxText;
function NyxImageFitSymbol(AFit: TNyxImageFit): TNyxText;
function NyxImageAnchorSymbol(AAnchor: TNyxImageAnchor): TNyxText;
{ Wire readers accept empty as the explicit Clear/default representation.
  Other unknown values refuse; authored behavior uses the enum methods above. }
function ReadNyxImageFit(const AName: TNyxText): TNyxImageFit;
function ReadNyxImageAnchor(const AName: TNyxText): TNyxImageAnchor;
{ Nonpositive source/box dimensions produce an empty rectangle. Position applies
  to remaining space, including negative space for cover/cropped natural images.
  Semantics match CSS Images object-fit/object-position; no host handles survive. }
function NyxImageRectangle(ASourceWidth, ASourceHeight, ABoxWidth,
  ABoxHeight: Integer; AFit: TNyxImageFit; AHorizontal: TNyxImageAnchor = niaCenter;
  AVertical: TNyxImageAnchor = niaCenter): TNyxImageRectangle;

implementation

const
  CAlphabet: TNyxText = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  CPrefixes: array[TNyxImageFormat] of TNyxText =
    ('data:image/png;base64,', 'data:image/jpeg;base64,');
  { Policy travels beside unchanged Base64 bytes, using RFC 2397's media-parameter
    boundary. Recognize only this exact version-1 choice; unknown/duplicated
    parameters refuse instead of quietly downgrading admission. Browsers decode
    the same raster; native readers consume Bytes, not a filesystem URL. }
  CFramingPrefixes: array[TNyxImageFormat] of TNyxText =
    ('data:image/png;nyx-validation=framing;base64,',
     'data:image/jpeg;nyx-validation=framing;base64,');
  CFitNames: array[TNyxImageFit] of TNyxText =
    ('contain', 'cover', 'fill', 'none', 'scale-down');
  CAnchorNames: array[TNyxImageAnchor] of TNyxText = ('start', 'center', 'end');

function NyxImageValidation: TNyxImageValidationPolicy;
begin
  Result := Default(TNyxImageValidationPolicy);
end;

function TNyxImageValidationPolicy.GetChecksumsRequired: Boolean;
begin
  Result := not FSkipContainerChecksums;
end;

function TNyxImageValidationPolicy.ContainerChecksums(ARequired: Boolean): TNyxImageValidationPolicy;
begin
  Result := Self;
  Result.FSkipContainerChecksums := not ARequired;
end;

function NyxImageFitName(AFit: TNyxImageFit): TNyxText;
begin
  Result := CFitNames[AFit];
end;

function NyxImageAnchorName(AAnchor: TNyxImageAnchor): TNyxText;
begin
  Result := CAnchorNames[AAnchor];
end;

function NyxImageFormatSymbol(AFormat: TNyxImageFormat): TNyxText;
const
  CNames: array[TNyxImageFormat] of TNyxText = ('nimPNG', 'nimJPEG');
begin
  Result := CNames[AFormat];
end;

function NyxImageFitSymbol(AFit: TNyxImageFit): TNyxText;
const
  CNames: array[TNyxImageFit] of TNyxText =
    ('nifContain', 'nifCover', 'nifStretch', 'nifNatural', 'nifShrink');
begin
  Result := CNames[AFit];
end;

function NyxImageAnchorSymbol(AAnchor: TNyxImageAnchor): TNyxText;
const
  CNames: array[TNyxImageAnchor] of TNyxText = ('niaStart', 'niaCenter', 'niaEnd');
begin
  Result := CNames[AAnchor];
end;

function ReadNyxImageFit(const AName: TNyxText): TNyxImageFit;
var
  LFit: TNyxImageFit;
begin

  if AName = '' then
  begin
    Exit(nifContain);
  end;
  for LFit := Low(TNyxImageFit) to High(TNyxImageFit) do
  begin

    if CFitNames[LFit] = AName then
    begin
      Result := LFit;
      Exit;
    end;
  end;
  raise ENyxImage.Create('Unknown image fit');
end;

function ReadNyxImageAnchor(const AName: TNyxText): TNyxImageAnchor;
var
  LAnchor: TNyxImageAnchor;
begin

  if AName = '' then
  begin
    Exit(niaCenter);
  end;
  for LAnchor := Low(TNyxImageAnchor) to High(TNyxImageAnchor) do
  begin

    if CAnchorNames[LAnchor] = AName then
    begin
      Result := LAnchor;
      Exit;
    end;
  end;
  raise ENyxImage.Create('Unknown image position');
end;

function DecodeBase64(const AText: TNyxText): TNyxImageBytes;
var
  LPadding: Integer;
  LIndex: Integer;
  LOutput: Integer;
  LPart: Integer;
  LValues: array[0..3] of Integer;
  LLength: Integer;
begin
  Result := nil;
  LLength := Length(AText);

  if (LLength = 0) or (LLength mod 4 <> 0) or
    (LLength > ((NyxImageMaximumBytes + 2) div 3) * 4) then
  begin
    raise ENyxImage.Create('Embedded image exceeds the base64 byte budget or has invalid length');
  end;
  LPadding := 0;

  if AText[LLength] = '=' then
  begin
    Inc(LPadding);

    if AText[LLength - 1] = '=' then
    begin
      Inc(LPadding);
    end;
  end;
  LOutput := (LLength div 4) * 3 - LPadding;

  if LOutput > NyxImageMaximumBytes then
  begin
    raise ENyxImage.Create('Embedded image exceeds the decoded byte budget');
  end;
  SetLength(Result, LOutput);
  LOutput := 0;
  LIndex := 1;
  while LIndex <= LLength do
  begin
    for LPart := 0 to 3 do
    begin
      LValues[LPart] := Pos(AText[LIndex + LPart], CAlphabet) - 1;

      if (LValues[LPart] < 0) and (AText[LIndex + LPart] = '=') and
        (LIndex + LPart > LLength - LPadding) then
      begin
        LValues[LPart] := 0;
      end
      else if LValues[LPart] < 0 then
      begin
        raise ENyxImage.Create('Embedded image requires compact canonical base64');
      end;
    end;

    if (LIndex + 3 = LLength) and
      (((LPadding = 2) and (LValues[1] and 15 <> 0)) or
      ((LPadding = 1) and (LValues[2] and 3 <> 0))) then
    begin
      raise ENyxImage.Create('Embedded image has noncanonical base64 pad bits');
    end;
    Result[LOutput] := (LValues[0] shl 2) or (LValues[1] shr 4);
    Inc(LOutput);

    if LOutput < Length(Result) then
    begin
      Result[LOutput] := ((LValues[1] and 15) shl 4) or (LValues[2] shr 2);
      Inc(LOutput);
    end;

    if LOutput < Length(Result) then
    begin
      Result[LOutput] := ((LValues[2] and 3) shl 6) or LValues[3];
      Inc(LOutput);
    end;
    Inc(LIndex, 4);
  end;
end;

function EncodeBase64(const ABytes: TNyxImageBytes): TNyxText;
var
  LIndex: Integer;
  LFirst: Integer;
  LSecond: Integer;
  LThird: Integer;
  LGroup: TNyxText;
  LChunk: TNyxText;
  LParts: TNyxStrings;
begin

  if (Length(ABytes) = 0) or (Length(ABytes) > NyxImageMaximumBytes) then
  begin
    raise ENyxImage.Create('Embedded image bytes must fit the declared budget');
  end;
  { Bounded chunks avoid whole-string indexed mutation on pas2js. The portable
    text join performs one complete output allocation on either target. }
  LParts := TNyxStrings.Create;
  try
    LChunk := '';
    LIndex := 0;
    while LIndex < Length(ABytes) do
    begin
      LFirst := ABytes[LIndex];
      LSecond := 0;
      LThird := 0;

      if LIndex + 1 < Length(ABytes) then
      begin
        LSecond := ABytes[LIndex + 1];
      end;

      if LIndex + 2 < Length(ABytes) then
      begin
        LThird := ABytes[LIndex + 2];
      end;
      LGroup := TNyxText(CAlphabet[(LFirst shr 2) + 1]) +
        CAlphabet[((LFirst and 3) shl 4) + (LSecond shr 4) + 1] + '==';

      if LIndex + 1 < Length(ABytes) then
      begin
        LGroup[3] := CAlphabet[((LSecond and 15) shl 2) + (LThird shr 6) + 1];
      end;

      if LIndex + 2 < Length(ABytes) then
      begin
        LGroup[4] := CAlphabet[(LThird and 63) + 1];
      end;
      LChunk := LChunk + LGroup;

      if Length(LChunk) >= 4096 then
      begin
        LParts.Add(LChunk);
        LChunk := '';
      end;
      Inc(LIndex, 3);
    end;

    if LChunk <> '' then
    begin
      LParts.Add(LChunk);
    end;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

{ PNG's reflected CRC-32 covers the four type bytes and chunk data, excluding
  length and stored CRC. Fixed unsigned shifts/XOR keep native checked arithmetic
  and pas2js within the same 32-bit domain; no table/global mutable state survives.
  The caller has already bounded this span against the complete byte stream. }
function PNGChecksum(const ABytes: TNyxImageBytes; AStart, ACount: Integer): LongWord;
var
  LCRC: LongWord;
  LIndex: Integer;
  LBit: Integer;
begin
  LCRC := $FFFFFFFF;
  for LIndex := AStart to AStart + ACount - 1 do
  begin
    LCRC := LCRC xor ABytes[LIndex];
    for LBit := 0 to 7 do
    begin

      if (LCRC and 1) <> 0 then
      begin
        LCRC := (LCRC shr 1) xor $EDB88320;
      end
      else
      begin
        LCRC := LCRC shr 1;
      end;
    end;
  end;
  Result := LCRC xor $FFFFFFFF;
end;

procedure Dimensions(AFormat: TNyxImageFormat; const ABytes: TNyxImageBytes;
  const AValidation: TNyxImageValidationPolicy;
  out AWidth, AHeight: Integer);
const
  CSignature: array[0..7] of Byte = (137, 80, 78, 71, 13, 10, 26, 10);
var
  LIndex: Integer;
  LMarker: Integer;
  LLength: Integer;
  LWidth: Double;
  LHeight: Double;
  LChunkLength: Double;
  LHasPixels: Boolean;
  LHasEnd: Boolean;
  LCRCStart: Integer;
  LStoredCRC: Double;
begin
  AWidth := 0;
  AHeight := 0;
  LWidth := 0;
  LHeight := 0;

  if AFormat = nimPNG then
  begin

    if Length(ABytes) < 33 then
    begin
      raise ENyxImage.Create('Truncated PNG header');
    end;
    for LIndex := 0 to 7 do
    begin

      if ABytes[LIndex] <> CSignature[LIndex] then
      begin
        raise ENyxImage.Create('Embedded PNG signature does not match its format');
      end;
    end;

    if (ABytes[8] <> 0) or (ABytes[9] <> 0) or (ABytes[10] <> 0) or
      (ABytes[11] <> 13) or (ABytes[12] <> 73) or (ABytes[13] <> 72) or
      (ABytes[14] <> 68) or (ABytes[15] <> 82) then
    begin
      raise ENyxImage.Create('Embedded PNG requires its initial IHDR');
    end;
    for LIndex := 16 to 19 do
    begin
      LWidth := LWidth * 256 + ABytes[LIndex];
      LHeight := LHeight * 256 + ABytes[LIndex + 4];
    end;
    { Check framing before either decoder sees bytes. Some native readers ignore
      short chunk-header reads; letting truncated input reach them can fail in
      allocation cleanup rather than produce a recoverable decoding exception.
      Mandatory framing bounds checksum reads. Checksums establish chunk
      integrity only; pixel-codec validation remains a separate guarantee. }
    LIndex := 8;
    LHasPixels := False;
    LHasEnd := False;
    while LIndex < Length(ABytes) do
    begin

      if Length(ABytes) - LIndex < 12 then
      begin
        raise ENyxImage.Create('Truncated PNG chunk framing');
      end;
      LChunkLength := ABytes[LIndex] * 16777216.0 +
        ABytes[LIndex + 1] * 65536.0 + ABytes[LIndex + 2] * 256.0 +
        ABytes[LIndex + 3];

      if LChunkLength > Length(ABytes) - LIndex - 12 then
      begin
        raise ENyxImage.Create('PNG chunk exceeds the admitted byte stream');
      end;

      if AValidation.ChecksumsRequired then
      begin
        LCRCStart := LIndex + 8 + Trunc(LChunkLength);
        { Double holds every unsigned 32-bit stored value exactly on both targets
          without a native Integer multiply overflowing under checked builds. }
        LStoredCRC := ABytes[LCRCStart] * 16777216.0 +
          ABytes[LCRCStart + 1] * 65536.0 + ABytes[LCRCStart + 2] * 256.0 +
          ABytes[LCRCStart + 3];

        if PNGChecksum(ABytes, LIndex + 4, Trunc(LChunkLength) + 4) <> LStoredCRC then
        begin
          raise ENyxImage.Create('Embedded PNG chunk checksum does not match its type and data');
        end;
      end;
      LHasPixels := LHasPixels or ((ABytes[LIndex + 4] = 73) and
        (ABytes[LIndex + 5] = 68) and (ABytes[LIndex + 6] = 65) and
        (ABytes[LIndex + 7] = 84) and (LChunkLength > 0));
      LHasEnd := (ABytes[LIndex + 4] = 73) and (ABytes[LIndex + 5] = 69) and
        (ABytes[LIndex + 6] = 78) and (ABytes[LIndex + 7] = 68);

      if LHasEnd and ((LChunkLength <> 0) or
        (LIndex + 12 <> Length(ABytes))) then
      begin
        raise ENyxImage.Create('PNG requires an empty final IEND chunk');
      end;
      Inc(LIndex, Trunc(LChunkLength) + 12);
    end;

    if not LHasPixels or not LHasEnd then
    begin
      raise ENyxImage.Create('PNG requires pixel data and its final IEND');
    end;
  end
  else
  begin

    if (Length(ABytes) < 4) or (ABytes[0] <> 255) or (ABytes[1] <> 216) or
      (ABytes[High(ABytes) - 1] <> 255) or (ABytes[High(ABytes)] <> 217) then
    begin
      raise ENyxImage.Create('Embedded JPEG requires SOI and final EOI markers');
    end;
    LIndex := 2;
    while LIndex + 3 < Length(ABytes) do
    begin

      if ABytes[LIndex] <> 255 then
      begin
        raise ENyxImage.Create('Malformed JPEG header marker');
      end;
      while (LIndex < Length(ABytes)) and (ABytes[LIndex] = 255) do
      begin
        Inc(LIndex);
      end;

      if LIndex >= Length(ABytes) then
      begin
        Break;
      end;
      LMarker := ABytes[LIndex];
      Inc(LIndex);

      if (LMarker = 217) or (LMarker = 218) then
      begin
        Break;
      end;

      if (LMarker = 1) or (LMarker in [208..216]) then
      begin
        Continue;
      end;

      if LIndex + 1 >= Length(ABytes) then
      begin
        raise ENyxImage.Create('Truncated JPEG segment');
      end;
      LLength := ABytes[LIndex] * 256 + ABytes[LIndex + 1];

      if (LLength < 2) or (LLength > Length(ABytes) - LIndex) then
      begin
        raise ENyxImage.Create('Invalid JPEG segment length');
      end;

      if LMarker in [192, 193, 194] then
      begin

        if (LLength < 11) or (ABytes[LIndex + 2] <> 8) then
        begin
          raise ENyxImage.Create('JPEG requires an eight-bit frame header');
        end;
        LHeight := ABytes[LIndex + 3] * 256 + ABytes[LIndex + 4];
        LWidth := ABytes[LIndex + 5] * 256 + ABytes[LIndex + 6];
        Break;
      end;
      Inc(LIndex, LLength);
    end;
  end;

  if (LWidth < 1) or (LHeight < 1) or (LWidth > NyxImageMaximumDimension) or
    (LHeight > NyxImageMaximumDimension) or
    (LWidth * LHeight > NyxImageMaximumPixels) then
  begin
    raise ENyxImage.Create('Embedded image dimensions exceed the raster allocation budget');
  end;
  AWidth := Trunc(LWidth);
  AHeight := Trunc(LHeight);
end;

class function TNyxImageSource.FromWire(const AWire: TNyxText): TNyxImageSource;
var
  LFormat: TNyxImageFormat;
  LBytes: TNyxImageBytes;
  LSource: TNyxImageSource;
  LPrefix: TNyxText;
begin
  { Managed return slots can alias the caller's current source in native FPC.
    Build locally and publish only on success, including deliberate empty input. }
  LSource := Default(TNyxImageSource);

  if AWire = '' then
  begin
    Result := LSource;
    Exit;
  end;

  if (Pos(#0, AWire) > 0) or (Length(AWire) > 1400000) then
  begin
    raise ENyxImage.Create('Invalid image source text');
  end;
  LSource.FWire := AWire;
  LSource.FKind := nisLocation;
  for LFormat := Low(TNyxImageFormat) to High(TNyxImageFormat) do
  begin
    LPrefix := '';

    if LowerCase(Copy(AWire, 1, Length(CPrefixes[LFormat]))) = CPrefixes[LFormat] then
    begin
      LPrefix := CPrefixes[LFormat];
    end
    else if LowerCase(Copy(AWire, 1, Length(CFramingPrefixes[LFormat]))) = CFramingPrefixes[LFormat] then
    begin
      LPrefix := CFramingPrefixes[LFormat];
      LSource.FValidation := NyxImageValidation.ContainerChecksums(False);
    end;

    if LPrefix <> '' then
    begin
      LSource.FKind := nisEmbedded;
      LSource.FFormat := LFormat;
      LSource.FPayload := Copy(AWire, Length(LPrefix) + 1, MaxInt);
      LBytes := DecodeBase64(LSource.FPayload);
      Dimensions(LFormat, LBytes, LSource.FValidation, LSource.FWidth, LSource.FHeight);
      Result := LSource;
      Exit;
    end;
  end;

  if Pos('data:', LowerCase(AWire)) = 1 then
  begin
    raise ENyxImage.Create('Embedded sources require explicit PNG/JPEG base64');
  end;
  Result := LSource;
end;

function TNyxImageSource.ToWire: TNyxText;
begin
  Result := FWire;
end;

function TNyxImageSource.Bytes: TNyxImageBytes;
begin

  if FKind <> nisEmbedded then
  begin
    raise ENyxImage.Create('Only embedded images expose bytes');
  end;
  Result := DecodeBase64(FPayload);
end;

function TNyxImageSource.Format: TNyxImageFormat;
begin

  if FKind <> nisEmbedded then
  begin
    raise ENyxImage.Create('Only embedded images expose a raster format');
  end;
  Result := FFormat;
end;

function TNyxImageSource.Width: Integer;
begin
  Format;
  Result := FWidth;
end;

function TNyxImageSource.Height: Integer;
begin
  Format;
  Result := FHeight;
end;

function TNyxImageSource.Encoded: TNyxText;
begin
  Format;
  Result := FPayload;
end;

function NyxImageLocation(const AName: TNyxText): TNyxImageLocation;
begin

  if (AName = '') or (Pos(#0, AName) > 0) or
    (Pos('data:', LowerCase(AName)) = 1) then
  begin
    raise ENyxImage.Create('Image locations require a nonempty open resource reference');
  end;
  Result.Name := AName;
end;

function NyxImage(const ALocation: TNyxImageLocation): TNyxImageSource;
begin
  NyxImageLocation(ALocation.Name);
  Result := TNyxImageSource.FromWire(ALocation.Name);
end;

function NyxNoImage: TNyxImageSource;
begin
  Result := Default(TNyxImageSource);
end;

function NyxEmbeddedImage(AFormat: TNyxImageFormat;
  const ABase64: TNyxText): TNyxImageSource;
begin
  Result := NyxEmbeddedImage(AFormat, ABase64, NyxImageValidation);
end;

function NyxEmbeddedImage(AFormat: TNyxImageFormat; const ABase64: TNyxText;
  const AValidation: TNyxImageValidationPolicy): TNyxImageSource;
var
  LPrefix: TNyxText;
begin
  LPrefix := CPrefixes[AFormat];

  if not AValidation.ChecksumsRequired then
  begin
    LPrefix := CFramingPrefixes[AFormat];
  end;
  Result := TNyxImageSource.FromWire(LPrefix + ABase64);
end;

function NyxEmbeddedImageBytes(AFormat: TNyxImageFormat;
  const ABytes: TNyxImageBytes): TNyxImageSource;
begin
  Result := NyxEmbeddedImageBytes(AFormat, ABytes, NyxImageValidation);
end;

function NyxEmbeddedImageBytes(AFormat: TNyxImageFormat; const ABytes: TNyxImageBytes;
  const AValidation: TNyxImageValidationPolicy): TNyxImageSource;
begin
  Result := NyxEmbeddedImage(AFormat, EncodeBase64(ABytes), AValidation);
end;

function NyxImageRectangle(ASourceWidth, ASourceHeight, ABoxWidth,
  ABoxHeight: Integer; AFit: TNyxImageFit; AHorizontal,
  AVertical: TNyxImageAnchor): TNyxImageRectangle;
const
  CPosition: array[TNyxImageAnchor] of Double = (0, 0.5, 1);
var
  LScale: Double;
begin
  Result := Default(TNyxImageRectangle);

  if (ASourceWidth <= 0) or (ASourceHeight <= 0) or
    (ABoxWidth <= 0) or (ABoxHeight <= 0) then
  begin
    Exit;
  end;
  Result.Width := ABoxWidth;
  Result.Height := ABoxHeight;

  if AFit <> nifStretch then
  begin
    LScale := 1;

    if AFit in [nifContain, nifShrink] then
    begin
      LScale := Min(ABoxWidth / ASourceWidth, ABoxHeight / ASourceHeight);

      if AFit = nifShrink then
      begin
        LScale := Min(1, LScale);
      end;
    end
    else if AFit = nifCover then
    begin
      LScale := Max(ABoxWidth / ASourceWidth, ABoxHeight / ASourceHeight);
    end;
    Result.Width := ASourceWidth * LScale;
    Result.Height := ASourceHeight * LScale;
  end;
  Result.Left := (ABoxWidth - Result.Width) * CPosition[AHorizontal];
  Result.Top := (ABoxHeight - Result.Height) * CPosition[AVertical];
end;

end.
