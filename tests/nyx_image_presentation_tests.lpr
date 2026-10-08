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

program nyx_image_presentation_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, Math, nyx.text, nyx.types, nyx.images, nyx.data, nyx.codec,
  nyx.model, nyx.controls, nyx.codegen, nyx.source, nyx.schema, nyx.responsive, nyx.resources,
  nyx.studio.session, nyx.studio.edits, nyx.studio.projects, nyx.generated.view, nyx.image.fixtures,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Interfaces, Forms, Controls, Graphics, ExtCtrls, Types, IntfGraphics,
    FPWritePNG, nyx.images.lcl, nyx.render.lcl;{$endif}

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Image presentation: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Keep a complete, admitted PNG frame but invalidate its compressed pixel stream.
  The Pascal raster writer emits IHDR, a nine-byte pHYs and then IDAT. Check that
  exact fixture layout before changing its first DEFLATE block; a writer change
  must fail visibly instead of corrupting an unrelated byte. Block type 3 is
  reserved by RFC 1951 section 3.2.3. The changed byte also invalidates IDAT's
  original CRC. Both physical decoders receive exactly this damaged input;
  neither this fixture nor a host load establishes which checks the host uses.
  Default admission now refuses its checksum. Framing-only admission remains
  explicit so the native host's independent decoder refusal is still exercised;
  a caller choosing it cannot assume host load establishes pixel integrity. }
function BrokenPNG: TNyxImageSource;
const
  CFirstIDATData = 62;
var
  LBytes: TNyxImageBytes;
begin
  LBytes := NyxEmbeddedImage(nimPNG, ImagePNG).Bytes;
  Check(Length(LBytes) > CFirstIDATData + 2, 'broken-pixel fixture has a complete stream');
  Check((LBytes[CFirstIDATData - 4] = Ord('I')) and
    (LBytes[CFirstIDATData - 3] = Ord('D')) and
    (LBytes[CFirstIDATData - 2] = Ord('A')) and
    (LBytes[CFirstIDATData - 1] = Ord('T')) and
    ((LBytes[CFirstIDATData] and $0F) = 8), 'broken-pixel fixture identifies the IDAT zlib header');
  LBytes[CFirstIDATData + 2] := $07;
  Result := NyxEmbeddedImageBytes(nimPNG, LBytes, NyxImageValidation.ContainerChecksums(False));
end;

procedure Shared;
var
  LDocument: TNyxDocument;
  LRead: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LPNG: TNyxImageSource;
  LJPEG: TNyxImageSource;
  LCopy: TNyxImageSource;
  LBytes: TNyxImageBytes;
  LImage: INyxImage;
  LSource: TNyxText;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LBad: TNyxText;
  LIndex: Integer;
  LCase: Integer;
  LRefused: Boolean;
  LFit: TNyxImageFit;
  LHorizontal: TNyxImageAnchor;
  LVertical: TNyxImageAnchor;
  LRect: TNyxImageRectangle;
  LPolicy: TNyxImageValidationPolicy;
  LResource: INyxResourceDefinition;
  LChunkLength: Integer;
  LChunkIndex: Integer;
  {$ifndef PAS2JS}LFile: TFileStream;{$endif}
begin
  LDocument := BuildNyxDocument;
  LRead := nil;
  LWorkspace := nil;
  LSession := nil;
  LResource := nil;
  try
    Check(LDocument.Title = 'Image workshop', 'exact English semantic seed');
    LPNG := NyxEmbeddedImage(nimPNG, ImagePNG);
    LJPEG := NyxEmbeddedImage(nimJPEG, ImageJPEG);
    LPolicy := Default(TNyxImageValidationPolicy);
    Check(LPolicy.ChecksumsRequired and LPNG.Validation.ChecksumsRequired,
      'default and standard values require container checksums');
    LPolicy := LPolicy.ContainerChecksums(False);
    LCopy := NyxEmbeddedImage(nimPNG, ImagePNG, LPolicy);
    Check(not LCopy.Validation.ChecksumsRequired and LPNG.Validation.ChecksumsRequired,
      'fluent policy derivation retains the independent default baseline');
    Check(LPolicy.ContainerChecksums(True).ChecksumsRequired and not LPolicy.ChecksumsRequired,
      're-enabling checksums derives a new value without changing the caller choice');
    Check(TNyxImageSource.FromWire(LCopy.ToWire).ToWire = LCopy.ToWire,
      'wire retains the exact explicit caller policy');
    LResource := NyxResourceFromData(NyxImageResource(LCopy).ToData);
    Check((LResource.Image.ToWire = LCopy.ToWire) and not LResource.Image.Validation.ChecksumsRequired,
      'packed resource persistence retains the image policy');
    LResource := nil;
    LBefore := LCopy.ToWire;
    LBytes := LPNG.Bytes;
    LChunkIndex := 8;
    while LChunkIndex < Length(LBytes) do
    begin
      { The independent Pascal raster writer supplies the known-good CRCs.
        Change each stored checksum separately, including ancillary and empty
        terminal chunks; never calculate expected CRCs with the product helper. }
      LChunkLength := LBytes[LChunkIndex] * 16777216 + LBytes[LChunkIndex + 1] * 65536 +
        LBytes[LChunkIndex + 2] * 256 + LBytes[LChunkIndex + 3];
      LBytes[LChunkIndex + 8 + LChunkLength] := LBytes[LChunkIndex + 8 + LChunkLength] xor 1;
      LRefused := False;
      try
        LCopy := NyxEmbeddedImageBytes(nimPNG, LBytes);
      except
        on ENyxImage do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (LCopy.ToWire = LBefore) and not LCopy.Validation.ChecksumsRequired,
        'every damaged chunk checksum refuses before overwriting the caller source');
      LResource := NyxImageResource(NyxEmbeddedImageBytes(nimPNG, LBytes, LPolicy));
      Check(not LResource.Image.Validation.ChecksumsRequired,
        'explicit framing-only caller choice retains even checksum-damaged bytes');
      LResource := nil;
      LBytes[LChunkIndex + 8 + LChunkLength] := LBytes[LChunkIndex + 8 + LChunkLength] xor 1;
      Inc(LChunkIndex, LChunkLength + 12);
    end;
    for LCase := 0 to 2 do
    begin
      case LCase of
        0: LBad := StringReplace(LCopy.ToWire, 'nyx-validation=framing', 'nyx-validation=unchecked', []);
        1: LBad := StringReplace(LCopy.ToWire, 'nyx-validation=framing',
          'nyx-validation=framing;nyx-validation=framing', []);
        2: LBad := StringReplace(LCopy.ToWire, ';base64,', ';base64;nyx-validation=framing,', []);
      end;
      LRefused := False;
      try
        LCopy := TNyxImageSource.FromWire(LBad);
      except
        on ENyxImage do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (LCopy.ToWire = LBefore),
        'unknown, duplicated or misplaced wire policy refuses atomically');
    end;
    Check((LPNG.Width = 100) and (LPNG.Height = 50) and
      (LJPEG.Width = 100) and (LJPEG.Height = 50), 'PNG/JPEG encoded dimensions');
    Check(LDocument.Find('hero-image').Prop('src') = LPNG.ToWire,
      'semantic authored resource equals Pascal-produced PNG');
    LCopy := LPNG;
    LBytes := LPNG.Bytes;
    LBytes[0] := 0;
    Check((LCopy.Bytes[0] = 137) and (LPNG.Bytes[0] = 137), 'byte reads never alias source copies');
    for LCase := 0 to 2 do
    begin
      { Insert a legal JPEG comment segment after SOI. This varies the encoded
        tail without adding bytes outside the admitted image container. }
      LBytes := LJPEG.Bytes;
      SetLength(LBytes, Length(LBytes) + LCase + 4);
      for LIndex := High(LBytes) downto LCase + 6 do
      begin
        LBytes[LIndex] := LBytes[LIndex - LCase - 4];
      end;
      LBytes[2] := 255;
      LBytes[3] := 254;
      LBytes[4] := 0;
      LBytes[5] := LCase + 2;
      for LIndex := 6 to LCase + 5 do
      begin
        LBytes[LIndex] := 0;
      end;
      LCopy := NyxEmbeddedImageBytes(nimJPEG, LBytes);
      Check(Length(LCopy.Bytes) = Length(LBytes), 'all base64 tail lengths round-trip exact bytes');
    end;
    for LCase := 0 to 13 do
    begin
      LRefused := False;
      try
        case LCase of
          0: NyxEmbeddedImage(nimJPEG, ImagePNG);
          1: NyxEmbeddedImage(nimPNG, ImageJPEG);
          2: NyxEmbeddedImage(nimPNG, 'abc');
          3: NyxEmbeddedImage(nimPNG, ImagePNG + #10);
          4: NyxEmbeddedImage(nimPNG, Copy(ImagePNG, 1, 4) + '=' + Copy(ImagePNG, 6, MaxInt));
          5: NyxEmbeddedImage(nimPNG, 'AB==');
          6: TNyxImageSource.FromWire('data:image/svg+xml;base64,AAAA');
          7: NyxImage(Default(TNyxImageLocation));
          8: ReadNyxImageFit('magic');
          9: ReadNyxImageAnchor('left');
          10:
            begin
              LBytes := LPNG.Bytes;
              LBytes[16] := 1;
              NyxEmbeddedImageBytes(nimPNG, LBytes);
            end;
          11:
            begin
              LBytes := LPNG.Bytes;
              SetLength(LBytes, 33);
              NyxEmbeddedImageBytes(nimPNG, LBytes);
            end;
          12:
            begin
              LBytes := LPNG.Bytes;
              SetLength(LBytes, Length(LBytes) + 1);
              NyxEmbeddedImageBytes(nimPNG, LBytes);
            end;
          13:
            begin
              LBytes := LPNG.Bytes;
              LBytes[33] := 255;
              NyxEmbeddedImageBytes(nimPNG, LBytes);
            end;
        end;
      except
        on ENyxImage do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'strict source/geometry boundary refuses invalid values');
    end;
    LBytes := LPNG.Bytes;
    SetLength(LBytes, NyxImageMaximumBytes + 1);
    LRefused := False;
    try
      NyxEmbeddedImageBytes(nimPNG, LBytes);
    except
      on ENyxImage do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'byte budget refuses before resource retention');
    for LFit := Low(TNyxImageFit) to High(TNyxImageFit) do
    begin
      for LHorizontal := Low(TNyxImageAnchor) to High(TNyxImageAnchor) do
      begin
        for LVertical := Low(TNyxImageAnchor) to High(TNyxImageAnchor) do
        begin
          LRect := NyxImageRectangle(100, 50, 100, 100, LFit, LHorizontal, LVertical);
          Check((LRect.Width > 0) and (LRect.Height > 0), 'all fits/anchors admit ordinary geometry');

          if LFit = nifCover then
          begin
            Check((LRect.Width >= 100) and (LRect.Height >= 100), 'cover fills the complete box');
          end
          else if LFit = nifStretch then
          begin
            Check((LRect.Width = 100) and (LRect.Height = 100), 'stretch uses the complete box');
          end
          else
          begin
            Check((LRect.Width <= 100) and (LRect.Height <= 100), 'contain/natural/shrink retain admitted aspect');
          end;

          if LFit <> nifStretch then
          begin
            Check(SameValue(LRect.Width / LRect.Height, 2), 'non-stretch preserves encoded aspect ratio');
          end;
        end;
      end;
    end;
    LRect := NyxImageRectangle(100, 50, 200, 100, nifShrink);
    Check((LRect.Left = 50) and (LRect.Top = 25) and (LRect.Width = 100),
      'scale-down never enlarges natural pixels');
    LRect := NyxImageRectangle(0, 50, 100, 100, nifCover);
    Check((LRect.Width = 0) and (LRect.Height = 0), 'empty source produces empty geometry');
    LImage := NewNyxImage('typed-image');
    LImage.Image := LJPEG;
    LImage.ImageFit := nifCover;
    LImage.ImageHorizontal := niaEnd;
    Check((LImage.Image.Format = nimJPEG) and (LImage.ImageFit = nifCover) and
      (LImage.ImageHorizontal = niaEnd), 'specialized managed Image exposes typed properties');
    LImage.Configure.Clear(atImageFit).Clear(atImageHorizontal).Clear(atImageVertical).Done;
    Check((LImage.ImageFit = nifContain) and (LImage.ImageHorizontal = niaCenter) and
      (LImage.ImageVertical = niaCenter), 'explicit Clear restores specialized image defaults');
    LDocument.Pages[0].Add(LImage);
    LImage := NewNyxImage('typed-policy-image');
    LImage.Configure.Source(NyxEmbeddedImage(nimPNG, ImagePNG, LPolicy))
      .AlternativeText('A banner with an explicit admission policy').Width(100).Height(50).Done;
    LDocument.Pages[0].Add(LImage);
    LDocument.Find('hero-image').Configure.ImageFit(nifContain).ImageHorizontal(niaCenter)
      .ImageVertical(niaCenter).WhenViewport(TNyxViewportWidth.Below(420))
      .ImageFit(nifCover).Done;
    LDocument.Find('hero-image').Configure.ForPlatform(npfNativeLCL)
      .ImageHorizontal(niaEnd).Done;
    LBefore := TNyxCodec.Encode(LDocument);
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('NyxEmbeddedImage(nimPNG', LSource) > 0) and
      (Pos('NyxEmbeddedImage(nimJPEG', LSource) > 0) and
      (Pos('.ImageFit(nifCover)', LSource) > 0) and
      (Pos('NyxImageValidation.ContainerChecksums(False)', LSource) > 0),
      'crafted source uses typed resources, fit and explicit fluent policy');
    LRead := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = LBefore, 'exact source reconstructs image bytes/scopes/tree');
    LRead.Free;
    LRead := nil;
    LWorkspace.Free;
    LWorkspace := nil;
    LBad := StringReplace(LSource, '.ImageFit(nifCover)', '.ImageFit(''cover'')', []);
    LRefused := False;
    try
      LRead := TNyxSourceWorkspace.PrepareDraft(LBad, LWorkspace);
    except
      on ENyxSource do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'source admission refuses string-driven behavior');
    LRead.Free;
    LRead := nil;
    LWorkspace.Free;
    LWorkspace := nil;
    LBad := StringReplace(LSource, 'ContainerChecksums(False)', 'ContainerChecksums(''False'')', []);
    LRefused := False;
    try
      LRead := TNyxSourceWorkspace.PrepareDraft(LBad, LWorkspace);
    except
      on ENyxSource do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'managed source refuses a string instead of the typed policy Boolean');
    LRead.Free;
    LRead := nil;
    LWorkspace.Free;
    LWorkspace := nil;
    LSession := TNyxStudioSession.Create;
    LSession.Load(LBefore);
    LSession.ApplyPatch(ReadNyxDesignPatch(NyxArray([NyxObject([
      NyxField('op', NyxData('update')), NyxField('id', NyxData('hero-image')),
      NyxField('properties', NyxObject([NyxField('image-fit', NyxData('fill')),
        NyxField('src', NyxData(NyxEmbeddedImage(nimJPEG, ImageJPEG, LPolicy).ToWire))]))])])));
    LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    LSession.Undo;
    Check(TNyxCodec.Encode(LSession.Document) = LBefore, 'one Undo restores exact image resources');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter, 'one Redo restores exact changed pair');
    LRefused := False;
    try
      LSession.ApplyPatch(ReadNyxDesignPatch(NyxArray([NyxObject([
        NyxField('op', NyxData('update')), NyxField('id', NyxData('hero-image')),
        NyxField('properties', NyxObject([NyxField('src', NyxData('data:image/png;base64,AB=='))]))])])));
    except
      on ENyxImage do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LAfter),
      'malformed source preserves complete accepted pair');
    {$ifndef PAS2JS}

    if ParamCount > 0 then
    begin
      LSource := TNyxCodegen.Generate(LDocument, 'nyx.generated.images');
      LFile := TFileStream.Create(ParamStr(1), fmCreate);
      try
        LFile.WriteBuffer(LSource[1], Length(LSource));
      finally
        LFile.Free;
      end;
    end;
    {$endif}
  finally
    LImage := nil;
    LResource := nil;
    LSession.Free;
    LWorkspace.Free;
    LRead.Free;
    LDocument.Free;
  end;
end;

{$ifndef PAS2JS}
procedure NativeControls;
var
  LWindow: TForm;
  LDocument: TNyxDocument;
  LView: TNyxLCLRenderer;
  LImage: TNyxLCLImage;
  LPart: TNyxNode;
  LBefore: TGraphic;
  LControl: TControl;
  LFit: TNyxImageFit;
  LRect: TRect;
  LOriginalBounds: TRect;
  LRefused: Boolean;
  LBroken: TNyxImageBytes;
  LFailure: Integer;
  LBitmap: TBitmap;
  LCapture: TLazIntfImage;

  procedure Capture(const APath: String);
  begin
    LWindow.Repaint;
    Application.ProcessMessages;
    LBitmap := TBitmap.Create;
    LBitmap.SetSize(LWindow.ClientWidth, LWindow.ClientHeight);
    LWindow.PaintTo(LBitmap.Canvas, 0, 0);
    LCapture := LBitmap.CreateIntfImage;
    LCapture.SaveToFile(APath);
    FreeAndNil(LCapture);
    FreeAndNil(LBitmap);
  end;

begin
  LWindow := TForm.CreateNew(nil);
  LDocument := BuildNyxDocument;
  LView := TNyxLCLRenderer.Create;
  LBitmap := nil;
  LCapture := nil;
  try
    LWindow.SetBounds(40, 40, 900, 820);
    LWindow.Show;
    LView.Render(LDocument, LDocument.Pages[0], LWindow);
    Application.ProcessMessages;
    LImage := TNyxLCLImage(LView.ControlFor('hero-image'));
    LOriginalBounds := LImage.BoundsRect;
    Check((LImage.Picture.Width = 100) and (LImage.Picture.Height = 50),
      'ordinary native image decodes the exact semantic PNG');
    Check(LImage.AccessibleDescription = 'Red and blue sample banner', 'native alternative text is retained');
    LView.Root.Find('hero-image').Configure.Source(
      NyxEmbeddedImage(nimPNG, ImagePNG, NyxImageValidation.ContainerChecksums(False))).Done;
    LView.Sync;
    Check((LImage.Picture.Width = 100) and
      not TNyxImageSource.FromWire(LView.Root.Find('hero-image').Prop('src')).Validation.ChecksumsRequired,
      'ordinary native decoding consumes the persisted explicit policy');
    LControl := LImage;
    LBefore := LImage.Picture.Graphic;
    for LFit := Low(TNyxImageFit) to High(TNyxImageFit) do
    begin
      LView.Root.Find('hero-image').Configure.ImageFit(LFit).Done;
      LView.Sync;
      LImage.SetBounds(10, 10, 100, 100);
      LRect := LImage.DestRect;

      if LFit = nifCover then
      begin
        Check((LRect.Left = -50) and (LRect.Top = 0) and (LRect.Right = 150),
          'actual LCL cover crops centered encoded pixels');
      end
      else if LFit = nifStretch then
      begin
        Check((LRect.Left = 0) and (LRect.Top = 0) and
          (LRect.Right = 100) and (LRect.Bottom = 100), 'actual LCL stretch fills the face');
      end
      else
      begin
        Check((LRect.Left = 0) and (LRect.Top = 25) and
          (LRect.Right = 100) and (LRect.Bottom = 75), 'actual LCL aspect/natural rectangle');
      end;
      Check((LView.ControlFor('hero-image') = LControl) and
        (LImage.Picture.Graphic = LBefore), 'policy-only sync retains physical image and decoded picture');
    end;
    LView.Root.Find('hero-image').Configure.Source(NyxEmbeddedImage(nimJPEG, ImageJPEG))
      .ImageFit(nifCover).ImageHorizontal(niaEnd).Done;
    LView.Sync;
    Check((LImage.Picture.Width = 100) and (LImage.Picture.Height = 50),
      'ordinary native image decodes the exact JPEG');
    LImage.SetBounds(10, 10, 100, 100);
    LRect := LImage.DestRect;
    Check((LRect.Left = -100) and (LRect.Right = 100), 'end anchor positions negative crop space');
    LView.Root.Find('hero-image').Configure.Clear(atImageFit)
      .Clear(atImageHorizontal).Clear(atImageVertical).Done;
    LView.Sync;
    LImage.SetBounds(10, 10, 100, 100);
    LRect := LImage.DestRect;
    Check((LRect.Left = 0) and (LRect.Top = 25) and (LRect.Right = 100) and
      (LRect.Bottom = 75), 'ordinary native Clear restores contain and centered anchors');
    LPart := LView.Root.Find(NyxQualifiedID('feature', 'feature-card'));
    Check(LPart <> nil, 'qualified reusable root is mounted independently');
    LPart := LPart.Part(NyxPart('media'));
    Check(TImage(LView.ControlFor(LPart.ID)).Picture.Width = 100,
      'reusable compound media part resolves the same portable picture');
    LBroken := NyxEmbeddedImage(nimPNG, ImagePNG).Bytes;
    { Preserve the original CRC refusal, then exercise the same bad compressed
      pixel stream used by the asynchronous browser journey. Neither replaces
      the admitted physical picture/source baseline on a native failure. }
    LBroken[High(LBroken)] := LBroken[High(LBroken)] xor 1;
    for LFailure := 0 to 1 do
    begin
      LBefore := LImage.Picture.Graphic;

      if LFailure = 0 then
      begin
        LView.Root.Find('hero-image').Configure.Source(NyxEmbeddedImageBytes(nimPNG, LBroken,
          NyxImageValidation.ContainerChecksums(False))).Done;
      end
      else
      begin
        LView.Root.Find('hero-image').Configure.Source(BrokenPNG).Done;
      end;
      LRefused := False;
      try
        LView.Sync;
      except
        on Exception do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (LImage.Picture.Graphic = LBefore) and
        (LView.ControlFor('hero-image') = LControl),
        'ordinary Sync decoder failure retains the exact physical picture and face');
    end;
    LView.Root.Find('hero-image').Configure.Source(NyxEmbeddedImage(nimJPEG, ImageJPEG)).Done;
    LView.Sync;
    Check(LImage.Picture.Graphic = LBefore, 'return to accepted source retains its exact decoding baseline');
    { Geometry probes temporarily position the physical control. Restore its
      ordinary arranged rectangle before the visual acceptance captures. }
    LImage.BoundsRect := LOriginalBounds;

    if ParamCount > 1 then
    begin
      Capture(ParamStr(2));
    end;

    if ParamCount > 2 then
    begin
      LWindow.ClientWidth := 390;
      Application.ProcessMessages;
      Capture(ParamStr(3));
    end;
    LView.Root.Find('hero-image').Configure.Source(NyxNoImage).Done;
    LView.Sync;
    Check(LImage.Picture.Width = 0, 'explicit NoImage clears native picture');
  finally
    LCapture.Free;
    LBitmap.Free;
    LView.Free;
    LDocument.Free;
    LWindow.Free;
  end;
end;
{$else}
{ Await actual host decoding with a real five-second deadline. The timer owns
  only its Promise rejection callback; it never borrows a document or renderer.
  Clear it on every outcome so successful decodes leave no delayed work behind. }
function DecodeImage(AImage: TJSHTMLImageElement): JSValue; async;
var
  LTimer: NativeInt;

  procedure Deadline(AResolve, AReject: TJSPromiseResolver);

    procedure Expired;
    begin
      AReject(TJSError.new('Image decoding exceeded its five-second deadline'));
    end;

  begin
    LTimer := window.setTimeout(@Expired, 5000);
  end;

begin
  Result := Undefined;
  LTimer := 0;
  try
    await(TJSPromise.race([AImage.decode, TJSPromise.new(@Deadline)]));
  finally
    window.clearTimeout(LTimer);
  end;
end;

{ The host can keep its previous completely available request while a replacement
  is pending. decode() alone can therefore succeed for the previous pixels. Own
  load/error listeners across the typed source publication, then qualify request
  identity before decoding. This is a target-input harness, not a second image
  component or proof of the still-open portable image callback contract. }
function ChangeImageSource(AView: TNyxBrowserRenderer; AImage: TJSHTMLImageElement;
  const ASource: TNyxImageSource): JSValue; async;
var
  LTimer: NativeInt;
  LLoaded: TJSRawEventHandler;
  LFailed: TJSRawEventHandler;
  LResolve: TJSPromiseResolver;
  LReady: TJSPromise;
  LSuccess: Boolean;

  procedure Loaded(AEvent: TJSEvent);
  begin
    LResolve(True);
  end;

  procedure Failed(AEvent: TJSEvent);
  begin
    LResolve(False);
  end;

  procedure Start(AResolve, AReject: TJSPromiseResolver);

    procedure Expired;
    begin
      AReject(TJSError.new('Image source change exceeded its five-second deadline'));
    end;

  begin
    LResolve := AResolve;
    LTimer := window.setTimeout(@Expired, 5000);
  end;

begin
  LTimer := 0;
  LLoaded := @Loaded;
  LFailed := @Failed;
  LReady := TJSPromise.new(@Start);
  try
    AImage.addEventListener('load', LLoaded);
    AImage.addEventListener('error', LFailed);
    AView.Root.Find('hero-image').Configure.Source(ASource).Done;
    AView.Sync;
    Check(AImage.getAttribute('src') = ASource.ToWire,
      'the typed replacement reaches the actual browser source attribute');
    LSuccess := JSValue(await(TJSPromise.resolve(LReady))) = True;
  finally
    AImage.removeEventListener('load', LLoaded);
    AImage.removeEventListener('error', LFailed);
    window.clearTimeout(LTimer);
  end;

  if LSuccess then
  begin
    Check(AImage.currentSrc = ASource.ToWire, 'the loaded host request is the typed replacement');
    await(DecodeImage(AImage));
  end;
  Result := LSuccess;
end;

{ Sample the decoded ordinary image through a detached canvas. This verifies
  encoded pixels rather than inferring readiness from src/complete/dimensions.
  JPEG tolerates channel quantization; neither sample touches its boundary.
  The canvas is never a replacement component and retains no Pascal owner. }
procedure DecodedPixels(AImage: TJSHTMLImageElement; const AFormat: TNyxText);
var
  LCanvas: TJSHTMLCanvasElement;
  LContext: TJSCanvasRenderingContext2D;
  LPixels: TJSUint8ClampedArray;
  LRed: Integer;
  LBlue: Integer;
begin
  Check(AImage.currentSrc = AImage.getAttribute('src'),
    AFormat + ' pixels belong to the requested source');
  Check((AImage.naturalWidth = 100) and (AImage.naturalHeight = 50),
    AFormat + ' is actually decoded at its encoded dimensions');
  LCanvas := TJSHTMLCanvasElement(document.createElement('canvas'));
  LCanvas.width := 100;
  LCanvas.height := 50;
  try
    LContext := LCanvas.getContextAs2DContext('2d');
    Check(LContext <> nil, 'decoded-pixel context is available');
    LContext.drawImage(AImage, 0, 0);
    LPixels := LContext.getImageData(0, 0, 100, 50).data;
    LRed := (25 * 100 + 25) * 4;
    LBlue := (25 * 100 + 75) * 4;
    Check((LPixels[LRed] > 240) and (LPixels[LRed + 1] < 16) and
      (LPixels[LRed + 2] < 16) and (LPixels[LRed + 3] = 255), AFormat + ' decodes red pixels');
    Check((LPixels[LBlue] < 16) and (LPixels[LBlue + 1] < 16) and
      (LPixels[LBlue + 2] > 240) and (LPixels[LBlue + 3] = 255), AFormat + ' decodes blue pixels');
  finally
    { Release the transient pixel surface promptly rather than keeping a second
      rendering alive until the browser eventually collects this local object. }
    LCanvas.width := 0;
    LCanvas.height := 0;
  end;
end;

{ Opt-in capture uses the maintained observer's acknowledgement and a monotonic
  deadline. Actual decoded controls stay mounted until PNG/DOM are saved. Manual
  execution finishes directly; this handshake does not change application state. }
function CaptureScene(const AName: TNyxText): JSValue; async;
var
  LStarted: Double;

  function Pause: TJSPromise;

    procedure Start(AResolve, AReject: TJSPromiseResolver);

      procedure Complete;
      begin
        AResolve(Undefined);
      end;

    begin
      window.setTimeout(@Complete, 25);
    end;

  begin
    Result := TJSPromise.new(@Start);
  end;

begin
  Result := Undefined;

  if Pos('capture=1', window.location.search) = 0 then
  begin
    Exit;
  end;
  document.body.setAttribute('data-capture-checkpoint', AName);
  LStarted := window.performance.now;
  while document.body.getAttribute('data-capture-observed') <> AName do
  begin
    if window.performance.now - LStarted >= 30000 then
    begin
      raise Exception.Create('Decoded image capture was not acknowledged before its deadline');
    end;
    await(Pause);
  end;
end;

function BrowserControls: JSValue; async;
var
  LDocument: TNyxDocument;
  LView: TNyxBrowserRenderer;
  LImage: TJSHTMLImageElement;
  LReusable: TJSHTMLImageElement;
  LPart: TNyxNode;
  LBefore: TJSHTMLElement;
  LPending: TJSPromise;
  LRefused: Boolean;
  LBrokenSource: TNyxImageSource;
  LAcceptedSource: TNyxText;
  LAdmissionRefused: Boolean;
  LProbeBytes: TNyxImageBytes;
  LProbeCanvas: TJSHTMLCanvasElement;
  LProbePixels: TJSUint8ClampedArray;
  LProbe: TNyxDataValue;
begin
  Result := Undefined;
  LDocument := BuildNyxDocument;
  LView := TNyxBrowserRenderer.Create;
  try
    LView.Render(LDocument, LDocument.Pages[0], TJSHTMLElement(document.body));
    LImage := TJSHTMLImageElement(LView.ElementFor('hero-image'));
    Check(LImage.getAttribute('src') = NyxEmbeddedImage(nimPNG, ImagePNG).ToWire,
      'ordinary browser image retains exact portable source');
    LBefore := LImage;
    await(DecodeImage(LImage));
    DecodedPixels(LImage, 'PNG');
    LPart := LView.Root.Find(NyxQualifiedID('feature', 'feature-card'));
    Check(LPart <> nil, 'browser reusable media root is independently mounted');
    LPart := LPart.Part(NyxPart('media'));
    Check(LPart <> nil, 'browser reusable media slot remains named');
    LReusable := TJSHTMLImageElement(LView.ElementFor(LPart.ID));
    Check(LReusable <> LImage, 'reusable image has an independent physical face');
    await(DecodeImage(LReusable));
    DecodedPixels(LReusable, 'Reusable PNG');
    LView.Root.Find('hero-image').Configure.Width(100).Height(100)
      .ImageFit(nifCover).ImageHorizontal(niaEnd).Done;
    LView.Sync;
    Check((LView.ElementFor('hero-image') = LBefore) and
      (LImage.style.getPropertyValue('object-fit') = 'cover') and
      (LImage.style.getPropertyValue('object-position') = '100% 50%'),
      'ordinary browser image retains face and applies typed crop/position');
    Check((LImage.getBoundingClientRect.width = 100) and
      (LImage.getBoundingClientRect.height = 100), 'crop applies inside the actual square allocation');
    await(CaptureScene('media-crop'));
    Check(Boolean(await(ChangeImageSource(LView, LImage,
      NyxEmbeddedImage(nimPNG, ImagePNG, NyxImageValidation.ContainerChecksums(False))))),
      'the ordinary browser loads the explicit caller policy data URL');
    DecodedPixels(LImage, 'Explicit framing PNG');
    Check(not TNyxImageSource.FromWire(LView.Root.Find('hero-image').Prop('src')).Validation.ChecksumsRequired,
      'the actual browser source retains its explicit policy');
    Check(Boolean(await(ChangeImageSource(LView, LImage, NyxEmbeddedImage(nimJPEG, ImageJPEG)))),
      'the browser reports loading the replacement JPEG');
    DecodedPixels(LImage, 'JPEG');
    Check((LView.ElementFor('hero-image') = LBefore) and
      (LReusable.getAttribute('src') = NyxEmbeddedImage(nimPNG, ImagePNG).ToWire),
      'source replacement retains its face and independent reusable pixels');
    LBrokenSource := BrokenPNG;
    LAcceptedSource := LView.Root.Find('hero-image').Prop('src');
    LAdmissionRefused := False;
    try
      LView.Root.Find('hero-image').Configure.Source(
        NyxEmbeddedImageBytes(nimPNG, LBrokenSource.Bytes)).Done;
    except
      on ENyxImage do
      begin
        LAdmissionRefused := True;
      end;
    end;
    Check(LAdmissionRefused and
      (LView.Root.Find('hero-image').Prop('src') = LAcceptedSource) and
      (LImage.currentSrc = LAcceptedSource) and (LView.ElementFor('hero-image') = LBefore),
      'standard admission refuses damaged bytes before replacing the accepted document/control source');
    DecodedPixels(LImage, 'Retained JPEG after admission refusal');
    { The caller can deliberately relax checksum admission. Qualify its host
      outcome without asserting that load means valid pixels; native and browser
      decoders may refuse, partially paint or accept the same damaged stream.
      The old default failure is repaired at shared admission, not hidden by
      treating this explicitly unchecked request as portable integrity proof. }
    LRefused := not Boolean(await(ChangeImageSource(LView, LImage, LBrokenSource)));
    LProbeBytes := LBrokenSource.Bytes;
    LProbe := NyxNull;

    if not LRefused then
    begin
      { Negative evidence must identify the actual bytes and painted sample,
        not infer decoder strictness from a positive load or encoded dimensions. }
      LProbeCanvas := TJSHTMLCanvasElement(document.createElement('canvas'));
      LProbeCanvas.width := 100;
      LProbeCanvas.height := 50;
      try
        LProbeCanvas.getContextAs2DContext('2d').drawImage(LImage, 0, 0);
        LProbePixels := LProbeCanvas.getContextAs2DContext('2d').getImageData(25, 25, 1, 1).data;
        LProbe := NyxArray([NyxData(Integer(LProbePixels[0])), NyxData(Integer(LProbePixels[1])),
          NyxData(Integer(LProbePixels[2])), NyxData(Integer(LProbePixels[3]))]);
      finally
        LProbeCanvas.width := 0;
        LProbeCanvas.height := 0;
      end;
    end;
    { Preserve bounded host outcome even when the enclosing finally retires its
      controls before a failure capture. No image bytes or model are exported. }
    document.body.setAttribute('data-image-refusal', NyxObject([
      NyxField('refused', NyxData(LRefused)),
      NyxField('standardAdmissionRefused', NyxData(LAdmissionRefused)),
      NyxField('hostContainerChecksumsRequired', NyxData(LBrokenSource.Validation.ChecksumsRequired)),
      NyxField('encodedDeflateByte', NyxData(Integer(LProbeBytes[64]))),
      NyxField('paintedSample', LProbe),
      NyxField('currentRequestMatchesSource', NyxData(LImage.currentSrc = LImage.getAttribute('src'))),
      NyxField('width', NyxData(Double(LImage.naturalWidth))),
      NyxField('height', NyxData(Double(LImage.naturalHeight))),
      NyxField('complete', NyxData(LImage.complete)),
      NyxField('retainedFace', NyxData(LView.ElementFor('hero-image') = LBefore))]).ToJSON);
    Check((LView.ElementFor('hero-image') = LBefore) and
      not LBrokenSource.Validation.ChecksumsRequired,
      'caller framing-only choice retains its face without promising host integrity');
    LView.Root.Find('hero-image').Configure.Clear(atImageFit).Clear(atImageHorizontal).Clear(atImageVertical).Done;
    Check(Boolean(await(ChangeImageSource(LView, LImage, NyxEmbeddedImage(nimJPEG, ImageJPEG)))),
      'the browser reports loading the corrected JPEG');
    DecodedPixels(LImage, 'Recovered JPEG');
    Check((LImage.style.getPropertyValue('object-fit') = 'contain') and
      (LImage.style.getPropertyValue('object-position') = '50% 50%') and
      (LReusable.naturalWidth = 100), 'recovery restores defaults without affecting the reusable instance');
    await(CaptureScene('media-recovered'));
    LPending := LImage.decode;
    LView.Root.Find('hero-image').Configure.Source(NyxNoImage).Done;
    LView.Sync;
    Check(not LImage.hasAttribute('src'), 'empty browser source makes no surrounding-page request');
    LRefused := False;
    try
      { pas2js awaits an external Promise-producing call. resolve assimilates
        this already-started request; it does not start another image decode. }
      await(TJSPromise.resolve(LPending));
    except
      LRefused := String(TJSObject(JSExceptValue).Properties['name']) = 'EncodingError';
    end;
    Check(LRefused and not LImage.hasAttribute('src') and
      (LView.ElementFor('hero-image') = LBefore), 'clearing invalidates pending decoding while retaining the empty face');
  finally
    LView.Free;
    LDocument.Free;
    document.body.setAttribute('data-image-disposed', 'true');
  end;
end;
{$endif}

{$ifdef PAS2JS}
{ Keep readiness behind the complete asynchronous journey. Handle both Pascal
  assertions and raw host Promise failures so the driver sees a useful terminal
  failure instead of a swallowed rejection or a premature script-loaded pass. }
function RunBrowser: JSValue; async;
begin
  Result := Undefined;
  try
    Shared;
    await(BrowserControls);
    WriteLn('PASS / image presentation / ', GChecks, ' checks');
    document.body.setAttribute('data-image-checks', IntToStr(GChecks));
    document.body.setAttribute('data-test-result', 'passed');
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
    end
    else
    begin
      document.body.setAttribute('data-event-error', 'Host image review failed: ' +
        String(TJSObject(JSExceptValue).Properties['message']));
      document.body.setAttribute('data-test-result', 'failed');
    end;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}RunBrowser;{$else}
  try
    Application.Initialize;
    Shared;
    NativeControls;
    WriteLn('PASS / image presentation / ', GChecks, ' checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
