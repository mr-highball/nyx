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
  nyx.model, nyx.controls, nyx.codegen, nyx.source, nyx.schema, nyx.responsive,
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
  {$ifndef PAS2JS}LFile: TFileStream;{$endif}
begin
  LDocument := BuildNyxDocument;
  LRead := nil;
  LWorkspace := nil;
  LSession := nil;
  try
    Check(LDocument.Title = 'Image workshop', 'exact English semantic seed');
    LPNG := NyxEmbeddedImage(nimPNG, ImagePNG);
    LJPEG := NyxEmbeddedImage(nimJPEG, ImageJPEG);
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
    LDocument.Find('hero-image').Configure.ImageFit(nifContain).ImageHorizontal(niaCenter)
      .ImageVertical(niaCenter).WhenViewport(TNyxViewportWidth.Below(420))
      .ImageFit(nifCover).Done;
    LDocument.Find('hero-image').Configure.ForPlatform(npfNativeLCL)
      .ImageHorizontal(niaEnd).Done;
    LBefore := TNyxCodec.Encode(LDocument);
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('NyxEmbeddedImage(nimPNG', LSource) > 0) and
      (Pos('NyxEmbeddedImage(nimJPEG', LSource) > 0) and
      (Pos('.ImageFit(nifCover)', LSource) > 0), 'crafted source uses typed resources and fit');
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
    LSession := TNyxStudioSession.Create;
    LSession.Load(LBefore);
    LSession.ApplyPatch(ReadNyxDesignPatch(NyxArray([NyxObject([
      NyxField('op', NyxData('update')), NyxField('id', NyxData('hero-image')),
      NyxField('properties', NyxObject([NyxField('image-fit', NyxData('fill')),
        NyxField('src', NyxData(LJPEG.ToWire))]))])])));
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
    { Framing is admitted portably. A complete stream with a bad CRC exercises
      the independent native decoder candidate rather than truncated framing. }
    LBroken[High(LBroken)] := LBroken[High(LBroken)] xor 1;
    LBefore := LImage.Picture.Graphic;
    LView.Root.Find('hero-image').Configure.Source(NyxEmbeddedImageBytes(nimPNG, LBroken)).Done;
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
procedure BrowserControls;
var
  LDocument: TNyxDocument;
  LView: TNyxBrowserRenderer;
  LImage: TJSHTMLImageElement;
  LBefore: TJSHTMLElement;
begin
  LDocument := BuildNyxDocument;
  LView := TNyxBrowserRenderer.Create;
  try
    LView.Render(LDocument, LDocument.Pages[0], TJSHTMLElement(document.body));
    LImage := TJSHTMLImageElement(LView.ElementFor('hero-image'));
    Check(LImage.getAttribute('src') = NyxEmbeddedImage(nimPNG, ImagePNG).ToWire,
      'ordinary browser image retains exact portable source');
    LBefore := LImage;
    LView.Root.Find('hero-image').Configure.ImageFit(nifCover).ImageHorizontal(niaEnd).Done;
    LView.Sync;
    Check((LView.ElementFor('hero-image') = LBefore) and
      (LImage.style.getPropertyValue('object-fit') = 'cover') and
      (LImage.style.getPropertyValue('object-position') = '100% 50%'),
      'ordinary browser image retains face and applies typed crop/position');
    LView.Root.Find('hero-image').Configure.Source(NyxNoImage).Done;
    LView.Sync;
    Check(not LImage.hasAttribute('src'), 'empty browser source makes no surrounding-page request');
  finally
    LView.Free;
    LDocument.Free;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    Shared;
    {$ifdef PAS2JS}BrowserControls;{$else}NativeControls;{$endif}
    WriteLn('PASS / image presentation / ', GChecks, ' checks');
    {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'passed');{$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'failed');
      {$else}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
