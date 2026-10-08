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
program nyx_image_authoring_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.model, nyx.images, nyx.image.fixtures, nyx.generated.view
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LImage: TNyxNode;

procedure Check(ACondition: Boolean; const AReason: String);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
end;

begin
  try
    LDocument := BuildNyxDocument;
    try
      LImage := LDocument.Find('hero-image');
      Check((LDocument.Title = 'Image workshop') and (LImage <> nil),
        'Compiled image builder lost its semantic seed');
      Check(LImage.Prop(NyxAttributeName(atSource)) = NyxEmbeddedImage(nimJPEG, ImageJPEG).ToWire,
        'Compiled image lost exact JPEG bytes');
      Check(LImage.Prop(NyxAttributeName(atAlt)) = TNyxText('A colorful banner / 🌙 / ''quoted'''),
        'Compiled image lost exact Unicode alternative text');
      Check(LImage.Prop(NyxAttributeName(atImageFit)) = NyxImageFitName(nifCover), 'Compiled image lost cover fit');
      Check(LImage.Prop(NyxAttributeName(atImageHorizontal)) = NyxImageAnchorName(niaEnd), 'Compiled image lost end anchor');
      Check(LImage.Prop(NyxAttributeName(atImageVertical)) = NyxImageAnchorName(niaCenter), 'Compiled image lost center anchor');
      WriteLn('PASS / compiled image authoring / 6 checks');
      {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'passed');{$endif}
    finally
      LDocument.Free;
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'failed');{$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
