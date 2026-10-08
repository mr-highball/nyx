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


program nyx_resource_image_authoring_generated;
{$mode delphi}{$H+}{$codepage utf8}
uses
  SysUtils, nyx.text, nyx.model, nyx.images, nyx.image.fixtures,
  nyx.resources, nyx.binding.types, nyx.generated.view
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LBinding: TNyxBindingSpec;
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: String);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

begin
  try
    LDocument := BuildNyxDocument;
    try
      Check(LDocument.Title = 'Image workshop', 'Exact emitted builder retains semantic identity');
      Check(LDocument.Resources.Count = 1, 'Exact emitted builder retains one packed file');
      Check(LDocument.Resources.Definition(NyxResourceRef('cover'), NyxDefaultLocale).Image.ToWire =
        NyxEmbeddedImage(nimJPEG, ImageJPEG).ToWire, 'Exact emitted builder retains imported JPEG bytes');
      Check(LDocument.Find('hero-image').FindBinding(bpImage, LBinding), 'Exact emitted builder retains image binding');
      Check(LBinding.Source = bsResourceImage, 'Binding remains a specialized image family');
      Check((LBinding.ResourceImage.Reference.Name = 'cover') and LBinding.ResourceImage.Localized and
        not LBinding.ResourceImage.Locale.Defined, 'Explicit default pin survives compiler reconstruction');
      WriteLn('PASS / exact compiled resource image authoring / ', GChecks, ' checks');
      {$ifdef PAS2JS}
      document.body.setAttribute('data-test-result', 'passed');
      {$endif}
    finally
      LDocument.Free;
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
      {$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
