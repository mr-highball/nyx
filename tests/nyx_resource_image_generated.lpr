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
program nyx_resource_image_generated;

{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, nyx.model, nyx.types, nyx.resources, nyx.resource.sources,
  nyx.images, nyx.binding.types, nyx.codec, nyx.data, nyx.generated.resource.images
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LDefinition: INyxResourceDefinition;
  LSpec: TNyxBindingSpec;
  LCount: Integer;

procedure Check(ACondition: Boolean; const AReason: String);
begin

  if not ACondition then
  begin
    raise Exception.Create('Compiled image resource: ' + AReason);
  end;
  Inc(LCount);
end;

begin
  LDocument := BuildNyxDocument;
  try
    LDocument.Validate;
    Check(TNyxDataValue.ParseJSON(TNyxCodec.Encode(LDocument)).Field('version').AsInteger = 10,
      'wire version');
    LSpec := LDocument.Find('hero-image').Bindings[0];
    Check(LSpec.Source = bsResourceImage, 'specialized binding family');
    Check(LSpec.ResourceImage.Localized and not LSpec.ResourceImage.Locale.Defined,
      'explicit fixed default locale');
    LSpec := LDocument.Find('feature-card-part-1').Bindings[0];
    Check((LSpec.Source = bsResourceImage) and not LSpec.ResourceImage.Localized,
      'reusable image follows runtime locale');
    LDefinition := LDocument.Resources.Definition(NyxResourceRef('cover'), NyxDefaultLocale);
    Check((LDefinition.Source.Kind = rskHosted) and
      (LDefinition.Source.CachePolicy.Server = rcspOverride), 'hosted policy');
    Check(LDefinition.FallbackDefinition.Image.Format = nimPNG, 'typed authored fallback');
    Check(LDefinition.Title = 'Shared project cover', 'common form proposal survives source emission');
    Check(LDocument.Find('second-feature') <> nil, 'second reusable consumer');
    WriteLn('PASS / exact compiled image resources / ', LCount, ' checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-resource-image-checks', IntToStr(LCount));
    document.body.setAttribute('data-test-result', 'passed');
    {$endif}
  finally
    LDocument.Free;
  end;
end.
