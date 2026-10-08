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


program nyx_agent_resource_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.model, nyx.resources, nyx.resource.sources,
  nyx.binding, nyx.composition, nyx.generated.view
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LRuntime: TNyxNode;
  LResource: INyxResourceDefinition;
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

begin
  LDocument := nil;
  LRuntime := nil;
  try
    try
      LDocument := BuildNyxDocument;
      Check(LDocument.Title = 'Resource companion', 'Exact builder changed project identity');
      Check(LDocument.Resources.Count = 8, 'Exact builder lost resource variants');
      LResource := LDocument.Resources.Definition(NyxResourceRef('copy'), NyxDefaultLocale);
      Check((LResource.Title = TNyxText('Project copy 🌙')) and
        (LResource.Description = 'Designed for tomorrow.'), 'Exact builder lost creator intent');
      Check(LResource.Data.Field('price').AsDecimal.Text = '9007199254740993.1250',
        'Exact builder changed a numeric token');
      LResource := LDocument.Resources.Definition(NyxResourceRef('notes'), NyxDefaultLocale);
      Check(LResource.Text = TNyxText('Hello 🌙') + #0 + ' tomorrow',
        'Exact builder changed NUL or supplementary text');
      LResource := LDocument.Resources.Definition(NyxResourceRef('remote-copy'), NyxDefaultLocale);
      Check((LResource.Source.CachePolicy.Server = rcspOverride) and
        (LResource.Source.CachePolicy.FreshSeconds = 120) and
        (LResource.FallbackDefinition <> nil), 'Exact builder lost hosted cache/fallback');
      LRuntime := RealizeNyxView(LDocument, LDocument.Pages[0]);
      ApplyNyxBindings(LRuntime, LDocument.State);
      Check((LRuntime.Find('headline').Prop('text') = TNyxText('Tomorrow 🌙')) and
        (LRuntime.Find('project-name').Prop('placeholder') = 'Keep building'),
        'Exact builder cannot realize caption/prompt bindings');
      WriteLn('PASS ', GChecks, ' exact semantic resource builder checks');
      {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'passed');{$endif}
    finally
      LResource := nil;
      LRuntime.Free;
      LDocument.Free;
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'failed');{$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
