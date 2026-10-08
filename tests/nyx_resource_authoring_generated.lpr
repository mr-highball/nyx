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


program nyx_resource_authoring_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.resources, nyx.model, nyx.binding,
  nyx.binding.types, nyx.composition, nyx.generated.view
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LResource: INyxResourceDefinition;
  LProjection: TNyxNode;
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
  try
    LDocument := BuildNyxDocument;
    LProjection := nil;
    try
      LResource := LDocument.Resources.Definition(NyxResourceRef('copy'), NyxDefaultLocale);
      Check(LDocument.Title = 'Resource workshop', 'Exact accepted source lost its application title');
      Check(LResource.Kind = nrkJSON, 'Exact source lost typed JSON file');
      Check(LResource.Title = TNyxText('Workshop copy 🌙'), 'Exact source lost Unicode creator title');
      Check(LResource.Description = 'Captions and prompts supplied by the project.',
        'Exact source lost creator help');
      Check(LResource.Data.Field('rows').Item(0).Field('value').AsDecimal.Text = '3.125',
        'Exact source changed JSON number spelling');
      Check(LDocument.Find('workshop-headline').Bindings[0].ResourceValue.Path.ToData.ToJSON =
        NyxResourcePath.Field('literal.dot').ToData.ToJSON, 'Exact source changed literal dotted selector');
      LProjection := RealizeNyxView(LDocument, LDocument.Pages[0]);
      ApplyNyxBindings(LProjection, LDocument.State);
      Check(LProjection.Find('workshop-headline').Prop('text') = TNyxText('Your resource workshop 🌙'),
        'Exact compiled builder cannot project its resource caption');
      WriteLn('PASS / compiled resource authoring / ', GChecks, ' checks');
      {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'passed');{$endif}
    finally
      LResource := nil;
      LProjection.Free;
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
