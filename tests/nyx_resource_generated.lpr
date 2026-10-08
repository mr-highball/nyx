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
program nyx_resource_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.resources, nyx.resource.sources, nyx.model,
  nyx.codec, nyx.composition, nyx.binding, nyx.generated.view
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LRoundTrip: TNyxDocument;
  LView: TNyxNode;
  LChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(LChecks);
end;

begin
  LDocument := nil;
  LRoundTrip := nil;
  LView := nil;
  try
    LDocument := BuildNyxDocument;
    Check(LDocument.Title = 'Resource workshop', 'Compiled resource builder lost project identity');
    LRoundTrip := TNyxCodec.Decode(TNyxCodec.Encode(LDocument));
    Check(TNyxCodec.Encode(LRoundTrip) = TNyxCodec.Encode(LDocument), 'Compiled resources lost exact persistence');
    Check(LDocument.Resources.Definition(NyxResourceRef('notes'), NyxDefaultLocale).Text =
      TNyxText('Notes / 🌙') + TNyxText(#0), 'Compiled file content lost supplementary text or NUL');
    Check(LDocument.Resources.Definition(NyxResourceRef('hosted-copy'), NyxDefaultLocale)
      .Source.CachePolicy.Server = rcspOverride, 'Compiled hosted policy lost caller override');
    Check(LDocument.Resources.Definition(NyxResourceRef('hosted-copy'), NyxDefaultLocale)
      .FallbackDefinition.Data.Field('caption').AsText = 'Ready while hosted data loads',
      'Compiled hosted fallback lost exact data');
    LView := RealizeNyxView(LDocument, LDocument.Pages[0]);
    ApplyNyxBindings(LView, LDocument.State);
    Check(LView.Find('workshop-headline').Prop('text') = 'Your resource workbench',
      'Compiled localized selector failed to project');
    Check(LView.Find('project-name').Prop('placeholder') = 'Choose a project name',
      'Compiled prompt binding failed to project');
    Check(LView.Find('hosted-label').Prop('text') = 'Ready while hosted data loads',
      'Compiled hosted fallback did not use the same binding contract');
    WriteLn('PASS / exact compiled resource builder / ', LChecks, ' checks');
    {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'passed');{$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'failed');{$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
  LView.Free;
  LRoundTrip.Free;
  LDocument.Free;
end.
