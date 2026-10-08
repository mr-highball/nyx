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

program nyx_resource_mapping_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.resources, nyx.resources.rows, nyx.collections,
  nyx.collections.view, nyx.model, nyx.codec, nyx.generated.view
  {$ifdef PAS2JS}, Web, nyx.render.browser
  {$else}, Interfaces, Forms, Grids, nyx.render.lcl{$endif};

var
  LDocument: TNyxDocument;
  LRoundTrip: TNyxDocument;
  LContext: INyxCollectionContext;
  LChecks: Integer;
  LBefore: TNyxText;
  LID: TNyxText;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(LChecks);
end;

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  LDocument := nil;
  LRoundTrip := nil;
  LRenderer := nil;
  LHost := nil;
  try
    LDocument := BuildNyxDocument;
    LBefore := TNyxCodec.Encode(LDocument);
    Check(LDocument.Title = 'Team resource workshop', 'Compiled source lost project identity');
    Check(LDocument.ResourceCollections.Count = 1, 'Compiled source lost saved recipe');
    Check(LDocument.Collections.Snapshot(NyxCollection('people')).Count = 0,
      'Compiled source incorrectly stores materialized rows');
    LRoundTrip := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LRoundTrip) = LBefore, 'Compiled recipe loses exact persistence');
    LContext := NewNyxCollectionContext(LDocument.Collections, LDocument.Resources,
      NyxLocale('en-GB'), NyxDefaultLocale);
    Check(LContext.Collections.Collection(NyxCollection('people')).Snapshot.ItemAt(0)
      .GetValue(NyxTextField('name')) = TNyxText('Ada 🌙'), 'Compiled structural recipe lost Unicode locale');
    Check(LContext.Collections.Collection(NyxCollection('people')).Snapshot.ItemAt(0)
      .GetValue(NyxNumberField('ratio')) = 1.5, 'Compiled recipe lost numeric family');

    {$ifdef PAS2JS}
    LRenderer := TNyxBrowserRenderer.Create;
    LHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(LHost);
    {$else}
    LRenderer := TNyxLCLRenderer.Create;
    LHost := TForm.CreateNew(nil);
    LHost.SetBounds(0, 0, 900, 700);
    {$endif}
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LID := 'people-table';

    if LDocument.Pages[0].ID = 'team-card' then
    begin
      LID := 'card-table';
    end;
    {$ifdef PAS2JS}
    Check(LRenderer.ElementFor(LID).querySelectorAll('[role="gridcell"]')[0].textContent = 'Ada',
      'Exact compiled builder did not paint the source table');
    {$else}
    Check(TStringGrid(LRenderer.ControlFor(LID)).Cells[0, 1] = 'Ada',
      'Exact compiled builder did not paint the source table');
    {$endif}
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'Compiled mounted consumer mutates authored recipe');
    WriteLn('PASS / exact compiled mapping builder / ', LChecks, ' checks');
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
  LRenderer.Free;
  {$ifdef PAS2JS}
  if LHost <> nil then
  begin
    LHost.remove;
  end;
  {$else}LHost.Free;{$endif}
  LRoundTrip.Free;
  LDocument.Free;
end.
