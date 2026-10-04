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
program nyx_collection_authoring_generated_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils,
  nyx.text,
  nyx.model,
  nyx.collections,
  nyx.collections.view,
  nyx.application.state,
  nyx.fixture.collection.authoring,
  nyx.fixture.collection.page,
  nyx.fixture.collection.component
  {$ifdef PAS2JS}
  , Web
  {$endif};

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LRuntime: TNyxApplicationState;
  LView: INyxCollectionView;
begin
  LDocument := nil;
  LRuntime := nil;
  try
    LDocument := nyx.fixture.collection.authoring.BuildNyxDocument;
    Check((LDocument.Count = 2) and (LDocument.Collections.Count = 1),
      'Compiled application must retain pages/defaults');
    LRuntime := TNyxApplicationState.Create(LDocument);
    Check(LRuntime.PageCollections('home').Count = 3,
      'Compiled application must automatically admit all three authored views');
    LView := LRuntime.PageCollections('home').ViewFor('left/items');
    Check(LView.CellText(NyxItem(NyxCollection('tasks'), 'root'), 0) = 'Root 漢字 🌙',
      'Compiled instance must retain exact typed text');
    FreeAndNil(LRuntime);
    FreeAndNil(LDocument);
    LDocument := nyx.fixture.collection.page.BuildNyxDocument;
    LRuntime := TNyxApplicationState.Create(LDocument);
    Check((LRuntime.PageCollections('other').Count = 1) and
      (LRuntime.PageCollections('other').ViewFor('tasks-tree').ParentIndex(1) = 0),
      'Compiled isolated page must retain hierarchy');
    FreeAndNil(LRuntime);
    FreeAndNil(LDocument);
    LDocument := nyx.fixture.collection.component.BuildNyxDocument;
    LRuntime := TNyxApplicationState.Create(LDocument);
    Check(LRuntime.PageCollections('task-set').Count = 1,
      'Compiled isolated reusable view must retain its binding');
    Check(LRuntime.PageCollections('task-set').ViewFor('items').Snapshot.Count = 2,
      'Compiled reusable instance must materialize saved rows');
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS 6 compiled collection authoring checks';
    document.body.setAttribute('data-collection-authoring-generated', 'passed');
    {$else}
    WriteLn('PASS 6 compiled collection authoring checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-collection-authoring-generated', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      Halt(1);
      {$endif}
    end;
  end;
  LRuntime.Free;
  LDocument.Free;
end;

begin
  Run;
end.

