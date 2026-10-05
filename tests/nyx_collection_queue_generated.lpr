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



program nyx_collection_queue_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.model, nyx.codec, nyx.data, nyx.collections,
  nyx.collections.view, nyx.collections.selection, nyx.application.state,
  nyx.generated.view
  {$ifdef PAS2JS}, Web{$else}, Interfaces, Forms, Grids, nyx.application.lcl{$endif};

var
  LDocument: TNyxDocument;
  LRuntime: TNyxApplicationState;
  LOther: TNyxApplicationState;
  LView: INyxCollectionView;
  LLeft: INyxCollectionView;
  LRight: INyxCollectionView;
  LItem: TNyxItemRef;
  LBefore: TNyxText;
  LExpected: TNyxText;
  LChecks: Integer;
  {$ifndef PAS2JS}
  LApplication: TNyxLCLApplication;
  LGrid: TStringGrid;
  LEdited: String;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxCollection.Create('Compiled collection companion: ' + AReason);
  end;
  Inc(LChecks);
end;

begin
  LDocument := nil;
  LRuntime := nil;
  LOther := nil;
  {$ifndef PAS2JS}LApplication := nil;{$endif}
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    LDocument := BuildNyxDocument;
    LDocument.Validate;
    LBefore := TNyxCodec.Encode(LDocument);
    { Keep both non-ASCII operands in the portable text contract. A Unicode
      literal otherwise promotes native comparison through an implicit boundary. }
    LExpected := TNyxText('Ready for release 🌙') + #10 + TNyxText('Café');
    Check((LDocument.Count = 2) and (LDocument.ComponentCount = 1) and
      (LDocument.Collections.Count = 1), 'Exact application retains pages, reusable root and data');
    LItem := NyxItem(NyxCollection('tasks'), 'alpha');
    Check(LDocument.Collections.Snapshot(LItem.Collection).Item(LItem)
      .GetValue(NyxTextField('caption')) = LExpected,
      'Exact compiled source preserves supplementary Unicode and newline');
    Check((LDocument.Find('tasks-table').CollectionView.ColumnAt(0).FieldName = 'priority') and
      (LDocument.Find('tasks-table').CollectionView.ColumnAt(1).Title = 'Task details') and
      (LDocument.Find('tasks-table').CollectionView.SelectionMode = nsmMultiple),
      'Exact reordered columns and selection policy reconstruct');

    LRuntime := TNyxApplicationState.Create(LDocument);
    LOther := TNyxApplicationState.Create(LDocument);
    LView := LRuntime.PageCollections('home').ViewFor('tasks-table');
    LLeft := LRuntime.PageCollections('home').ViewFor('left-list/task-list');
    LRight := LRuntime.PageCollections('home').ViewFor('right-list/task-list');
    LLeft.Store.Update(NyxCollectionItem(LItem).WithValue(NyxTextField('caption'), 'Left-only task'));
    Check((LLeft.CellText(LItem, 1) = 'Left-only task') and
      (LRight.CellText(LItem, 1) <> 'Left-only task') and
      (LView.CellText(LItem, 1) <> 'Left-only task'),
      'Compiled reusable instances have independent stores and application scope');
    Check(LRuntime.PageCollections('other').ViewFor('tasks-tree').ParentIndex(1) = 0,
      'Compiled tree admits typed parent mapping');

    {$ifndef PAS2JS}
    LApplication := TNyxLCLApplication.Create;
    LApplication.Mount(LDocument);
    LGrid := TStringGrid(LApplication.View.ControlFor('tasks-table'));
    Check((LGrid <> nil) and (TNyxText(LGrid.Cells[1, 0]) = 'Task details') and
      (TNyxText(LGrid.Cells[1, 1]) = LExpected),
      'Actual compiled native table paints admitted title and exact saved text');
    LEdited := 'Edited through the compiled table';
    LGrid.OnValidateEntry(LGrid, 1, 1, LGrid.Cells[1, 1], LEdited);
    Check((LApplication.Collections.Collection(NyxCollection('tasks')).Snapshot.Item(LItem)
      .GetValue(NyxTextField('caption')) = LEdited) and
      (LDocument.Collections.Snapshot(NyxCollection('tasks')).Item(LItem)
      .GetValue(NyxTextField('caption')) <> LEdited),
      'Actual generated table callback updates its runtime store independently of defaults');
    {$endif}
    Check((TNyxCodec.Encode(LDocument) = LBefore) and
      (LOther.PageCollections('home').ViewFor('tasks-table').CellText(LItem, 1) =
      LExpected),
      'Runtime editing never mutates authored defaults or another application');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-collection-generated', 'passed');
    document.body.setAttribute('data-collection-generated-checks', IntToStr(LChecks));
    {$else}
    WriteLn('PASS ', LChecks, ' compiled collection companion/native control checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-collection-generated', 'failed');
      document.body.setAttribute('data-collection-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}LApplication.Free;{$endif}
  LRight := nil;
  LLeft := nil;
  LView := nil;
  LOther.Free;
  LRuntime.Free;
  LDocument.Free;
end.
