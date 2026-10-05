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



program nyx_agent_collection_controls;
{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.model, nyx.codec, nyx.state, nyx.collections,
  nyx.collections.view, nyx.collections.selection, nyx.application.state,
  nyx.generated.view
  {$ifdef PAS2JS}, Web, nyx.application.browser
  {$else}, Interfaces, Forms, Grids, Classes, nyx.application.lcl{$endif};

var
  LDocument: TNyxDocument;
  LRuntime: TNyxApplicationState;
  LOther: TNyxApplicationState;
  LView: INyxCollectionView;
  LLeft: INyxCollectionView;
  LRight: INyxCollectionView;
  LItem: TNyxItemRef;
  LExpected: TNyxText;
  LBefore: TNyxText;
  LChecks: Integer;
  {$ifdef PAS2JS}
  LApplication: TNyxBrowserApplication;
  LInput: TJSHTMLInputElement;
  LInputEvent: TJSEvent;
  {$else}
  LApplication: TNyxLCLApplication;
  LGrid: TStringGrid;
  LEdited: String;
  LStream: TFileStream;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxCollection.Create('Compiled semantic collections: ' + AReason);
  end;
  Inc(LChecks);
end;

begin
  LDocument := nil;
  LRuntime := nil;
  LOther := nil;
  LApplication := nil;
  try
    try
      {$ifndef PAS2JS}
      Application.Initialize;

      if ParamCount <> 1 then
      begin
        raise ENyxCollection.Create('Supply the exact semantic design artifact');
      end;
      LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
      try
        SetLength(LExpected, LStream.Size);

        if Length(LExpected) > 0 then
        begin
          LStream.ReadBuffer(LExpected[1], Length(LExpected));
        end;
      finally
        LStream.Free;
      end;
      {$endif}
      { Execute the unchanged exported companion, not a regenerated substitute.
        Native comparison includes exact emitted design bytes and helper lifetime. }
      LDocument := BuildNyxDocument;
      LDocument.Validate;
      LBefore := TNyxCodec.Encode(LDocument);
      {$ifndef PAS2JS}
      Check(LBefore = LExpected, 'exact admitted companion reconstructs its design');
      {$endif}
      LExpected := TNyxText('A🌙') + #0 + TNyxText('éZ');
      Check(LDocument.Collections.Snapshot(NyxCollection('qualification')).Schema.FieldAt(0)
        .DefaultValue.TextValue = LExpected, 'compiled supplementary/NUL default');
      Check((LDocument.Find('tasks-table').CollectionView.ColumnAt(0).FieldName = 'priority') and
        (LDocument.Find('tasks-table').CollectionView.ColumnAt(1).Title = 'Task') and
        (LDocument.Collections.Snapshot(NyxCollection('tasks')).ItemAt(0).Ref.ID = 'gamma'),
        'compiled typed view/order follows semantic commands');
      LRuntime := TNyxApplicationState.Create(LDocument);
      LOther := TNyxApplicationState.Create(LDocument);
      LView := LRuntime.PageCollections('home').ViewFor('tasks-table');
      LLeft := LRuntime.PageCollections('home').ViewFor('first-card/definition-list');
      LRight := LRuntime.PageCollections('home').ViewFor('second-card/definition-list');
      LItem := NyxItem(NyxCollection('tasks'), 'alpha');
      LLeft.Store.Update(NyxCollectionItem(LItem).WithValue(NyxTextField('caption'), 'Instance task'));
      Check((LLeft.CellText(LItem, 0) = 'Instance task') and
        (LRight.CellText(LItem, 0) = 'Plan the release') and
        (LView.CellText(LItem, 1) = 'Plan the release'),
        'instance binding stores remain independent of application scope');
      Check(LRuntime.PageCollections('home').ViewFor('tasks-tree').ParentIndex(2) = 1,
        'compiled tree maps parent identity after row movement');

      {$ifdef PAS2JS}
      LApplication := TNyxBrowserApplication.Create;
      LApplication.Run(LDocument, TJSHTMLElement(document.getElementById('preview')));
      LInput := TJSHTMLInputElement(LApplication.View.ElementFor('tasks-table').querySelector(
        '[data-nyx-item="alpha"] input[data-nyx-column="1"]'));
      Check(LInput <> nil, 'actual compiled browser table exposes its typed cell editor');
      LInput.value := 'Edited through the compiled table';
      LInputEvent := TJSEvent.new('change');
      LInput.dispatchEvent(LInputEvent);
      {$else}
      LApplication := TNyxLCLApplication.Create;
      LApplication.Mount(LDocument);
      LGrid := TStringGrid(LApplication.View.ControlFor('tasks-table'));
      Check((LGrid <> nil) and (LGrid.Cells[1, 0] = 'Task') and
        (LGrid.Cells[1, 2] = 'Plan the release'), 'actual native table paints semantic column/row');
      LEdited := 'Edited through the compiled table';
      LGrid.OnValidateEntry(LGrid, 1, 2, LGrid.Cells[1, 2], LEdited);
      {$endif}
      Check((LApplication.Collections.Collection(NyxCollection('tasks')).Snapshot.Item(LItem)
        .GetValue(NyxTextField('caption')) = 'Edited through the compiled table') and
        (LDocument.Collections.Snapshot(NyxCollection('tasks')).Item(LItem)
        .GetValue(NyxTextField('caption')) = 'Plan the release'),
        'actual target table callback writes runtime store and preserves document defaults');
      {$ifndef PAS2JS}
      LEdited := 'invalid';
      LGrid.OnValidateEntry(LGrid, 0, 2, LGrid.Cells[0, 2], LEdited);
      Check(LApplication.Collections.Collection(NyxCollection('tasks')).Snapshot.Item(LItem)
        .GetValue(NyxIntegerField('priority')) = 2,
        'actual invalid Integer editor preserves accepted row');
      LEdited := 'false';
      LGrid.OnValidateEntry(LGrid, 2, 2, LGrid.Cells[2, 2], LEdited);
      Check(not LApplication.Collections.Collection(NyxCollection('tasks')).Snapshot.Item(LItem)
        .GetValue(NyxBooleanField('done')), 'actual Boolean editor keeps its scalar family');
      LEdited := '0.625';
      LGrid.OnValidateEntry(LGrid, 3, 2, LGrid.Cells[3, 2], LEdited);
      Check(LApplication.Collections.Collection(NyxCollection('tasks')).Snapshot.Item(LItem)
        .GetValue(NyxNumberField('ratio')) = 0.625, 'actual number editor keeps its scalar family');
      {$endif}
      Check((TNyxCodec.Encode(LDocument) = LBefore) and
        (LOther.PageCollections('home').ViewFor('tasks-table').CellText(LItem, 1) =
          'Plan the release'), 'callbacks retain exact defaults and another independent runtime');
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-agent-collection-controls', 'passed');
      document.body.setAttribute('data-nyx-agent-collection-checks', IntToStr(LChecks));
      {$else}
      WriteLn('PASS ', LChecks, ' exact compiled semantic collection/native control checks');
      {$endif}
    except
      on LException: Exception do
      begin
        {$ifdef PAS2JS}
        document.body.setAttribute('data-nyx-agent-collection-controls', 'failed');
        document.body.setAttribute('data-nyx-agent-collection-error', LException.Message);
        {$else}
        WriteLn('FAIL ', LException.Message);
        DumpExceptionBackTrace(Output);
        ExitCode := 1;
        {$endif}
      end;
    end;
  finally
    LApplication.Free;
    LLeft := nil;
    LRight := nil;
    LView := nil;
    LOther.Free;
    LRuntime.Free;
    LDocument.Free;
  end;
end.
