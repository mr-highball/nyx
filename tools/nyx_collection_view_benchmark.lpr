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
program nyx_collection_view_benchmark;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  {$ifdef PAS2JS}
  JS, Web,
  {$else}
  Interfaces, Forms, StdCtrls, ComCtrls, Grids,
  {$endif}
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.controls,
  nyx.state,
  nyx.collections,
  nyx.collections.view,
  nyx.collections.view.types,
  nyx.collections.mount,
  {$ifdef PAS2JS}
  nyx.render.browser;
  {$else}
  nyx.render.lcl;
  {$endif}

function NowMS: Double;
begin
  {$ifdef PAS2JS}
  Result := window.performance.now;
  {$else}
  Result := GetTickCount64;
  {$endif}
end;

procedure Require(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxCollection.Create('Collection view benchmark: ' + AMessage);
  end;
end;

function Measure(ACount: Integer): TNyxText;
var
  LDocument: TNyxDocument;
  LRoot: INyxColumn;
  LKey: TNyxCollectionRef;
  LItems: array of TNyxCollectionItem;
  LStore: INyxCollection;
  LViews: array of INyxCollectionView;
  LMounts: array of INyxCollectionMount;
  LSpec: TNyxCollectionViewSpec;
  LIndex: Integer;
  LRef: TNyxItemRef;
  LStart: Double;
  LSetupMS: Double;
  LUpdateMS: Double;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  LRenderer: TNyxBrowserRenderer;
  {$else}
  LHost: TForm;
  LRenderer: TNyxLCLRenderer;
  {$endif}
begin
  { Construction/seed costs are excluded: this measures mounting admitted rows
    and end-to-end updates with all three real target controls attached. Each
    update includes store publication and view validation/projection, not merely
    adapter time or paint latency. Dataset correctness gates precede CSV output. }
  LKey := NyxCollection('measured');
  { Dynamic managed arrays are supported by both compilers; pas2js rejects
    fixed arrays of reference-counted interfaces. }
  SetLength(LViews, 3);
  SetLength(LMounts, 3);
  SetLength(LItems, ACount);
  for LIndex := 0 to ACount - 1 do
  begin
    LItems[LIndex] := NyxCollectionItem(NyxItem(LKey, 'row-' + IntToStr(LIndex)))
      .WithValue(NyxTextField('caption'), TNyxText('Item ') + IntToStr(LIndex) + TNyxText(' / 🌙'));

    if LIndex > 0 then
    begin
      LItems[LIndex] := LItems[LIndex].WithValue(NyxTextField('parent'),
        'row-' + IntToStr((LIndex - 1) div 4));
    end;
  end;
  LStore := NewNyxCollection(LKey, NyxCollectionSchema
    .Text(NyxTextField('caption'), '')
    .Integer(NyxIntegerField('priority'), 0)
    .Text(NyxTextField('parent'), ''), LItems);
  LRef := LStore.Snapshot.ItemAt(ACount - 1).Ref;
  LDocument := TNyxDocument.Create;
  LRoot := NewNyxColumn('measured-view');
  LDocument.AddPage(LRoot);
  LRoot.Add(NewNyxList('measured-list')).Add(NewNyxTable('measured-table'))
    .Add(NewNyxTree('measured-tree'));
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  LRenderer := TNyxBrowserRenderer.Create;
  {$else}
  LHost := TForm.CreateNew(nil);
  LHost.SetBounds(0, 0, 1200, 900);
  LRenderer := TNyxLCLRenderer.Create;
  {$endif}
  try
    LStart := NowMS;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LSpec := NyxCollectionView(LKey).Column(NyxTextField('caption'), 'Item');
    LViews[0] := NewNyxCollectionView(LStore, LSpec, cpList);
    LViews[1] := NewNyxCollectionView(LStore,
      LSpec.Column(NyxIntegerField('priority'), 'Priority', cmEditable), cpTable);
    LViews[2] := NewNyxCollectionView(LStore, LSpec.Parent(NyxTextField('parent')), cpTree);
    LMounts[0] := LRenderer.BindCollection('measured-list', LViews[0]);
    LMounts[1] := LRenderer.BindCollection('measured-table', LViews[1]);
    LMounts[2] := LRenderer.BindCollection('measured-tree', LViews[2]);
    LSetupMS := NowMS - LStart;
    LStart := NowMS;
    for LIndex := 1 to 20 do
    begin
      LStore.Update(NyxCollectionItem(LRef).WithValue(NyxIntegerField('priority'), LIndex));
    end;
    LUpdateMS := NowMS - LStart;
    Require((LStore.Snapshot.Count = ACount) and (LStore.Snapshot.Revision = 20) and
      (LStore.Snapshot.Item(LRef).GetValue(NyxIntegerField('priority')) = 20),
      'store result/revision differs from the measured workload');
    for LIndex := 0 to 2 do
    begin
      Require((LViews[LIndex].Snapshot.Count = ACount) and
        (LMounts[LIndex].RefreshCount = 21), 'target attachment missed or repeated a refresh');
    end;
    {$ifdef PAS2JS}
    Require(LRenderer.ElementFor('measured-list').children.length = ACount,
      'browser list row count differs');
    Require(LRenderer.ElementFor('measured-table').querySelectorAll('tbody tr').length = ACount,
      'browser table row count differs');
    Require(LRenderer.ElementFor('measured-tree').querySelectorAll('details').length = ACount,
      'browser hierarchy row count differs');
    {$else}
    Require(TListBox(LRenderer.ControlFor('measured-list')).Items.Count = ACount,
      'native list row count differs');
    Require(TStringGrid(LRenderer.ControlFor('measured-table')).RowCount = ACount + 1,
      'native table row count differs');
    Require(TTreeView(LRenderer.ControlFor('measured-tree')).Items.Count = ACount,
      'native hierarchy row count differs');
    {$endif}
    Result := IntToStr(ACount) + ',' + TNyxStateValue.FromNumber(LSetupMS).NumberText + ',' +
      TNyxStateValue.FromNumber(LUpdateMS).NumberText + ',21,21,21';
    LRenderer.Unmount;
    for LIndex := 0 to 2 do
    begin
      Require(not LMounts[LIndex].Connected, 'unmount left a live control subscription');
    end;
  finally
    LRenderer.Free;
    {$ifdef PAS2JS}
    LHost.remove;
    {$else}
    LHost.Free;
    {$endif}
    LDocument.Free;
  end;
end;

var
  LResult: TNyxText;
begin
  {$ifndef PAS2JS}
  Application.Initialize;
  {$endif}
  try
    LResult := 'rows,mount_ms,updates_20_ms,list_refreshes,table_refreshes,tree_refreshes' + #10 +
      Measure(512) + #10 + Measure(4096);
    {$ifdef PAS2JS}
    document.body.textContent := LResult;
    document.body.setAttribute('data-collection-view-benchmark', 'passed');
    {$else}
    WriteLn(LResult);
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-collection-view-benchmark', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      Halt(1);
      {$endif}
    end;
  end;
end.
