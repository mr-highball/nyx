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
program nyx_collection_refresh_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web, nyx.render.browser,
  {$else}Interfaces, Forms, StdCtrls, Grids, ComCtrls, nyx.render.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls,
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.query, nyx.collections.refresh, nyx.collections.mount,
  nyx.test.collections.controls;

type
  { Owns the immutable receipt only. The caller disconnects its borrowed store
    subscription before disposing this callback receiver. }
  TReceipt = class
  public
    Changes: INyxCollectionChanges;
    procedure Changed(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
  end;

const
  CUpdatedCaption: TNyxText = 'Notes / 🌙';

var
  GChecks: Integer;

procedure TReceipt.Changed(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
begin
  Changes := AChanges;
end;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise ENyxCollection.Create('Incremental collection controls: ' + AMessage);
  end;
  Inc(GChecks);
end;

function Ref(const AID: TNyxText): TNyxItemRef;
begin
  Result := NyxItem(NyxCollection('refresh-review'), AID);
end;

function CreateStore: INyxCollection;
begin
  Result := NewNyxCollection(NyxCollection('refresh-review'), NyxCollectionSchema
    .Text(NyxTextField('caption'), '').Text(NyxTextField('parent'), '')
    .Integer(NyxIntegerField('priority'), 0), [
    NyxCollectionItem(Ref('root')).WithValue(NyxTextField('caption'), 'Workspace'),
    NyxCollectionItem(Ref('notes')).WithValue(NyxTextField('caption'), 'Notes')
      .WithValue(NyxTextField('parent'), 'root'),
    NyxCollectionItem(Ref('work')).WithValue(NyxTextField('caption'), 'Work')
      .WithValue(NyxTextField('parent'), 'root'),
    NyxCollectionItem(Ref('later')).WithValue(NyxTextField('caption'), 'Later')
      .WithValue(NyxTextField('parent'), 'root')]);
end;

procedure RunPlans;
var
  LStore: INyxCollection;
  LBefore: INyxCollectionSnapshot;
  LVisibleBefore: INyxCollectionSnapshot;
  LView: INyxCollectionView;
  LReceipt: TReceipt;
  LToken: INyxCollectionSubscription;
  LSpec: TNyxCollectionViewSpec;
  LPlan: TNyxCollectionRefreshPlan;
  LCopy: TNyxCollectionRefreshPlan;
  LRefused: Boolean;
begin
  LStore := CreateStore;
  LSpec := NyxCollectionView(NyxCollection('refresh-review')).Column(NyxTextField('caption'), 'Item');
  LReceipt := TReceipt.Create;
  LToken := LStore.Subscribe(LReceipt.Changed);
  try
    LBefore := LStore.Snapshot;
    LPlan := NyxCollectionRefreshPlan(nil, LBefore, LSpec, nil);
    Check((LPlan.Kind = ncrFull) and (LPlan.ChangedCount = 4) and LPlan.ValuesChanged(3),
      'first mount requests all values');
    LPlan := NyxCollectionRefreshPlan(LBefore, LBefore, LSpec, nil);
    Check((LPlan.Kind = ncrUnchanged) and not LPlan.ValuesChanged(0),
      'selection/policy publication retains values');
    LStore.Apply([
      NyxUpdate(NyxCollectionItem(Ref('notes')).WithValue(NyxIntegerField('priority'), 1)),
      NyxUpdate(NyxCollectionItem(Ref('notes')).WithValue(NyxIntegerField('priority'), 2))]);
    LPlan := NyxCollectionRefreshPlan(LBefore, LStore.Snapshot, LSpec, LReceipt.Changes);
    Check((LPlan.Kind = ncrRows) and (LPlan.ChangedCount = 1) and
      LPlan.ValuesChanged(1) and not LPlan.ValuesChanged(2), 'repeated edits mark one exact row');
    LCopy := LPlan;
    LPlan := Default(TNyxCollectionRefreshPlan);
    LReceipt.Changes := nil;
    Check(LCopy.ValuesChanged(1) and not LCopy.ValuesChanged(0), 'plan copy outlives its receipt');
    LRefused := False;
    try
      LCopy.ValuesChanged(-1);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'negative row refuses');
    LRefused := False;
    try
      LPlan.ValuesChanged(0);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'undefined plan refuses');
    LRefused := False;
    try
      LCopy.ValuesChanged(LCopy.Count);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'end row refuses');
    LRefused := False;
    try
      NyxCollectionRefreshPlan(LBefore, nil, LSpec, nil);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'absent current snapshot refuses');

    LView := NewNyxCollectionView(LStore,
      LSpec.Query(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('priority')).AtLeast(2))), cpList);
    LVisibleBefore := LView.Snapshot;
    LStore.Update(NyxCollectionItem(Ref('later')).WithValue(NyxTextField('caption'), 'Tomorrow'));
    LPlan := NyxCollectionRefreshPlan(LVisibleBefore, LView.Snapshot, LView.Spec, LReceipt.Changes);
    Check((LPlan.Kind = ncrUnchanged) and (LPlan.ChangedCount = 0), 'hidden source edit touches no visible values');
    LVisibleBefore := LView.Snapshot;
    LStore.Update(NyxCollectionItem(Ref('later')).WithValue(NyxIntegerField('priority'), 3));
    LPlan := NyxCollectionRefreshPlan(LVisibleBefore, LView.Snapshot, LView.Spec, LReceipt.Changes);
    Check(LPlan.Kind = ncrFull, 'query membership change requests a complete refresh');
    LVisibleBefore := LView.Snapshot;
    LView.ConfigureQuery(NyxCollectionQuery.OrderBy(NyxIntegerField('priority'), nsdDescending));
    LPlan := NyxCollectionRefreshPlan(LVisibleBefore, LView.Snapshot, LView.Spec, nil);
    Check(LPlan.Kind = ncrFull, 'query configuration has no scalar shortcut');
    LVisibleBefore := LView.Snapshot;
    LStore.Update(NyxCollectionItem(Ref('notes')).WithValue(NyxIntegerField('priority'), 4));
    LPlan := NyxCollectionRefreshPlan(LVisibleBefore, LView.Snapshot, LView.Spec, LReceipt.Changes);
    Check(LPlan.Kind = ncrFull, 'sort-changing scalar edit requests complete placement');
    LPlan := NyxCollectionRefreshPlan(LBefore, LStore.Snapshot, LSpec, LReceipt.Changes);
    Check(LPlan.Kind = ncrFull, 'stale receipt context cannot skip values');
    LBefore := LStore.Snapshot;
    LStore.Update(NyxCollectionItem(Ref('notes')).WithValue(NyxTextField('parent'), ''));
    LPlan := NyxCollectionRefreshPlan(LBefore, LStore.Snapshot,
      LSpec.Parent(NyxTextField('parent')), LReceipt.Changes);
    Check(LPlan.Kind = ncrFull, 'changed hierarchy requests complete structure');
    LBefore := LStore.Snapshot;
    LStore.Move(Ref('later'), 1);
    LPlan := NyxCollectionRefreshPlan(LBefore, LStore.Snapshot, LSpec, LReceipt.Changes);
    Check(LPlan.Kind = ncrFull, 'structural publication requests complete structure');
  finally
    LToken.Disconnect;
    LToken := nil;
    LReceipt.Free;
  end;
end;

procedure RunControls;
var
  LStore: INyxCollection;
  LDocument: TNyxDocument;
  LRoot: INyxColumn;
  LSpec: TNyxCollectionViewSpec;
  LListView: INyxCollectionView;
  LTableView: INyxCollectionView;
  LTreeView: INyxCollectionView;
  LMount: INyxCollectionMount;
  LTableMount: INyxCollectionMount;
  LRefreshes: Integer;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  LList: TJSHTMLElement;
  LTable: TJSHTMLElement;
  LTree: TJSHTMLElement;
  LListText: TJSNode;
  LTreeText: TJSNode;
  LTableText: TJSNode;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LList: TListBox;
  LTable: TStringGrid;
  LTree: TTreeView;
  LMarker: TObject;
  LTreeNode: TTreeNode;
  {$endif}
begin
  LStore := CreateStore;
  LDocument := TNyxDocument.Create;
  LRoot := NewNyxColumn('refresh-review');
  LDocument.AddPage(LRoot);
  LRoot.Add(NewNyxList('review-list')).Add(NewNyxTable('review-table')).Add(NewNyxTree('review-tree'));
  LSpec := NyxCollectionView(NyxCollection('refresh-review')).Column(NyxTextField('caption'), 'Item');
  LListView := NewNyxCollectionView(LStore, LSpec, cpList);
  LTableView := NewNyxCollectionView(LStore,
    LSpec.Column(NyxIntegerField('priority'), 'Priority', cmEditable), cpTable);
  LTreeView := NewNyxCollectionView(LStore, LSpec.Parent(NyxTextField('parent')), cpTree);
  {$ifdef PAS2JS}
  LRenderer := TNyxBrowserRenderer.Create;
  LHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(LHost);
  {$else}
  LRenderer := TNyxLCLRenderer.Create;
  LHost := TForm.CreateNew(nil);
  LHost.SetBounds(0, 0, 900, 600);
  LMarker := TObject.Create;
  {$endif}
  try
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LMount := LRenderer.BindCollection('review-list', LListView);
    LTableMount := LRenderer.BindCollection('review-table', LTableView);
    LRenderer.BindCollection('review-tree', LTreeView);
    {$ifdef PAS2JS}
    LList := LRenderer.ElementFor('review-list');
    LTable := LRenderer.ElementFor('review-table');
    LTree := LRenderer.ElementFor('review-tree');
    LListText := LList.querySelector('[data-nyx-item="notes"] span').firstChild;
    LTreeText := LTree.querySelector('[data-nyx-item="notes"] summary span').firstChild;
    LTableText := LTable.querySelector('[data-nyx-item="notes"] td span').firstChild;
    {$else}
    LList := TListBox(LRenderer.ControlFor('review-list'));
    LTable := TStringGrid(LRenderer.ControlFor('review-table'));
    LTree := TTreeView(LRenderer.ControlFor('review-tree'));
    LList.Items.Objects[1] := LMarker;
    LTreeNode := LTree.Items[1];
    {$endif}
    LStore.Update(NyxCollectionItem(Ref('notes')).WithValue(NyxIntegerField('priority'), 7));
    {$ifdef PAS2JS}
    Check(LList.querySelector('[data-nyx-item="notes"] span').firstChild = LListText,
      'unrelated scalar preserves list text node');
    Check(LTree.querySelector('[data-nyx-item="notes"] summary span').firstChild = LTreeText,
      'unrelated scalar preserves tree text node');
    Check(LTable.querySelector('[data-nyx-item="notes"] td span').firstChild = LTableText,
      'unrelated scalar preserves table text node');
    Check(TJSHTMLInputElement(LTable.querySelector('[data-nyx-item="notes"] input')).value = '7',
      'changed table cell receives admitted scalar');
    {$else}
    Check(LList.Items.Objects[1] = LMarker, 'unrelated scalar preserves native list entry');
    Check(LTree.Items[1] = LTreeNode, 'unrelated scalar preserves native tree node');
    Check(LTable.Cells[1, 2] = '7', 'changed table cell receives admitted scalar');
    {$endif}
    LListView.Select(Ref('notes'));
    LMount.SetInteraction(False, True);
    LMount.SetInteraction(True, False);
    {$ifdef PAS2JS}
    Check(LList.querySelector('[data-nyx-item="notes"] span').firstChild = LListText,
      'selection and policy preserve visible text');
    {$else}
    Check((LList.Items.Objects[1] = LMarker) and (LList.ItemIndex = 1),
      'selection and policy preserve entry and selected identity');
    {$endif}
    LStore.Update(NyxCollectionItem(Ref('notes')).WithValue(NyxTextField('caption'), CUpdatedCaption));
    {$ifdef PAS2JS}
    Check(LList.querySelector('[data-nyx-item="notes"] span').textContent = CUpdatedCaption,
      'changed list label receives exact supplementary text');
    Check(LTree.querySelector('[data-nyx-item="notes"] summary span').textContent = CUpdatedCaption,
      'changed tree label receives exact supplementary text');
    {$else}
    Check(TNyxText(LList.Items[1]) = CUpdatedCaption, 'changed list label receives exact supplementary text');
    Check(LList.Items.Objects[1] = LMarker, 'changed caption preserves borrowed native entry object');
    Check(TNyxText(LTreeNode.Text) = CUpdatedCaption, 'changed tree label receives exact supplementary text');
    {$endif}
    { Poison only the widget draft. Failed/no-op commands must normalize the
      exact cell even though there is no dataset publication/change-plan row. }
    {$ifdef PAS2JS}
    TJSHTMLInputElement(LTable.querySelector('[data-nyx-item="notes"] input')).value := 'draft';
    {$else}
    LTable.Cells[1, 2] := 'draft';
    {$endif}
    Check(not LTableMount.EditCell(Ref('notes'), 1, 'bad'),
      'invalid numeric wire edit refuses');
    {$ifdef PAS2JS}
    Check(TJSHTMLInputElement(LTable.querySelector('[data-nyx-item="notes"] input')).value = '7',
      'rejected edit normalizes accepted browser cell');
    TJSHTMLInputElement(LTable.querySelector('[data-nyx-item="notes"] input')).value := 'draft';
    {$else}
    Check(LTable.Cells[1, 2] = '7', 'rejected edit normalizes accepted native cell');
    LTable.Cells[1, 2] := 'draft';
    {$endif}
    Check(LTableMount.EditCell(Ref('notes'), 1, '7'),
      'no-op wire edit admits with normalization');
    {$ifdef PAS2JS}
    Check(TJSHTMLInputElement(LTable.querySelector('[data-nyx-item="notes"] input')).value = '7',
      'no-op edit normalizes accepted browser cell');
    {$else}
    Check(LTable.Cells[1, 2] = '7', 'no-op edit normalizes accepted native cell');
    {$endif}
    LStore.Move(Ref('later'), 1);
    {$ifdef PAS2JS}
    Check(TJSHTMLElement(LList.children[1]).getAttribute('data-nyx-item') = 'later',
      'structural move updates actual list order');
    {$else}
    Check(LList.Items[1] = 'Later', 'structural move updates actual list order');
    {$endif}
    LRefreshes := LMount.RefreshCount;
    LRenderer.Unmount;
    Check(not LMount.Connected, 'owned unmount disconnects retained mount');
    LStore.Update(NyxCollectionItem(Ref('notes')).WithValue(NyxIntegerField('priority'), 8));
    Check(LMount.RefreshCount = LRefreshes, 'retired mount receives no later publication');
  finally
    LRenderer.Free;
    {$ifdef PAS2JS}
    LHost.remove;
    {$else}
    LHost.Free;
    LMarker.Free;
    {$endif}
    LDocument.Free;
  end;
end;

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  try
    RunPlans;
    RunControls;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-refresh-checks', IntToStr(GChecks));
    document.body.setAttribute('data-refresh-controls', 'passed');
    {$else}
    WriteLn('PASS ', GChecks, ' incremental collection plan/control checks');
    {$endif}
    { Existing lifetime, ordered notifications and real input remain regressions
      of the changed adapter rather than being inferred from the new plan. }
    {$ifdef PAS2JS}
    document.body.setAttribute('data-collection-regressions', IntToStr(RunNyxCollectionControlJourney));
    {$else}
    WriteLn('PASS ', RunNyxCollectionControlJourney, ' existing collection control checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-refresh-controls', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
