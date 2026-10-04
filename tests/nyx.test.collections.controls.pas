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
unit nyx.test.collections.controls;

{$mode delphi}{$H+}
{$codepage utf8}

interface

function RunNyxCollectionControlJourney: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.data,
  nyx.model,
  nyx.codec,
  nyx.controls,
  nyx.collections,
  nyx.collections.view,
  nyx.collections.view.types,
  nyx.collections.mount,
  nyx.binding.types,
  nyx.behavior,
  nyx.events,
  nyx.scheduler,
  nyx.test.collections.view,
  {$ifdef PAS2JS}
  JS, Web, nyx.test.keyboard.browser, nyx.render.browser;
  {$else}
  Forms, StdCtrls, ComCtrls, Grids, nyx.render.lcl;
  {$endif}

type
  {$ifdef PAS2JS}
  TCollectionRenderer = TNyxBrowserRenderer;
  {$else}
  TCollectionRenderer = TNyxLCLRenderer;
  {$endif}

  TUnmountProbe = class
  public
    Renderer: TCollectionRenderer;
    Calls: Integer;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  end;

  TCollectionClick = class(TNyxEventCallback)
  public
    Calls: Integer;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

  TCollectionKey = class(TNyxEventCallback)
  public
    DownCalls: Integer;
    UpCalls: Integer;
    Consume: Boolean;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

procedure TCollectionKey.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin

  if AEvent.Trigger = ntKeyDown then
  begin
    Inc(DownCalls);

    if Consume then
    begin
      NyxEventResponse(AExecution).Consume;
    end;
  end
  else if AEvent.Trigger = ntKeyUp then
  begin
    Inc(UpCalls);
  end;
end;

procedure TUnmountProbe.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(Calls);
  Renderer.Unmount;
end;

procedure BindOnlyToRenderer(ARenderer: TCollectionRenderer; const AID: TNyxText;
  const AView: INyxCollectionView);
var
  LMount: INyxCollectionMount;
begin
  { All returned interface temporaries end here. During the target callback,
    the renderer holds the only external attachment reference. }
  LMount := ARenderer.BindCollection(AID, AView);
end;

procedure TCollectionClick.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin

  if AEvent.Trigger = ntClick then
  begin
    Inc(Calls);
  end;
end;

{$ifdef PAS2JS}
function Row(AHost: TJSHTMLElement; const AID: TNyxText): TJSHTMLElement;
var
  LRows: TJSNodeList;
  LIndex: Integer;
begin
  LRows := AHost.querySelectorAll('[data-nyx-item]');
  for LIndex := 0 to LRows.length - 1 do
  begin
    Result := TJSHTMLElement(LRows[LIndex]);

    if Result.getAttribute('data-nyx-item') = AID then
    begin
      Exit;
    end;
  end;
  raise ENyxCollection.Create('Browser collection row is missing: ' + AID);
end;

function Cell(AHost: TJSHTMLElement; const AID: TNyxText;
  AColumn: Integer): TJSHTMLInputElement;
begin
  Result := TJSHTMLInputElement(Row(AHost, AID).querySelector(
    'input[data-nyx-column="' + IntToStr(AColumn) + '"]'));
end;

procedure Edit(AInput: TJSHTMLInputElement; const AValue: TNyxText);
begin

  if AInput._type = 'checkbox' then
  begin
    AInput.checked := AValue = 'true';
  end
  else
  begin
    AInput.value := AValue;
  end;
  AInput.dispatchEvent(TJSEvent.new('change'));
end;
{$else}
procedure CommitGrid(AGrid: TStringGrid; ACol, ARow: Integer; const AValue: TNyxText);
var
  LDraft: String;
begin
  { Drive the real grid's registered entry-validation callback and apply its
    returned accepted display, matching LCL's edit-commit boundary. This proves
    widget/admission routing, not trusted operating-system keyboard input. }
  LDraft := AValue;
  AGrid.OnValidateEntry(AGrid, ACol, ARow, AGrid.Cells[ACol, ARow], LDraft);
  AGrid.Cells[ACol, ARow] := LDraft;
end;
{$endif}

function RunNyxCollectionControlJourney: Integer;
var
  LDocument: TNyxDocument;
  LPage: INyxColumn;
  LDefinition: INyxColumn;
  LKey: TNyxCollectionRef;
  LStore: INyxCollection;
  LContext: INyxCollectionContext;
  LLeftStore: INyxCollection;
  LRightStore: INyxCollection;
  LTableView: INyxCollectionView;
  LTreeView: INyxCollectionView;
  LLeftView: INyxCollectionView;
  LRightView: INyxCollectionView;
  LTableMount: INyxCollectionMount;
  LTreeMount: INyxCollectionMount;
  LLeftMount: INyxCollectionMount;
  LRightMount: INyxCollectionMount;
  LRejectedMount: INyxCollectionMount;
  LChild: TNyxItemRef;
  LNew: TNyxItemRef;
  LSpec: TNyxCollectionViewSpec;
  LBefore: TNyxText;
  LRevision: Integer;
  LRejected: Boolean;
  LCaption: TNyxText;
  LClickOne: TCollectionClick;
  LClickTwo: TCollectionClick;
  LCallbackOne: INyxEventCallback;
  LCallbackTwo: INyxEventCallback;
  LSubscriptionOne: INyxEventSubscription;
  LSubscriptionTwo: INyxEventSubscription;
  LUnmountProbe: TUnmountProbe;
  LUnmountToken: INyxCollectionViewSubscription;
  LKeys: TCollectionKey;
  LKeyCallback: INyxEventCallback;
  LDownToken: INyxEventSubscription;
  LUpToken: INyxEventSubscription;
  LTableKeyToken: INyxEventSubscription;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  LGrid: TJSHTMLElement;
  LTree: TJSHTMLElement;
  LLeft: TJSHTMLElement;
  LRight: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LOriginalInput: TJSHTMLInputElement;
  LOriginalTree: TJSHTMLElement;
  LKeyEvent: TJSKeyboardEvent;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LGrid: TStringGrid;
  LTree: TTreeView;
  LLeft: TListBox;
  LRight: TListBox;
  LOriginalTree: TTreeNode;
  LValidate: TValidateEntryEvent;
  LTreeChange: TTVChangedEvent;
  LEditedValue: String;
  LNativeKey: Word;
  {$endif}

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxCollection.Create('Collection controls: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  LStore := CreateNyxViewCollection;
  LKey := LStore.Snapshot.Key;
  LChild := NyxItem(LKey, 'child / 漢字');
  LNew := NyxItem(LKey, 'new " row');
  LDocument := TNyxDocument.Create;
  LDocument.Collections.Define(LStore.Snapshot);
  LPage := NewNyxColumn('collection-home');
  LPage.Configure.Padding(16).Gap(12).Done;
  LDocument.AddPage(LPage);
  LDefinition := NewNyxColumn('task-set');
  LDefinition.Add(NewNyxList('task-items'));
  LDocument.AddComponent(LDefinition);
  LPage.Add(NewNyxTable('tasks-table')).Add(NewNyxTree('tasks-tree'));
  LPage.Add(NewNyxComponent('left').Configure.Component(NyxComponent('task-set')).Done);
  LPage.Add(NewNyxComponent('right').Configure.Component(NyxComponent('task-set')).Done);
  LBefore := TNyxCodec.Encode(LDocument);
  LContext := NewNyxCollectionContext(LDocument.Collections);
  LStore := LContext.Collections.Collection(LKey);
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  LRenderer := TNyxBrowserRenderer.Create;
  {$else}
  LHost := TForm.CreateNew(nil);
  LHost.SetBounds(0, 0, 900, 900);
  LRenderer := TNyxLCLRenderer.Create;
  {$endif}
  LUnmountProbe := TUnmountProbe.Create;
  LUnmountProbe.Renderer := LRenderer;
  try
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LSpec := NyxTestTableViewSpec.Scoped(csInstance);
    { Resolve through actual realized reusable-owner identities. The two copies
      share the same definition/child ID, but never share their local data. }
    LLeftStore := LContext.Resolve(LSpec, LRenderer.Root.Find('left/task-set').ID);
    LRightStore := LContext.Resolve(LSpec, LRenderer.Root.Find('right/task-set').ID);
    LTableView := NewNyxCollectionView(LStore, NyxTestTableViewSpec, cpTable);
    LTreeView := NewNyxCollectionView(LStore, NyxTestTreeViewSpec, cpTree);
    LLeftView := NewNyxCollectionView(LLeftStore, LSpec, cpList);
    LRightView := NewNyxCollectionView(LRightStore, LSpec, cpList);
    LTableMount := LRenderer.BindCollection('tasks-table', LTableView);
    LTreeMount := LRenderer.BindCollection('tasks-tree', LTreeView);
    LLeftMount := LRenderer.BindCollection('left/task-items', LLeftView);
    LRightMount := LRenderer.BindCollection('right/task-items', LRightView);
    LRevision := LStore.Snapshot.Revision;
    Check(not LTableMount.EditCell(Default(TNyxItemRef), 0, 'Unscoped draft') and
      (LTableMount.Failure = nbfRejected) and (LStore.Snapshot.Revision = LRevision),
      'undefined edit identity rejects without a secondary target refresh failure');
    Check(not LTableMount.EditCell(NyxItem(NyxCollection('foreign'),
      LStore.Snapshot.ItemAt(0).Ref.ID), 0, 'Foreign draft') and
      (LTableMount.Failure = nbfRejected) and (LStore.Snapshot.Revision = LRevision),
      'foreign edit identity cannot normalize or publish an identically named row');
    LClickOne := TCollectionClick.Create;
    LClickTwo := TCollectionClick.Create;
    LCallbackOne := LClickOne;
    LCallbackTwo := LClickTwo;
    LSubscriptionOne := LRenderer.Events.On(
      NyxControlEvents('left/task-items', niRuntime), ntClick).Subscribe(LCallbackOne);
    LSubscriptionTwo := LRenderer.Events.On(
      NyxControlEvents('left/task-items', niRuntime), ntClick).Subscribe(LCallbackTwo);
    LKeys := TCollectionKey.Create;
    LKeyCallback := LKeys;
    LDownToken := LRenderer.Events.On(
      NyxControlEvents('left/task-items', niRuntime), ntKeyDown).Subscribe(LKeyCallback);
    LUpToken := LRenderer.Events.On(
      NyxControlEvents('left/task-items', niRuntime), ntKeyUp).Subscribe(LKeyCallback);
    LTableKeyToken := LRenderer.Events.On(
      NyxControlEvents('tasks-table', niRuntime), ntKeyUp).Subscribe(LKeyCallback);
    {$ifdef PAS2JS}
    LGrid := LRenderer.ElementFor('tasks-table');
    LTree := LRenderer.ElementFor('tasks-tree');
    LLeft := LRenderer.ElementFor('left/task-items');
    LRight := LRenderer.ElementFor('right/task-items');
    Check((LGrid.querySelectorAll('tbody tr').length = 3) and
      (LLeft.children.length = 3) and (LRight.children.length = 3),
      'actual browser table and reusable lists project typed collections');
    Check(Row(LTree, LChild.ID).parentElement.parentElement = Row(LTree, 'root'),
      'browser tree represents parent links, not flat details');
    Row(LLeft, 'root').click;
    {$else}
    LGrid := TStringGrid(LRenderer.ControlFor('tasks-table'));
    LTree := TTreeView(LRenderer.ControlFor('tasks-tree'));
    LLeft := TListBox(LRenderer.ControlFor('left/task-items'));
    LRight := TListBox(LRenderer.ControlFor('right/task-items'));
    Check((LGrid.RowCount = 4) and (LLeft.Items.Count = 3) and (LRight.Items.Count = 3),
      'actual native grid and reusable list boxes project typed collections');
    Check(LTree.Items.FindNodeWithText('Child 🌙').Parent =
      LTree.Items.FindNodeWithText('Root 漢字'), 'native tree represents typed parent links');
    LLeft.ItemIndex := 1;
    LLeft.OnClick(LLeft);
    {$endif}
    Check(LLeftView.HasSelection and (LLeftView.Selected.ID = 'root') and
      not LRightView.HasSelection, 'real list selection stays in its reusable instance');
    Check((LClickOne.Calls = 1) and (LClickTwo.Calls = 1),
      'collection row clicks preserve both existing Nyx callback registrations');
    {$ifdef PAS2JS}
    Row(LLeft, LChild.ID).dispatchEvent(NyxTestKeyboard(ntKeyDown, 'Enter'));
    Row(LLeft, LChild.ID).dispatchEvent(NyxTestKeyboard(ntKeyUp, 'Enter'));
    {$else}
    LNativeKey := 13;
    LLeft.OnKeyDown(LLeft, LNativeKey, []);
    LLeft.OnKeyUp(LLeft, LNativeKey, []);
    {$endif}
    Check((LKeys.DownCalls = 1) and (LKeys.UpCalls = 1),
      'bound collection controls retain their canonical key-down and key-up callbacks');
    LKeys.Consume := True;
    {$ifdef PAS2JS}
    LKeyEvent := NyxTestKeyboard(ntKeyDown, 'Enter');
    Row(LLeft, 'root').dispatchEvent(LKeyEvent);
    Check((LKeys.DownCalls = 2) and LKeyEvent.defaultPrevented and
      (LLeftView.Selected.ID = LChild.ID),
      'consumed row key reaches Nyx before collection default selection');
    Cell(LGrid, LChild.ID, 0).dispatchEvent(NyxTestKeyboard(ntKeyUp, 'A'));
    {$else}
    LNativeKey := 13;
    LLeft.OnKeyDown(LLeft, LNativeKey, []);
    Check((LKeys.DownCalls = 2) and (LNativeKey = 0),
      'consumed native collection key suppresses its widget default action');
    LNativeKey := 65;
    LGrid.OnKeyUp(LGrid, LNativeKey, []);
    {$endif}
    Check(LKeys.UpCalls = 2,
      'table editor keys retain the owning data-control callback boundary');
    LLeftStore.Update(NyxCollectionItem(LChild)
      .WithValue(NyxTextField('caption'), 'Local 🌙'));
    {$ifdef PAS2JS}
    Check((Row(LLeft, LChild.ID).textContent = TNyxText('Local 🌙')) and
      (Row(LRight, LChild.ID).textContent = TNyxText('Child 🌙')),
      'real reusable browser lists update independently');
    LOriginalInput := Cell(LGrid, LChild.ID, 0);
    LOriginalInput.focus;
    LOriginalTree := Row(LTree, LChild.ID);
    { Click the actual editor. A deliberate row-background click now takes row
      focus for keyboard navigation; it cannot stand in for editing a cell. }
    LOriginalInput.click;
    Edit(Cell(LGrid, LChild.ID, 2), '3');
    {$else}
    Check((TNyxText(LLeft.Items[0]) = TNyxText('Local 🌙')) and
      (TNyxText(LRight.Items[0]) = TNyxText('Child 🌙')),
      'real reusable native list boxes update independently');
    LOriginalTree := LTree.Items.FindNodeWithText('Child 🌙');
    LGrid.Row := 2;
    LGrid.Row := 1;
    CommitGrid(LGrid, 2, 1, '3');
    {$endif}
    Check(LTableView.HasSelection and (LTableView.Selected.ID = LChild.ID) and
      (LStore.Snapshot.Item(LChild).GetValue(NyxIntegerField('priority')) = 3),
      'real table selection and edit enter the typed view/store');
    LRevision := LStore.Snapshot.Revision;
    {$ifdef PAS2JS}
    Edit(Cell(LGrid, LChild.ID, 2), '99');
    Check(Cell(LGrid, LChild.ID, 2).value = '3',
      'browser rejected cell restores the accepted display');
    {$else}
    CommitGrid(LGrid, 2, 1, '99');
    Check(LGrid.Cells[2, 1] = '3', 'native rejected entry restores the accepted display');
    {$endif}
    Check((LTableMount.Failure = nbfRejected) and (LTableMount.ErrorText <> '') and
      (LStore.Snapshot.Revision = LRevision), 'rejected control edit preserves revision and reports its phase');
    {$ifdef PAS2JS}
    Edit(Cell(LGrid, LChild.ID, 1), 'true');
    {$else}
    CommitGrid(LGrid, 1, 1, 'true');
    {$endif}
    Check((LTableMount.Failure = nbfNone) and
      LStore.Snapshot.Item(LChild).GetValue(NyxBooleanField('done')),
      'next accepted Boolean edit clears the control diagnostic');
    {$ifdef PAS2JS}
    Check(document.activeElement = LOriginalInput,
      'editing other fields preserves the originally focused cell');
    {$endif}
    LStore.Move(LChild, 2);
    Check((LTableView.Selected.ID = LChild.ID) and (LStore.Snapshot.IndexOf(LChild) = 2),
      'selection follows item identity after actual control reorder');
    {$ifdef PAS2JS}
    Check((Cell(LGrid, LChild.ID, 0) = LOriginalInput) and
      (Row(LTree, LChild.ID) = LOriginalTree),
      'browser data reorder retains cell and tree DOM identity');
    Check(document.activeElement = LOriginalInput, 'browser reorder retains the focused input');
    {$else}
    Check((LGrid.Row = 3) and
      (LTree.Items.FindNodeWithText('Child 🌙') = LOriginalTree),
      'native reorder retains selected grid row and tree-node identity');
    {$endif}
    LStore.Apply([
      NyxUpdate(NyxCollectionItem(NyxItem(LKey, 'root'))
        .WithValue(NyxTextField('parent'), LChild.ID)),
      NyxUpdate(NyxCollectionItem(LChild).WithValue(NyxTextField('parent'), ''))]);
    {$ifdef PAS2JS}
    Check(Row(LTree, 'root').parentElement.parentElement = LOriginalTree,
      'browser parent swap admits final hierarchy without a temporary DOM cycle');
    {$else}
    Check(LTree.Items.FindNodeWithText('Root 漢字').Parent = LOriginalTree,
      'native parent swap admits final hierarchy without a temporary tree cycle');
    {$endif}
    LRevision := LStore.Snapshot.Revision;
    LRejected := False;
    try
      LStore.Update(NyxCollectionItem(LChild).WithValue(NyxTextField('parent'), 'root'));
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStore.Snapshot.Revision = LRevision),
      'mounted tree rejects cyclic data before any control update');
    LCaption := 'New "' + #0 + ' 🌙';
    LStore.Append(NyxCollectionItem(LNew).WithValue(NyxTextField('caption'), LCaption));
    {$ifdef PAS2JS}
    Check(Cell(LGrid, LNew.ID, 0).value = LCaption,
      'browser cells preserve exact NUL, quotes and supplementary Unicode');
    Row(LGrid, LNew.ID).click;
    {$else}
    Check(TNyxText(LGrid.Cells[0, 4]) = NyxData(LCaption).ToJSON,
      'native NUL cells preserve exact text through the explicit escaped display');
    LGrid.Row := 4;
    {$endif}
    Check(LTableView.HasSelection and (LTableView.Selected.ID = LNew.ID),
      'inserted row participates in actual table selection');
    LStore.Remove(LNew);
    Check(not LTableView.HasSelection and (LTableView.Snapshot.Count = 3),
      'removing selected row clears identity instead of selecting its neighbor');
    LRejected := False;
    try
      LRejectedMount := LRenderer.BindCollection('tasks-table', LTreeView);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and LTableMount.Connected,
      'wrong projection rejects while retaining the accepted control binding');
    Check(TNyxCodec.Encode(LDocument) = LBefore,
      'actual collection controls never edit authored defaults/design');
    LRenderer.Unmount;
    Check(not LTableMount.Connected and not LTreeMount.Connected and
      not LLeftMount.Connected and not LRightMount.Connected,
      'renderer disconnects every retained attachment before destroying controls');
    LStore.Update(NyxCollectionItem(LChild).WithValue(NyxIntegerField('priority'), 4));
    Check(LStore.Snapshot.Item(LChild).GetValue(NyxIntegerField('priority')) = 4,
      'retained runtime store stays usable after target disposal');

    { Invoke the real registered editor callback with caller-owned value text.
      Its observer destroys the widget and releases the renderer's attachment
      during publication. Never access a target handle after that callback. }
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    BindOnlyToRenderer(LRenderer, 'tasks-table', LTableView);
    LUnmountToken := LTableView.Subscribe(LUnmountProbe.Changed);
    {$ifdef PAS2JS}
    LGrid := LRenderer.ElementFor('tasks-table');
    Edit(Cell(LGrid, LChild.ID, 2), '5');
    {$else}
    LGrid := TStringGrid(LRenderer.ControlFor('tasks-table'));
    LValidate := LGrid.OnValidateEntry;
    LEditedValue := '5';
    LValidate(LGrid, 2, LTableView.Snapshot.IndexOf(LChild) + 1, '4', LEditedValue);
    {$endif}
    Check((LUnmountProbe.Calls = 1) and (LRenderer.Root = nil) and
      (LTableView.CellText(LChild, 2) = '5')
      {$ifndef PAS2JS}and (LEditedValue = '5'){$endif},
      'actual editor callback survives observer unmount and returns the committed value');
    LUnmountToken.Disconnect;
    LUnmountProbe.Calls := 0;
    LTreeView.ClearSelection;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    BindOnlyToRenderer(LRenderer, 'tasks-tree', LTreeView);
    LUnmountToken := LTreeView.Subscribe(LUnmountProbe.Changed);
    {$ifdef PAS2JS}
    LTree := LRenderer.ElementFor('tasks-tree');
    Row(LTree, 'root').click;
    {$else}
    LTree := TTreeView(LRenderer.ControlFor('tasks-tree'));
    LTreeChange := LTree.OnChange;
    LTreeChange(LTree, LTree.Items.FindNodeWithText('Root 漢字'));
    {$endif}
    Check((LUnmountProbe.Calls = 1) and (LRenderer.Root = nil) and
      LTreeView.HasSelection and (LTreeView.Selected.ID = 'root'),
      'actual selection callback survives unmount without forwarding to destroyed targets');
    LUnmountToken.Disconnect;
  finally

    if LUnmountToken <> nil then
    begin
      LUnmountToken.Disconnect;
    end;
    LUnmountProbe.Free;
    LRenderer.Free;
    {$ifdef PAS2JS}
    LHost.remove;
    {$else}
    LHost.Free;
    {$endif}
    LDocument.Free;
  end;
end;

end.
