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
program nyx_virtual_tree_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, nyx.text, nyx.types, nyx.contract, nyx.model, nyx.codec,
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.mount, nyx.collections.selection, nyx.collections.query,
  nyx.collections.tree.rows, nyx.generated.view,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Interfaces, Classes, Forms, Controls, ComCtrls, LCLType, LCLProc,
  Graphics, IntfGraphics, FPWritePNG, nyx.render.lcl;
  {$endif}

const
  CRows = 4096;
  CEdited: TNyxText = 'A moonlit handbook / 🌙';
  CDraft: TNyxText = 'Unfinished chapter / 🌙';

type
  {$ifdef PAS2JS}
  TTreeKey = class external name 'KeyboardEvent' (TJSKeyboardEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;
  {$else}
  TTreeAccess = class(TTreeView)
    procedure Press(AKey: Word; AShift: TShiftState);
    procedure Feed(const AText: TNyxText);
  end;
  {$endif}
  { Borrowed observer, released only after disconnecting the mount. Its final
    command retires the renderer while the actual key handler still has a frame. }
  TSelectionProbe = class
    Retire: Boolean;
    procedure Changed(const ABefore, AAfter: INyxCollectionSelection);
  end;

var
  GDocument: TNyxDocument;
  GRenderer:
    {$ifdef PAS2JS}TNyxBrowserRenderer;{$else}TNyxLCLRenderer;{$endif}
  GHost: {$ifdef PAS2JS}TJSHTMLElement;{$else}TForm;{$endif}
  GTree: {$ifdef PAS2JS}TJSHTMLElement;{$else}TTreeView;{$endif}
  GView: INyxCollectionView;
  GHierarchy: INyxTreeHierarchy;
  GStore: INyxCollection;
  GMount: INyxCollectionMount;
  GProbe: TSelectionProbe;
  GKey: TNyxCollectionRef;
  GAuthored: TNyxText;
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Virtual tree: ' + AReason);
  end;
  Inc(GChecks);
end;

function Item(AIndex: Integer): TNyxItemRef;
begin
  Result := NyxItem(GKey, 'chapter-' + IntToStr(AIndex));
end;

{$ifndef PAS2JS}
procedure TTreeAccess.Press(AKey: Word; AShift: TShiftState);
begin
  KeyDown(AKey, AShift);
  Application.ProcessMessages;
end;

procedure TTreeAccess.Feed(const AText: TNyxText);
var
  LText: TUTF8Char;
begin
  LText := AText;
  UTF8KeyPress(LText);
  Application.ProcessMessages;
end;
{$endif}

procedure TSelectionProbe.Changed(const ABefore, AAfter: INyxCollectionSelection);
begin
  {$ifdef PAS2JS}
  if AAfter.Focus.Defined then
  begin
    document.body.setAttribute('data-tree-focus', AAfter.Focus.ID);
  end
  else
  begin
    document.body.setAttribute('data-tree-focus', '');
  end;
  document.body.setAttribute('data-tree-selected', IntToStr(AAfter.Count));
  {$endif}

  if Retire then
  begin
    GRenderer.Unmount;
  end;
end;

{ Synthesized keys retain the real focused target and bubble through the host.
  A separate trusted Pascal driver exercises the browser's physical defaults. }
procedure Press(const AKey: TNyxText; AControl: Boolean = False;
  AShift: Boolean = False);
var
  {$ifdef PAS2JS}
  LOptions: TJSObject;
  LTarget: TJSHTMLElement;
  {$else}
  LKey: Word;
  LShift: TShiftState;
  {$endif}
begin
  {$ifdef PAS2JS}
  LTarget := TJSHTMLElement(document.activeElement);
  Check(GTree.contains(LTarget), 'key belongs to an actual realized tree row');
  LOptions := TJSObject.new;
  LOptions['key'] := String(AKey);
  LOptions['ctrlKey'] := AControl;
  LOptions['shiftKey'] := AShift;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LTarget.dispatchEvent(TTreeKey.new('keydown', LOptions));
  {$else}
  LShift := [];

  if AControl then
  begin
    Include(LShift, ssCtrl);
  end;

  if AShift then
  begin
    Include(LShift, ssShift);
  end;
  LKey := 0;

  if AKey = 'Home' then
  begin
    LKey := VK_HOME;
  end
  else if AKey = 'End' then
  begin
    LKey := VK_END;
  end
  else if AKey = 'ArrowDown' then
  begin
    LKey := VK_DOWN;
  end
  else if AKey = 'ArrowUp' then
  begin
    LKey := VK_UP;
  end
  else if AKey = 'ArrowRight' then
  begin
    LKey := VK_RIGHT;
  end
  else if AKey = 'ArrowLeft' then
  begin
    LKey := VK_LEFT;
  end
  else if AKey = 'F2' then
  begin
    LKey := VK_F2;
  end
  else if AKey = 'Escape' then
  begin
    LKey := VK_ESCAPE;
  end;

  if LKey = 0 then
  begin
    TTreeAccess(GTree).Feed(AKey);
  end
  else
  begin
    TTreeAccess(GTree).Press(LKey, LShift);
  end;
  {$endif}
end;

procedure FocusTree;
begin
  {$ifdef PAS2JS}
  TJSHTMLElement(GTree.querySelector('[role=treeitem][tabindex="0"]')).focus;
  {$else}
  GTree.SetFocus;
  {$endif}
end;

procedure Physical;
{$ifdef PAS2JS}
var
  LRows: INyxTreeRows;
  LElements: TJSNodeList;
  LElement: TJSHTMLElement;
  LRow: TNyxTreeRow;
  LOrdinal: Integer;
  LIndex: Integer;
{$endif}
begin
  {$ifdef PAS2JS}
  LRows := NyxTreeRows(GView);
  LElements := GTree.querySelectorAll('[role=treeitem]');
  Check((LElements.length > 0) and (LElements.length < 80),
    'physical tree rows remain bounded for the complete 4096-item source');
  Check(GTree.querySelectorAll('[data-nyx-row-spacer][role=treeitem]').length = 0,
    'spacers have no selectable tree identity');
  for LIndex := 0 to LElements.length - 1 do
  begin
    LElement := TJSHTMLElement(LElements[LIndex]);
    LOrdinal := LRows.IndexOf(NyxItem(GKey, LElement.getAttribute('data-nyx-item')));
    Check(LOrdinal >= 0, 'physical entries belong to the disclosed logical hierarchy');
    LRow := LRows.RowAt(LOrdinal);
    Check((LElement.getAttribute('aria-level') = IntToStr(LRow.Level)) and
      (LElement.getAttribute('aria-posinset') = IntToStr(LRow.Position)) and
      (LElement.getAttribute('aria-setsize') = IntToStr(LRow.Size)),
      'physical entries expose exact full-query depth and sibling metadata');

    if LRow.Parent.Defined then
    begin
      Check((LElement.parentElement.getAttribute('role') = 'group') and
        (LElement.parentElement.parentElement.getAttribute('data-nyx-item') = LRow.Parent.ID),
        'each physical child is owned by its actual semantic parent group');
    end;
  end;
  Check(GTree.querySelectorAll('[role=treeitem][tabindex="0"]').length = 1,
    'the disclosed composite owns exactly one row Tab entry');
  {$else}
  Check(GTree.Items.Count = GView.Snapshot.Count, 'native keeps the complete real LCL query hierarchy');
  Check(GTree.Height <= 320, 'native paints within the authored bounded viewport');
  {$endif}
end;

procedure Attach;
begin
  GRenderer.Render(GDocument, GDocument.Pages[0], GHost);
  GRenderer.CollectionMount('handbook-tree').Disconnect;
  GMount := GRenderer.BindCollection('handbook-tree', GView);
  {$ifdef PAS2JS}GTree := GRenderer.ElementFor('handbook-tree');
  {$else}GTree := TTreeView(GRenderer.ControlFor('handbook-tree'));{$endif}
  GMount.ObserveSelection(GProbe.Changed);
end;

procedure Run;
var
  LRows: array of TNyxCollectionItem;
  LSpec: TNyxCollectionViewSpec;
  LIndex: Integer;
  LParent: Integer;
  {$ifdef PAS2JS}
  LInput: TJSHTMLInputElement;
  LRestored: TJSHTMLInputElement;
  {$else}
  LText: String;
  {$endif}
begin
  GDocument := BuildNyxDocument;
  GAuthored := TNyxCodec.Encode(GDocument);
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.getElementById('app'));
  GRenderer := TNyxBrowserRenderer.Create;
  {$else}
  GHost := TForm.Create(nil);
  GHost.SetBounds(0, 0, 800, 620);
  GHost.Show;
  GRenderer := TNyxLCLRenderer.Create;
  {$endif}
  GRenderer.Render(GDocument, GDocument.Pages[0], GHost);
  GView := GRenderer.CollectionView('handbook-tree');
  LSpec := GView.Spec;
  GKey := LSpec.Key;
  SetLength(LRows, CRows);
  for LIndex := 0 to High(LRows) do
  begin
    LRows[LIndex] := NyxCollectionItem(Item(LIndex))
      .WithValue(NyxTextField('caption'), 'Handbook chapter ' + IntToStr(LIndex + 1));

    if LIndex mod 64 <> 0 then
    begin
      LParent := (LIndex div 64) * 64;

      if LIndex mod 64 = 63 then
      begin
        LParent := LIndex - 1;
      end;
      LRows[LIndex] := LRows[LIndex].WithValue(NyxTextField('parent'), Item(LParent).ID);
    end;
  end;
  LRows[CRows - 1] := LRows[CRows - 1]
    .WithValue(NyxTextField('caption'), 'Zenith chapter');
  GStore := NewNyxCollection(GKey, GView.Store.Snapshot.Schema, LRows);
  GView := NewNyxCollectionView(GStore, LSpec, cpTree);
  GHierarchy := NyxTreeHierarchy(GView);
  GProbe := TSelectionProbe.Create;
  Attach;
  Physical;
  Check(NyxTreeRows(GView).Count = 64, 'closed hierarchy admits only its 64 roots');
  GHierarchy.ExpandAll;
  Check(NyxTreeRows(GView).Count = CRows, 'full disclosure retains all 4096 logical rows');
  GView.Select(Item(0));
  FocusTree;
  Physical;
  {$ifdef PAS2JS}
  GTree.scrollTop := 50000;
  GMount.Refresh;
  Physical;
  {$endif}
  Press('End');
  Check(GView.Selection.Focus.ID = Item(CRows - 1).ID,
    'End navigates beyond the realized window into the complete hierarchy');
  Physical;
  Press('Home', False, True);
  Check(GView.Selection.Count = CRows,
    'Shift range includes every disclosed row, never just physical controls');
  Press('ArrowDown', True);
  Check((GView.Selection.Focus.ID = Item(1).ID) and (GView.Selection.Count = CRows),
    'control navigation changes cursor while retaining complete membership');
  Press('Home');
  Press('z');
  Check(GView.Selection.Focus.ID = Item(CRows - 1).ID,
    'type-ahead realizes a distant disclosed match');
  Press('F2');
  {$ifdef PAS2JS}
  LInput := TJSHTMLInputElement(document.activeElement);
  Check(LowerCase(LInput.tagName) = 'input', 'F2 enters the distant actual editor');
  LInput.value := CDraft;
  LInput.setSelectionRange(5, 5);
  GHierarchy.SetExpanded(Item(4032), False);
  Check(not GTree.contains(LInput), 'collapsed unfinished editor is retained outside the tree');
  Check(GView.Selection.Focus.ID = Item(4032).ID,
    'collapsing an active branch returns the cursor to its visible ancestor');
  GHierarchy.ExpandAll;
  GView.Select(Item(CRows - 1), nsaFocus);
  LRestored := TJSHTMLInputElement(GTree.querySelector(
    '[data-nyx-item="chapter-4095"] input'));
  Check((LRestored = LInput) and (LRestored.value = CDraft) and
    (LRestored.selectionStart = 5), 'reopening retains exact editor, Unicode draft and caret');
  LRestored.value := CEdited;
  LRestored.dispatchEvent(TJSEvent.new('change'));
  {$else}
  Check(Assigned(GTree.OnEdited), 'native tree retains its real edit callback');
  LText := String(CEdited);
  GTree.OnEdited(GTree, GTree.Selected, LText);
  {$endif}
  Check(GView.CellText(Item(CRows - 1), 0) = CEdited,
    'actual target edit publishes exact supplementary Unicode to its runtime store');
  Check(TNyxCodec.Encode(GDocument) = GAuthored,
    'runtime selection, disclosure and edits never rewrite authored defaults');
  GView.ConfigureQuery(NyxCollectionQuery.Where(
    NyxWhere(NyxTextField('caption')).Contains('moonlit')));
  Check(GView.Snapshot.Count = 3, 'filtered leaf retains both required logical ancestors');
  Physical;
  GView.ConfigureQuery(NyxCollectionQuery.Where(
    NyxWhere(NyxTextField('caption')).Contains('No matching handbook entry')));
  Check(GView.Snapshot.Count = 0, 'empty query admits no fabricated tree entry');
  {$ifdef PAS2JS}
  Check((GTree.querySelectorAll('[role=treeitem]').length = 0) and
    (GTree.getAttribute('tabindex') = '0'), 'empty tree retains one accessible host entry');
  {$else}
  Check(GTree.Items.Count = 0, 'native empty query has no physical nodes');
  {$endif}
  GView.ConfigureQuery(Default(TNyxCollectionQuery));
  GHierarchy.ExpandAll;
  GView.Select(Item(CRows - 1), nsaFocus);
  GStore.Update(NyxCollectionItem(Item(CRows - 1))
    .WithValue(NyxTextField('parent'), Item(0).ID));
  Check(NyxTreeRows(GView).RowAt(NyxTreeRows(GView).IndexOf(Item(CRows - 1))).Level = 2,
    'large source reparenting updates copied depth and hierarchy atomically');
  Physical;
  GStore.Update(NyxCollectionItem(Item(CRows - 1))
    .WithValue(NyxTextField('parent'), Item(CRows - 2).ID));
  Check(NyxTreeRows(GView).RowAt(NyxTreeRows(GView).IndexOf(Item(CRows - 1))).Level = 3,
    'restoring the original parent restores the exact logical depth');
  GHierarchy.CollapseAll;
  Check(NyxTreeRows(GView).Count = 64, 'closing every branch restores the root-only order');
  Physical;
  FocusTree;
  GRenderer.Root.Find('handbook-tree').Configure.Enabled(False).Done;
  GRenderer.Sync;
  Press('ArrowRight');
  Check(not GHierarchy.IsExpanded(GView.Selection.Focus),
    'disabled physical keys cannot reopen a branch');
  GRenderer.Root.Find('handbook-tree').Configure.Enabled(True).Done;
  GRenderer.Sync;
  GHierarchy.ExpandAll;
  GView.Select(Item(0));
  FocusTree;
  GProbe.Retire := True;
  Press('End');
  Check(not GMount.Connected, 'key publication can retire the renderer without a stale row access');
  Check(GView.Selection.Focus.ID = Item(CRows - 1).ID,
    'retained runtime view observes accepted selection after renderer retirement');
  GProbe.Retire := False;
  GView.Select(Item(0));
  Attach;
  FocusTree;
  Physical;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-tree-id', 'handbook-tree');
  GProbe.Changed(GView.Selection, GView.Selection);
  {$endif}
end;

procedure Capture;
{$ifndef PAS2JS}
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
{$endif}
begin
  {$ifndef PAS2JS}

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    GHost.Repaint;
    Application.ProcessMessages;
    LBitmap.SetSize(GHost.Width, GHost.Height);
    GHost.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(ParamStr(1), LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
  {$endif}
end;

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  try
    Run;
    Capture;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-virtual-tree', 'passed');
    document.body.setAttribute('data-tree-checks', IntToStr(GChecks));
    {$else}WriteLn('PASS ', GChecks, ' actual virtual-tree/native-control checks');{$endif}
  except
    on LError: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-virtual-tree', 'failed');
      document.body.setAttribute('data-event-error', LError.Message);
      {$else}WriteLn('FAIL ', LError.Message);{$endif}
      {$ifndef PAS2JS}ExitCode := 1;{$endif}
    end;
  end;
  {$ifndef PAS2JS}
  GRenderer.Free;
  GMount := nil;
  GProbe.Free;
  GHierarchy := nil;
  GView := nil;
  GStore := nil;
  GDocument.Free;
  GHost.Free;
  {$endif}
end.
