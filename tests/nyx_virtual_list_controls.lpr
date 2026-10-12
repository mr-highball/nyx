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
program nyx_virtual_list_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, nyx.text, nyx.types, nyx.contract, nyx.model, nyx.codec, nyx.state,
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.mount, nyx.collections.selection, nyx.collections.query,
  nyx.generated.view,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Interfaces, Classes, Forms, Controls, StdCtrls, LCLType, LCLProc, Graphics,
  IntfGraphics, FPWritePNG, nyx.render.lcl;
  {$endif}

const
  CRows = 4096;
  CMoonTitle: TNyxText = 'A moonlit idea / 🌙';

type
  {$ifdef PAS2JS}
  TListKey = class external name 'KeyboardEvent' (TJSKeyboardEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;
  {$else}
  TListAccess = class(TListBox)
    procedure Press(AKey: Word; AShift: TShiftState);
    procedure Feed(const AText: TNyxText);
  end;
  {$endif}
  { Borrowed observer; it lives until the mount is disconnected. The final
    callback retires the renderer to qualify captured-target lifetime guards. }
  TSelectionProbe = class
    Calls: Integer;
    Retire: Boolean;
    procedure Changed(const ABefore, AAfter: INyxCollectionSelection);
  end;

var
  GDocument: TNyxDocument;
  GView: INyxCollectionView;
  GOther: INyxCollectionView;
  GStore: INyxCollection;
  GMount: INyxCollectionMount;
  GProbe: TSelectionProbe;
  GKey: TNyxCollectionRef;
  GListID: TNyxText;
  GAuthored: TNyxText;
  GChecks: Integer;
  {$ifdef PAS2JS}
  GRenderer: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  GList: TJSHTMLElement;
  {$else}
  GRenderer: TNyxLCLRenderer;
  GHost: TForm;
  GList: TListBox;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Virtual list: ' + AReason);
  end;
  Inc(GChecks);
end;

function Item(AIndex: Integer): TNyxItemRef;
begin
  Result := NyxItem(GKey, 'idea-' + IntToStr(AIndex));
end;

{$ifndef PAS2JS}
procedure TListAccess.Press(AKey: Word; AShift: TShiftState);
begin
  KeyDown(AKey, AShift);
  Application.ProcessMessages;
end;

procedure TListAccess.Feed(const AText: TNyxText);
var
  LText: TUTF8Char;
begin
  LText := AText;
  UTF8KeyPress(LText);
  Application.ProcessMessages;
end;
{$endif}

{ The host receives actual target events. DOM keys are synthesized in Pascal;
  a separate trusted driver qualifies physical browser input and default Tab. }
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
  Check(GList.contains(LTarget), 'keyboard entry belongs to the actual list');
  LOptions := TJSObject.new;
  LOptions['key'] := String(AKey);
  LOptions['ctrlKey'] := AControl;
  LOptions['shiftKey'] := AShift;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LTarget.dispatchEvent(TListKey.new('keydown', LOptions));
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
  else if AKey = ' ' then
  begin
    LKey := VK_SPACE;
  end;

  if LKey = 0 then
  begin
    TListAccess(GList).Feed(AKey);
  end
  else
  begin
    TListAccess(GList).Press(LKey, LShift);
  end;
  {$endif}
end;

procedure TSelectionProbe.Changed(const ABefore, AAfter: INyxCollectionSelection);
begin
  Inc(Calls);
  {$ifdef PAS2JS}
  { Read-only observation for the maintained physical input driver. }
  if AAfter.Focus.Defined then
  begin
    document.body.setAttribute('data-list-focus', AAfter.Focus.ID);
  end
  else
  begin
    document.body.setAttribute('data-list-focus', '');
  end;
  document.body.setAttribute('data-list-selected', IntToStr(AAfter.Count));
  {$endif}

  if Retire then
  begin
    GRenderer.Unmount;
  end;
end;

procedure FocusList;
begin
  {$ifdef PAS2JS}
  TJSHTMLElement(GList.querySelector('[role=option][tabindex="0"]')).focus;
  {$else}
  GList.SetFocus;
  {$endif}
end;

procedure Physical;
{$ifdef PAS2JS}
var
  LOptions: TJSNodeList;
  LOption: TJSHTMLElement;
  LIndex: Integer;
{$endif}
begin
  {$ifdef PAS2JS}
  LOptions := GList.querySelectorAll('[role=option]');
  Check((LOptions.length > 0) and (LOptions.length < 64),
    'physical options stay bounded independently of the 4096-row source');
  Check(GList.querySelectorAll('[data-nyx-row-spacer][role=option]').length = 0,
    'spacers cannot impersonate selectable options');
  for LIndex := 0 to LOptions.length - 1 do
  begin
    LOption := TJSHTMLElement(LOptions[LIndex]);
    Check(LOption.getAttribute('aria-setsize') = IntToStr(GView.Snapshot.Count),
      'each option exposes the complete logical result count');
    Check(LOption.getAttribute('aria-posinset') = IntToStr(GView.Snapshot.IndexOf(
      NyxItem(GKey, LOption.getAttribute('data-nyx-item'))) + 1),
      'each realized option exposes its exact current query position');
  end;
  Check(GList.querySelectorAll('[role=option][tabindex="0"]').length = 1,
    'the list owns exactly one option Tab entry');
  {$else}
  Check(GList.Items.Count = GView.Snapshot.Count, 'native control exposes the complete logical set');
  Check(GList.Height <= 260, 'native list retains its bounded authored viewport');
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

procedure Run;
var
  LRows: array of TNyxCollectionItem;
  LIndex: Integer;
  LOtherSnapshot: INyxCollectionSnapshot;
  LBefore: Integer;
  LRefreshes: Integer;
begin
  GDocument := BuildNyxDocument;
  GAuthored := TNyxCodec.Encode(GDocument);
  GListID := NyxQualifiedID('first-card', 'idea-options');
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.getElementById('app'));
  GRenderer := TNyxBrowserRenderer.Create;
  {$else}
  GHost := TForm.Create(nil);
  GHost.SetBounds(0, 0, 900, 750);
  GHost.Show;
  GRenderer := TNyxLCLRenderer.Create;
  {$endif}
  GRenderer.Render(GDocument, GDocument.Find('list-review'), GHost);
  GView := GRenderer.CollectionView(GListID);
  GOther := GRenderer.CollectionView(NyxQualifiedID('second-card', 'idea-options'));
  Check((GView <> nil) and (GOther <> nil) and (GView.Store <> GOther.Store),
    'two reusable compound instances have independent admitted stores');
  LOtherSnapshot := GOther.Snapshot;
  GKey := GView.Spec.Key;
  SetLength(LRows, CRows);
  for LIndex := 0 to High(LRows) do
  begin
    LRows[LIndex] := NyxCollectionItem(Item(LIndex))
      .WithValue(NyxTextField('title'), 'Explore idea ' + IntToStr(LIndex + 1))
      .WithValue(NyxIntegerField('rank'), LIndex);
  end;
  LRows[CRows - 1] := LRows[CRows - 1]
    .WithValue(NyxTextField('title'), 'Zenith: a new direction');
  GStore := NewNyxCollection(GKey, GView.Store.Snapshot.Schema, LRows);
  GView := NewNyxCollectionView(GStore, GView.Spec, cpList);
  GRenderer.CollectionMount(GListID).Disconnect;
  GMount := GRenderer.BindCollection(GListID, GView);
  {$ifdef PAS2JS}
  GList := GRenderer.ElementFor(GListID);
  {$else}
  GList := TListBox(GRenderer.ControlFor(GListID));
  {$endif}
  GProbe := TSelectionProbe.Create;
  GMount.ObserveSelection(GProbe.Changed);
  Physical;
  FocusList;
  {$ifdef PAS2JS}
  GList.scrollTop := 50000;
  GMount.Refresh;
  Physical;
  {$endif}
  Press('End');
  Check(GView.Selection.Focus.ID = Item(CRows - 1).ID, 'End navigates the complete logical source');
  {$ifdef PAS2JS}
  Check(TJSHTMLElement(document.activeElement).getAttribute('aria-posinset') = IntToStr(CRows),
    'distant navigation realizes the correctly indexed physical option');
  Check(GList.scrollTop > 0, 'distant keyboard focus reveals its option');
  {$endif}
  Press('Home', False, True);
  Check(GView.Selection.Count = CRows, 'Shift range includes options outside the viewport');
  Press('ArrowDown', True);
  Check((GView.Selection.Count = CRows) and (GView.Selection.Focus.ID = Item(1).ID),
    'Control navigation moves focus while retaining complete membership');
  Press('Home');
  Press('z');
  Check(GView.Selection.Focus.ID = Item(CRows - 1).ID, 'type-ahead realizes a distant logical match');
  GStore.Update(NyxCollectionItem(Item(0)).WithValue(NyxTextField('title'), CMoonTitle));
  Check(GView.CellText(Item(0), 0) = CMoonTitle, 'exact Unicode publication remains available');
  GView.ConfigureQuery(NyxCollectionQuery.OrderBy(NyxIntegerField('rank'), nsdDescending));
  Check(GView.Snapshot.ItemAt(0).Ref.ID = Item(CRows - 1).ID, 'query order remains independent of physical rows');
  Press('Home');
  Check(GView.Selection.Focus.ID = Item(CRows - 1).ID, 'Home follows the new logical query order');
  {$ifdef PAS2JS}
  Check(GList.scrollTop < 80, 'Home reveals an already-focused option after query relocation');
  {$endif}
  Physical;
  Check(GOther.Snapshot = LOtherSnapshot, 'other reusable instance remains untouched');
  LBefore := GProbe.Calls;
  GMount.SetInteraction(False, False);
  Press('End');
  Check((GProbe.Calls = LBefore) and (GView.Selection.Focus.ID = Item(CRows - 1).ID),
    'disabled forced input cannot change selection or call observers');
  GMount.SetInteraction(True, False);
  FocusList;
  GView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('rank')).AtLeast(CRows)));
  Check(GView.Snapshot.Count = 0, 'empty query retains no invented item');
  {$ifdef PAS2JS}
  Check((GList.querySelectorAll('[role=option]').length = 0) and
    (GList.getAttribute('tabindex') = '0'), 'empty list retains one accessible host entry');
  {$else}
  Check(GList.Items.Count = 0, 'native empty list contains no fabricated row');
  {$endif}
  GView.ConfigureQuery(Default(TNyxCollectionQuery));
  FocusList;
  Physical;
  Capture;
  GProbe.Retire := True;
  Press('End');
  Check((GRenderer.Root = nil) and not GMount.Connected, 'selection callback can retire its actual host');
  LRefreshes := GMount.RefreshCount;
  GStore.Update(NyxCollectionItem(Item(0)).WithValue(NyxIntegerField('rank'), 1));
  Check(GMount.RefreshCount = LRefreshes, 'retired mount receives no later publication');
  Check(TNyxCodec.Encode(GDocument) = GAuthored, 'runtime interaction preserves exact authored source/defaults');
  {$ifdef PAS2JS}
  { Leave a fresh normal application for selective observation and trusted input. }
  GRenderer.Render(GDocument, GDocument.Find('list-review'), GHost);
  GRenderer.CollectionMount(GListID).Disconnect;
  GView := NewNyxCollectionView(GStore, GView.Spec, cpList);
  GMount := GRenderer.BindCollection(GListID, GView);
  GList := GRenderer.ElementFor(GListID);
  GProbe.Retire := False;
  GMount.ObserveSelection(GProbe.Changed);
  GProbe.Changed(nil, GView.Selection);
  document.body.setAttribute('data-list-id', GListID);
  {$endif}
end;

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-list-checks', IntToStr(GChecks));
    document.body.setAttribute('data-virtual-list', 'passed');
    {$else}
    WriteLn('PASS ', GChecks, ' actual native compound-list checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-virtual-list', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}
  GRenderer.Free;
  GProbe.Free;
  GHost.Free;
  GMount := nil;
  GView := nil;
  GOther := nil;
  GStore := nil;
  GDocument.Free;
  {$endif}
end.
