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
program nyx_collection_query_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, nyx.text, nyx.types, nyx.state, nyx.collections, nyx.collections.query,
  nyx.collections.view, nyx.collections.selection, nyx.model, nyx.codec,
  nyx.generated.view,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Classes, Interfaces, Forms, Controls, StdCtrls, Grids,
    Graphics, IntfGraphics, FPWritePNG, nyx.render.lcl;{$endif}

{$ifdef PAS2JS}
type
  TQueryKeyEvent = class external name 'KeyboardEvent' (TJSKeyboardEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;
{$endif}

var
  GDocument: TNyxDocument;
  GView: INyxCollectionView;
  GChecks: Integer;
  GAuthored: TNyxText;
  {$ifdef PAS2JS}
  GRenderer: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  GTable: TJSHTMLElement;
  GEditor: TJSHTMLInputElement;
  {$else}
  GRenderer: TNyxLCLRenderer;
  GHost: TForm;
  GTable: TStringGrid;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Actual query controls: ' + AReason);
  end;
  Inc(GChecks);
end;

function Item(const AID: TNyxText): TNyxItemRef;
begin
  Result := NyxItem(NyxCollection('work-items'), AID);
end;

procedure VisibleRows(const AIDs: array of TNyxText);
var
  LIndex: Integer;
  {$ifdef PAS2JS}LRows: TJSNodeList;{$endif}
begin
  Check(GView.Snapshot.Count = Length(AIDs), 'admitted result row count');
  {$ifdef PAS2JS}
  LRows := GTable.querySelectorAll('[data-nyx-item]');
  Check(LRows.length = Length(AIDs), 'actual DOM result row count');
  {$else}
  Check(GTable.RowCount = Length(AIDs) + 1, 'actual native result row count');
  {$endif}
  for LIndex := 0 to High(AIDs) do
  begin
    Check(GView.Snapshot.ItemAt(LIndex).Ref.ID = AIDs[LIndex], 'stable result identity');
    {$ifdef PAS2JS}
    Check(TJSHTMLElement(LRows[LIndex]).getAttribute('data-nyx-item') = AIDs[LIndex],
      'actual DOM follows typed ordering');
    {$else}
    Check((TNyxText(GTable.Cells[0, LIndex + 1]) = GView.CellText(Item(AIDs[LIndex]), 0)) or
      (GTable.EditorMode and (GTable.Row = LIndex + 1) and
        (GTable.Cells[0, LIndex + 1] = 'A pending idea')),
      'actual native cells follow typed ordering / ' + AIDs[LIndex] + ' / ' +
      TNyxText(GTable.Cells[0, LIndex + 1]));
    {$endif}
  end;
end;

procedure StartDraft;
{$ifdef PAS2JS}
var
  LCell: TJSHTMLElement;
  LOptions: TJSObject;
{$endif}
begin
  GView.Select(Item('plan'));
  {$ifdef PAS2JS}
  LCell := TJSHTMLElement(GTable.querySelector(
    '[data-nyx-item="plan"] [data-nyx-column="0"]'));
  LCell.focus;
  LOptions := TJSObject.new;
  LOptions['key'] := 'F2';
  LOptions['code'] := 'F2';
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LCell.dispatchEvent(TQueryKeyEvent.new('keydown', LOptions));
  GEditor := TJSHTMLInputElement(GTable.querySelector(
    '[data-nyx-item="plan"] [data-nyx-column="0"] input'));
  Check((GEditor <> nil) and (document.activeElement = GEditor), 'actual browser editor entry');
  GEditor.value := 'A pending idea';
  GEditor.setSelectionRange(2, 7);
  {$else}
  GTable.Col := 0;
  GTable.Row := 1;
  GTable.SetFocus;
  GTable.EditorMode := True;
  Check(GTable.EditorMode and (GTable.Editor is TCustomEdit), 'actual native editor entry');
  TCustomEdit(GTable.Editor).Text := 'A pending idea';
  TCustomEdit(GTable.Editor).SelStart := 2;
  TCustomEdit(GTable.Editor).SelLength := 5;
  {$endif}
end;

procedure DraftRetained;
begin
  {$ifdef PAS2JS}
  Check((document.activeElement = GEditor) and (GEditor.value = 'A pending idea') and
    (GEditor.selectionStart = 2) and (GEditor.selectionEnd = 7),
    'sorting retains the same browser editor, draft and caret');
  {$else}
  Check(GTable.EditorMode and (GTable.Row = GView.Snapshot.IndexOf(Item('plan')) + 1) and
    (TCustomEdit(GTable.Editor).Text = 'A pending idea') and
    (TCustomEdit(GTable.Editor).SelStart = 2) and (TCustomEdit(GTable.Editor).SelLength = 5),
    'sorting retains native item identity, draft and caret');
  {$endif}
  Check(GView.Store.Snapshot.Item(Item('plan')).GetValue(NyxTextField('task')) =
    'Plan the next idea', 'sorting never commits an in-progress draft');
end;

procedure Run;
var
  LOriginal: INyxCollectionSnapshot;
begin
  LOriginal := GView.Store.Snapshot;
  VisibleRows(['plan', 'design', 'build', 'share']);
  StartDraft;
  GView.ConfigureQuery(NyxCollectionQuery.OrderBy(NyxIntegerField('priority'), nsdDescending));
  VisibleRows(['build', 'design', 'plan', 'share']);
  DraftRetained;
  GView.ConfigureQuery(NyxCollectionQuery.OrderBy(NyxTextField('task')));
  VisibleRows(['build', 'plan', 'share', 'design']);
  DraftRetained;
  GView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('priority')).AtLeast(2))
    .OrderBy(NyxIntegerField('priority'), nsdDescending));
  VisibleRows(['build', 'design']);
  Check(GView.Selection.Contains(Item('plan')), 'filter retains hidden selected item');
  {$ifdef PAS2JS}
  Check(not GTable.contains(GEditor) and (document.activeElement <> GEditor),
    'filtering an edited row retires its browser editor');
  {$else}
  Check(not GTable.EditorMode, 'filtering an edited row exits native editing');
  {$endif}
  Check(GView.Store.Snapshot = LOriginal, 'runtime queries leave the complete source exact');
  GView.ConfigureQuery(NyxCollectionQuery);
  VisibleRows(['plan', 'design', 'build', 'share']);
  Check(GView.Store.Snapshot.Item(Item('plan')).GetValue(NyxTextField('task')) =
    'Plan the next idea', 'hidden draft is discarded without changing accepted text');
  Check(GView.Selection.Contains(Item('plan')), 'clear filter restores retained membership');
  GView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('priority')).AtLeast(2)));
  GView.Store.Update(GView.Store.Snapshot.Item(Item('share'))
    .WithValue(NyxIntegerField('priority'), 4));
  VisibleRows(['design', 'build', 'share']);
  GView.ConfigureQuery(NyxCollectionQuery.OrderBy(NyxIntegerField('priority'), nsdDescending));
  VisibleRows(['share', 'build', 'design', 'plan']);
  Check(TNyxCodec.Encode(GDocument) = GAuthored, 'runtime queries/edits retain exact document defaults');
end;

{$ifndef PAS2JS}
procedure CaptureNative;
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    Application.ProcessMessages;
    GHost.Repaint;
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
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    { The ordinary application remains the exact previously authenticated MCP
      grid companion. This fixture changes runtime policy, not designer state. }
    GDocument := BuildNyxDocument;
    GAuthored := TNyxCodec.Encode(GDocument);
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('main'));
    document.body.appendChild(GHost);
    GRenderer := TNyxBrowserRenderer.Create;
    {$else}
    GHost := TForm.Create(nil);
    GHost.SetBounds(0, 0, 900, 500);
    GHost.Show;
    GRenderer := TNyxLCLRenderer.Create;
    {$endif}
    GRenderer.Render(GDocument, GDocument.Pages[0], GHost);
    GView := GRenderer.CollectionView('work-table');
    {$ifdef PAS2JS}GTable := GRenderer.ElementFor('work-table');
    {$else}GTable := TStringGrid(GRenderer.ControlFor('work-table'));{$endif}
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-query-checks', IntToStr(GChecks));
    document.body.setAttribute('data-query-controls', 'passed');
    {$else}
    CaptureNative;
    WriteLn('PASS ', GChecks, ' ordinary native collection query checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-event-error', E.Message);
      document.body.setAttribute('data-query-controls', 'failed');
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}
  GView := nil;
  GRenderer.Free;
  GHost.Free;
  GDocument.Free;
  {$endif}
end.
