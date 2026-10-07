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
program nyx_virtual_table_browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

uses
  SysUtils, JS, Web, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.collections,
  nyx.collections.query, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.mount, nyx.collections.selection, nyx.render.browser,
  nyx.generated.view;

type
  TWindowKey = class external name 'KeyboardEvent' (TJSKeyboardEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;

const
  CRows = 4096;
  CPrivateDraft: TNyxText = 'A private draft / 🌙';
  CFocusedDraft: TNyxText = 'A focused draft / 🌙';

var
  GDocument: TNyxDocument;
  GRenderer: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  GTable: TJSHTMLElement;
  GDraft: TJSHTMLInputElement;
  GFocused: TJSHTMLInputElement;
  GView: INyxCollectionView;
  GStore: INyxCollection;
  GMount: INyxCollectionMount;
  GKey: TNyxCollectionRef;
  GAuthored: TNyxText;
  GPhase: Integer;
  GWait: Integer;
  GChecks: Integer;
  GRetiredRefreshes: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Actual browser row window: ' + AReason);
  end;
  Inc(GChecks);
end;

function Item(AIndex: Integer): TNyxItemRef;
begin
  Result := NyxItem(GKey, 'item-' + IntToStr(AIndex));
end;

function Cell(AIndex, AColumn: Integer): TJSHTMLElement;
begin
  Result := TJSHTMLElement(GTable.querySelector('[data-nyx-item="item-' +
    IntToStr(AIndex) + '"] [data-nyx-column="' + IntToStr(AColumn) + '"]'));
end;

procedure Key(AElement: TJSHTMLElement; const AKey: String;
  AControl: Boolean = False; AShift: Boolean = False);
var
  LOptions: TJSObject;
begin
  Check(AElement <> nil, 'keyboard destination is realized');
  LOptions := TJSObject.new;
  LOptions['key'] := AKey;
  LOptions['ctrlKey'] := AControl;
  LOptions['shiftKey'] := AShift;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  AElement.dispatchEvent(TWindowKey.new('keydown', LOptions));
end;

procedure Bounded;
var
  LRows: TJSNodeList;
begin
  LRows := GTable.querySelectorAll('[data-nyx-item]');
  Check((LRows.length > 0) and (LRows.length < 64),
    'actual attached row/editor count follows the viewport, not 4096 rows');
  Check(GTable.getAttribute('aria-rowcount') = IntToStr(GView.Snapshot.Count + 1),
    'logical row count includes rows outside the DOM');
  Check(TJSHTMLElement(GTable.querySelector('thead [role=row]'))
    .getAttribute('aria-rowindex') = '1', 'header participates in the logical row count');
  Check(GTable.querySelectorAll('[data-nyx-row-spacer][data-nyx-item]').length = 0,
    'spacers cannot impersonate selectable logical items');
end;

function Retire(AEvent: TEventListenerEvent): Boolean;
begin
  Result := True;

  if GRenderer <> nil then
  begin
    GRenderer.Free;
    GRenderer := nil;
  end;

  if GHost <> nil then
  begin
    GHost.remove;
    GHost := nil;
  end;
  GDocument.Free;
  GDocument := nil;
  GMount := nil;
  GView := nil;
  GStore := nil;
  GDraft := nil;
  GFocused := nil;
end;

procedure Advance(ATime: Double);
var
  LRows: TJSNodeList;
  LFocusCell: TJSHTMLElement;
begin
  try

    if GWait > 0 then
    begin
      Dec(GWait);
      window.requestAnimationFrame(@Advance);
      Exit;
    end;
    case GPhase of
      0:
        begin
          Bounded;
          Check(GView.Snapshot.Count = CRows, 'exact independent runtime source');
          Check(Cell(0, 0).parentElement.getAttribute('aria-rowindex') = '2',
            'initial logical row index follows the header');
          GDraft := TJSHTMLInputElement(Cell(0, 0).querySelector('input'));
          GDraft.value := CPrivateDraft;
          LFocusCell := Cell(1, 0);
          LFocusCell.focus;
          Key(LFocusCell, 'F2');
          GFocused := TJSHTMLInputElement(Cell(1, 0).querySelector('input'));
          GFocused.value := CFocusedDraft;
          GFocused.setSelectionRange(2, 7);
          GTable.scrollTop := 50000;
          GMount.Refresh;
        end;
      1:
        begin
          Bounded;
          Check(not GTable.contains(GDraft), 'unfocused offscreen draft leaves the DOM');
          Check(GTable.contains(GFocused) and (document.activeElement = GFocused),
            'focused editor remains an owned attached face while scrolled away');
          Check((GFocused.value = CFocusedDraft) and
            (GFocused.selectionStart = 2) and (GFocused.selectionEnd = 7),
            'focused Unicode draft and caret remain exact');
          GStore.Update(NyxCollectionItem(Item(CRows - 1))
            .WithValue(NyxIntegerField('priority'), 999));
          Check(GFocused.value = CFocusedDraft, 'unrelated publication preserves focused draft');
          GTable.scrollTop := 0;
          GMount.Refresh;
          Check(TJSHTMLInputElement(Cell(0, 0).querySelector('input')) = GDraft,
            'returning viewport reuses the exact detached draft control');
          Check(GDraft.value = CPrivateDraft, 'offscreen draft value remains exact');
          Key(GFocused, 'Escape');
          LFocusCell := Cell(1, 0);
          Check(document.activeElement = LFocusCell, 'Escape returns to the current cell');
          Key(LFocusCell, 'End', True);
          Check((Cell(CRows - 1, 2) <> nil) and
            (document.activeElement = Cell(CRows - 1, 2)),
            'Control+End realizes and focuses the logical final cell');
          Check(GView.Selection.Focus.ID = Item(CRows - 1).ID,
            'navigation focus uses source identity, not DOM position');
          GMount.Select(Item(CRows - 1));
          Key(Cell(CRows - 1, 2), 'Home', True, True);

          if GView.Spec.SelectionMode = nsmMultiple then
          begin
            Check(GView.Selection.Count = CRows, 'Shift range includes all unrealized rows');
          end;
        end;
      2:
        begin
          Bounded;
          GView.ConfigureQuery(NyxCollectionQuery.OrderBy(
            NyxIntegerField('priority'), nsdDescending));
          Check(GView.Snapshot.ItemAt(0).Ref.ID = Item(CRows - 1).ID,
            'query ordering retains the complete logical source');
          Check(TJSHTMLElement(GTable.querySelector('[data-nyx-item="item-4095"]'))
            .getAttribute('aria-rowindex') = '2', 'realized query row keeps logical ARIA index');
          GView.ConfigureQuery(NyxCollectionQuery.Where(
            NyxWhere(NyxIntegerField('priority')).AtLeast(1000)));
          LRows := GTable.querySelectorAll('[data-nyx-item]');
          Check((GView.Snapshot.Count = 0) and (LRows.length = 0) and
            (GTable.getAttribute('tabindex') = '0'), 'empty result retains one host entry');
          Check(TNyxCodec.Encode(GDocument) = GAuthored, 'runtime windows preserve authored document');
          GMount.Disconnect;
          GRetiredRefreshes := GMount.RefreshCount;
          GStore.Update(NyxCollectionItem(Item(0))
            .WithValue(NyxIntegerField('priority'), 1002));
          window.dispatchEvent(TJSEvent.new('resize'));
        end;
      3:
        begin
          Check(not GMount.Connected and (GMount.RefreshCount = GRetiredRefreshes),
            'source publication and host resize cannot call the disconnected receiver');
          Check(TNyxCodec.Encode(GDocument) = GAuthored, 'teardown preserves exact source defaults');
          GView.ConfigureQuery(NyxCollectionQuery.OrderBy(
            NyxIntegerField('priority'), nsdDescending));
          { Keep a fresh ordinary English consumer for selective desktop/narrow
            captures. The preceding attachment's retirement is already asserted. }
          GMount := GRenderer.BindCollection('work-table', GView);
          GDraft := nil;
          GFocused := nil;
        end;
      4:
        begin
          Bounded;
          document.body.setAttribute('data-windowing', 'passed');
          document.body.setAttribute('data-windowing-checks', IntToStr(GChecks));
          document.body.setAttribute('data-windowing-rows', IntToStr(CRows));
          window.addEventListener('pagehide', @Retire);
          Exit;
        end;
    end;
    Inc(GPhase);
    GWait := 2;
    window.requestAnimationFrame(@Advance);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-windowing', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      Retire(nil);
    end;
  end;
end;

procedure Start;
var
  LRows: array of TNyxCollectionItem;
  LIndex: Integer;
  LStatus: TNyxText;
begin
  GDocument := BuildNyxDocument;
  GAuthored := TNyxCodec.Encode(GDocument);
  GRenderer := TNyxBrowserRenderer.Create;
  GHost := TJSHTMLElement(document.createElement('main'));
  GHost.style.setProperty('max-width', '900px');
  document.body.appendChild(GHost);
  GRenderer.Render(GDocument, GDocument.Pages[0], GHost);
  GTable := GRenderer.ElementFor('work-table');
  GView := GRenderer.CollectionView('work-table');
  GKey := GView.Spec.Key;
  SetLength(LRows, CRows);
  for LIndex := 0 to CRows - 1 do
  begin
    LStatus := 'Ready';

    if LIndex mod 17 = 0 then
    begin
      LStatus := 'Ready for a thoughtful review with the whole team.';
    end;
    LRows[LIndex] := NyxCollectionItem(Item(LIndex))
      .WithValue(NyxTextField('task'), 'Work item ' + IntToStr(LIndex + 1))
      .WithValue(NyxIntegerField('priority'), LIndex mod 5)
      .WithValue(NyxTextField('status'), LStatus);
  end;
  GStore := NewNyxCollection(GKey, GView.Store.Snapshot.Schema, LRows);
  GView := NewNyxCollectionView(GStore, GView.Spec, cpTable);
  GRenderer.CollectionMount('work-table').Disconnect;
  GMount := GRenderer.BindCollection('work-table', GView);
  GWait := 2;
  window.requestAnimationFrame(@Advance);
end;

begin
  try
    Start;
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-windowing', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      Retire(nil);
    end;
  end;
end.
