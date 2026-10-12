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



program nyx_collection_queue_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Web, nyx.text, nyx.studio.authoring, nyx.studio.browser,
  nyx.test.collection.queue;

type
  THistoryCheck = (hcNone, hcUndo, hcRedo);

var
  GStudio: TNyxStudio;
  GStep: Integer;
  GChecks: Integer;
  GPolls: Integer;
  GBefore: TNyxText;
  GAfter: TNyxText;
  GDraft: TNyxText;
  GBindingOpened: Boolean;
  GHistory: THistoryCheck;
  GFinalReset: Boolean;
  GWidthReset: Boolean;
  GDataOpened: Boolean;
  GColumnCaptured: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Browser collection queue: ' + AReason);
  end;
  Inc(GChecks);
end;

{ This independent DOM journey qualifies mounted input behavior the semantic
  document API cannot prove. Recovery/agents remain disconnected, and no observed
  user project is imported, replaced or edited. It needs a permitted HTTP host. }
function Find(const AID: TNyxText): TJSHTMLElement;
var
  LPanel: TNyxText;
  LButton: TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if (Result = nil) and (window.innerWidth < 900) then
  begin
    LPanel := 'design';

    if (Pos('collection-column-', AID) = 1) or
      (Pos('collection-binding-', AID) = 1) or (AID = NyxStudioBindingsToggleID) then
    begin
      LPanel := 'inspector';
    end
    else if (Pos('collection-', AID) = 1) or (AID = NyxStudioStateToggleID) then
    begin
      LPanel := 'project';
    end;
    LButton := TJSHTMLElement(document.querySelector('[data-node="action-panel-' + LPanel + '"]'));

    if LButton <> nil then
    begin
      LButton.click;
    end;
    Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
  end;
  Check(Result <> nil, 'Actual Nyx control exists / ' + AID);
end;

function Field(const AID: TNyxText): TJSHTMLElement;
begin
  Result := Find(AID);

  if not (Result is TJSHTMLTextAreaElement) and not (Result is TJSHTMLInputElement) and
    not (Result is TJSHTMLSelectElement) then
  begin
    Result := TJSHTMLElement(Result.querySelector('textarea,input,select'));
  end;
  Check(Result <> nil, 'Actual Nyx input exists / ' + AID);
end;

procedure Click(const AID: TNyxText);
begin
  Find(AID).click;
end;

procedure Change(const AID, AValue: TNyxText);
var
  LField: TJSHTMLElement;
begin
  LField := Field(AID);
  LField.focus;

  if LField is TJSHTMLSelectElement then
  begin
    TJSHTMLSelectElement(LField).value := AValue;
  end
  else if LField is TJSHTMLInputElement then
  begin
    TJSHTMLInputElement(LField).value := AValue;
  end
  else
  begin
    TJSHTMLTextAreaElement(LField).value := AValue;
  end;
  LField.dispatchEvent(TJSEvent.new('change'));
end;

function Source: TNyxText;
var
  LTab: TJSHTMLElement;
begin
  { Diagnostics can select the messages tab after a rejected job. Return to
    source through its ordinary action before inspecting the paired buffer. }

  if document.querySelector('[data-node="studio-code"]') = nil then
  begin
    LTab := TJSHTMLElement(document.querySelector('[data-node="action-source-tab"]'));

    if LTab <> nil then
    begin
      LTab.click;
    end
    else
    begin
      Click('action-code');
    end;
  end;
  Result := TJSHTMLTextAreaElement(Field('studio-code')).value;
end;

{ Ordinary history actions schedule presentation. Each comparison occurs on a
  later event-loop turn; no private session or renderer state is assigned. }
function HistoryCheck(const ABefore, AAfter: TNyxText): Boolean;
begin
  Result := False;
  case GHistory of
    hcNone:
      begin
        Click('action-undo');
        GHistory := hcUndo;
      end;
    hcUndo:
      begin
        Check(Source = ABefore, 'One Undo restores exact preceding Pascal');
        Click('action-redo');
        GHistory := hcRedo;
      end;
    hcRedo:
      begin
        Check(Source = AAfter, 'One Redo restores exact admitted Pascal');
        GHistory := hcNone;
        Result := True;
      end;
  end;
end;

procedure Step;
var
  LStatus: TJSHTMLElement;
begin
  try
    document.body.setAttribute('data-collection-step', IntToStr(GStep));
    LStatus := TJSHTMLElement(document.querySelector('[data-node=studio-source-status]'));

    if (LStatus <> nil) and (Pos('Preparing', LStatus.textContent) = 1) then
    begin
      Inc(GPolls);

      if GPolls >= 3000 then
      begin
        raise Exception.Create('Collection worker exceeded the bounded journey');
      end;
      window.setTimeout(@Step, 10);
      Exit;
    end;
    GPolls := 0;
    case GStep of
      -1:
        begin
          GStudio := TNyxStudio.Create;
          GStudio.Run(False);
          Click('action-code');
          { The ordinary controller schedules source mounting after input.
            Return to the event loop before targeting that new physical face. }
        end;
      0:
        begin
          Change('studio-code', CreateNyxCollectionQueueSeed.Source);
          Click('action-apply-source');
        end;
      1:
        begin
          if not GBindingOpened then
          begin
            Click('tasks-table');
            Click(NyxStudioBindingsToggleID);
            GBindingOpened := True;
            { Selection/pane actions schedule an ordinary paint. Target the
              resulting controls on the following event-loop turn. }
            window.setTimeout(@Step, 10);
            Exit;
          end;
          GBefore := Source;
          Change('collection-column-1-fixed', '144');
        end;
      2:
        begin
          if GHistory = hcNone then
          begin
            Check((Pos('.ColumnSizing(NyxColumnSizing.Fixed(144))', Source) > 0) and
              (TJSHTMLInputElement(Field('collection-column-1-fixed')).value = '144') and
              (document.activeElement = Field('collection-column-1-fixed')),
              'Mounted width control publishes fluent Pascal and retains focus');
            GAfter := Source;
          end;

          if not HistoryCheck(GBefore, GAfter) then
          begin
            window.setTimeout(@Step, 10);
            Exit;
          end;
          Change('collection-column-0-flex', '2');
        end;
      3:
        begin
          Change('collection-column-0-minimum', '64');
          Change('collection-column-0-minimum', '80');
          Check(TJSHTMLInputElement(Field('collection-column-0-minimum')).value = '80',
            'The newest pending bound is immediately visible');
        end;
      4:
        begin
          Check((TJSHTMLInputElement(Field('collection-column-0-minimum')).value = '80') and
            (document.activeElement = Field('collection-column-0-minimum')),
            'Earlier preparation preserves the newest bound and focus');
          Change('collection-column-0-maximum', '240');
        end;
      5:
        begin
          if not GColumnCaptured then
          begin
            { The maintained Pascal driver saves the actual editor before
              acknowledging this fixture-only checkpoint. It authors no state. }
            document.body.setAttribute('data-capture-checkpoint', 'column-sizing-inspector');

            if document.body.getAttribute('data-capture-observed') <> 'column-sizing-inspector' then
            begin
              window.setTimeout(@Step, 10);
              Exit;
            end;
            GColumnCaptured := True;
          end;
          Check((Pos('.Flex(2).Minimum(80).Maximum(240)', Source) > 0) and
            (Pos('.Fixed(144)', Source) > 0), 'Independent widths and bounds survive ordinary source admission');
          GBefore := Source;
          Change('collection-column-0-maximum', '40');
        end;
      6:
        begin
          Check((Source = GBefore) and
            (TJSHTMLInputElement(Field('collection-column-0-maximum')).value = '240'),
            'An inverted bound refuses and restores the accepted control value / source=' +
            BoolToStr(Source = GBefore, True) + ' / field=' +
            TJSHTMLInputElement(Field('collection-column-0-maximum')).value);
          GDraft := Source + #10 + '{ Width workshop notes }';
          Change('studio-code', GDraft);
          Click('collection-column-1-content');
        end;
      7:
        begin
          if not GWidthReset then
          begin
            Check(Source = GDraft, 'Content sizing retains independent unfinished Pascal');
            Click('action-reset-source');
            GWidthReset := True;
            window.setTimeout(@Step, 10);
            Exit;
          end;
          Check(Pos('.Content()', Source) > 0, 'Accepted source has the explicit content policy');
          Click('collection-column-0-default-sizing');
          Click('collection-column-1-default-sizing');
        end;
      8:
        begin
          Check(Pos('.ColumnSizing(', Source) = 0, 'Ordinary reset restores the legacy default binding');
          Change('collection-column-1-title', 'Earlier title');
          Change('collection-column-1-title', 'Task details');
          Check(TJSHTMLInputElement(Field('collection-column-1-title')).value = 'Task details',
            'Newer typed field title remains visible before worker admission');
        end;
      9:
        begin
          Check((TJSHTMLInputElement(Field('collection-column-1-title')).value = 'Task details') and
            (document.activeElement = Field('collection-column-1-title')),
            'Admitted title preserves exact field focus');
          GBefore := Source;
          Click('collection-column-2-remove');
        end;
      10:
        begin
          Check(Source <> GBefore, 'Structural column removal publishes compiled source');
          Click('collection-column-add-1');
        end;
      11:
        begin
          if not GDataOpened then
          begin
            Click(NyxStudioStateToggleID);
            GDataOpened := True;
            window.setTimeout(@Step, 10);
            Exit;
          end;
          Change('collection-0-row-0-cell-0', 'Earlier task');
          Change('collection-0-row-0-cell-0', 'Ready for release');
        end;
      12:
        begin
          Check(TJSHTMLTextAreaElement(Field('collection-0-row-0-cell-0')).value = 'Ready for release',
            'Latest row text survives prior worker completion');
          GBefore := Source;
          Change('collection-0-row-0-cell-2', '-');
        end;
      13:
        begin
          Check((Source = GBefore) and
            (TJSHTMLInputElement(Field('collection-0-row-0-cell-2')).value = '1') and
            (document.activeElement = Field('collection-0-row-0-cell-2')),
            'Rejected Integer restores accepted text without moving focus');
          GDraft := Source + #10 + '{ Independent application notes }';
          Change('studio-code', GDraft);
          Change('collection-0-row-0-cell-3', '0.375');
        end;
      14:
        begin
          if not GFinalReset then
          begin
            Check(Source = GDraft, 'Collection publication preserves the independent Pascal draft');
            Click('action-reset-source');
            GFinalReset := True;
            window.setTimeout(@Step, 10);
            Exit;
          end;

          if GHistory = hcNone then
          begin
            GAfter := Source;
          end;

          if GHistory = hcUndo then
          begin
            Check(TJSHTMLInputElement(Field('collection-0-row-0-cell-3')).value = '0.125',
              'Undo restores the exact preceding numeric value while retaining its draft');
          end;

          if GHistory = hcRedo then
          begin
            Check(TJSHTMLInputElement(Field('collection-0-row-0-cell-3')).value = '0.375',
              'Redo restores the admitted numeric value');
          end;

          { Current history owns the unfinished draft at the command boundary.
            The code face shows that draft on Undo, while the paired accepted
            source/design are qualified by the native and semantic consumers. }
          if not HistoryCheck(GDraft, GAfter) then
          begin
            window.setTimeout(@Step, 10);
            Exit;
          end;
          GBefore := Source;
          Change('collection-page-size', '1');
        end;
      15:
        begin

          if GHistory = hcNone then
          begin
            Check((Pos('.Paged(NyxCollectionPaging.PageSize(1))', Source) > 0) and
              (TJSHTMLInputElement(Field('collection-page-size')).value = '1'),
              'Page size publishes typed Pascal and its physical field');
            GAfter := Source;
          end;

          if not HistoryCheck(GBefore, GAfter) then
          begin
            window.setTimeout(@Step, 10);
            Exit;
          end;
          Change('collection-initial-page', '2');
        end;
      16:
        begin
          Check((Pos('.Paged(NyxCollectionPaging.PageSize(1).InitialPage(2))', Source) > 0) and
            (TJSHTMLInputElement(Field('collection-initial-page')).value = '2'),
            'Initial page retains the exact page size through ordinary admission');
          GBefore := Source;
          Change('collection-page-size', '0');
        end;
      17:
        begin
          Check((Source = GBefore) and
            (TJSHTMLInputElement(Field('collection-page-size')).value = '1'),
            'Invalid page size restores the accepted source and physical field');
          Click('collection-default-paging');
        end;
      18:
        begin
          Check(Pos('.Paged(', Source) = 0, 'Ordinary reset removes authored paging');
          GStudio.Free;
          GStudio := nil;
          document.body.setAttribute('data-collection-controls', 'passed');
          document.body.setAttribute('data-collection-checks', IntToStr(GChecks));
          Exit;
        end;
    end;
    Inc(GStep);
    window.setTimeout(@Step, 10);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-collection-controls', 'failed');
      document.body.setAttribute('data-collection-error', LException.Message);
      GStudio.Free;
      GStudio := nil;
    end;
  end;
end;

begin
  GStep := -1;
  window.setTimeout(@Step, 0);
end.
