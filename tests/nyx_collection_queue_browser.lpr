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

var
  GStudio: TNyxStudio;
  GStep: Integer;
  GChecks: Integer;
  GPolls: Integer;
  GBefore: TNyxText;
  GAfter: TNyxText;
  GDraft: TNyxText;

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
begin
  Result := TJSHTMLTextAreaElement(Field('studio-code')).value;
end;

procedure Step;
var
  LStatus: TJSHTMLElement;
begin
  try
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
      0:
        begin
          GStudio := TNyxStudio.Create;
          GStudio.Run(False);
          Click('action-code');
          Change('studio-code', CreateNyxCollectionQueueSeed.Source);
          Click('action-apply-source');
        end;
      1:
        begin
          Click('tasks-table');
          Click(NyxStudioBindingsToggleID);
          Change('collection-column-1-title', 'Earlier title');
          Change('collection-column-1-title', 'Task details');
          Check(TJSHTMLInputElement(Field('collection-column-1-title')).value = 'Task details',
            'Newer typed field title remains visible before worker admission');
        end;
      2:
        begin
          Check((TJSHTMLInputElement(Field('collection-column-1-title')).value = 'Task details') and
            (document.activeElement = Field('collection-column-1-title')),
            'Admitted title preserves exact field focus');
          GBefore := Source;
          Click('collection-column-2-remove');
        end;
      3:
        begin
          Check(Source <> GBefore, 'Structural column removal publishes compiled source');
          Click('collection-column-add-1');
        end;
      4:
        begin
          Click(NyxStudioStateToggleID);
          Change('collection-0-row-0-cell-0', 'Earlier task');
          Change('collection-0-row-0-cell-0', 'Ready for release');
        end;
      5:
        begin
          Check(TJSHTMLTextAreaElement(Field('collection-0-row-0-cell-0')).value = 'Ready for release',
            'Latest row text survives prior worker completion');
          GBefore := Source;
          Change('collection-0-row-0-cell-2', '-');
        end;
      6:
        begin
          Check((Source = GBefore) and
            (TJSHTMLInputElement(Field('collection-0-row-0-cell-2')).value = '1') and
            (document.activeElement = Field('collection-0-row-0-cell-2')),
            'Rejected Integer restores accepted text without moving focus');
          GDraft := Source + #10 + '{ Independent application notes }';
          Change('studio-code', GDraft);
          Change('collection-0-row-0-cell-3', '0.375');
        end;
      7:
        begin
          Check(Source = GDraft, 'Collection publication preserves the independent Pascal draft');
          Click('action-reset-source');
          GAfter := Source;
          Click('action-undo');
          Check(Source = GBefore, 'One Undo restores the accepted source half');
          Click('action-redo');
          Check(Source = GAfter, 'One Redo restores the admitted source half');
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
  window.setTimeout(@Step, 0);
end.
