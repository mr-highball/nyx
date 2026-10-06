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
program nyx_guides_studio_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, JS, Web, nyx.text, nyx.studio.browser, nyx.studio.inspector, nyx.designer.resize;

const
  CResult = 'data-nyx-guides';
  CChecks = 'data-nyx-guides-checks';
  CError = 'data-nyx-guides-error';
  CDraft = 'Retain this independent English draft.';

var
  GStudio: TNyxStudio;
  GFrame: TJSHTMLIFrameElement;
  GEditor: TJSHTMLTextAreaElement;
  GInput: TJSHTMLTextAreaElement;
  GBefore: TNyxText;
  GAfter: TNyxText;
  GStage: Integer;
  GPolls: Integer;
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
end;

function Required(const AID: TNyxText): TJSHTMLElement;
begin
  Result := Find(AID);

  if Result = nil then
  begin
    raise Exception.Create('Missing ordinary Studio control: ' + AID);
  end;
end;

function Field(const AID: TNyxText): TJSHTMLElement;
begin
  Result := Required(AID);

  if not ((Result is TJSHTMLTextAreaElement) or (Result is TJSHTMLInputElement) or
    (Result is TJSHTMLSelectElement)) then
  begin
    Result := TJSHTMLElement(Result.querySelector('input,textarea,select'));
  end;

  if Result = nil then
  begin
    raise Exception.Create('Missing actual Studio input: ' + AID);
  end;
end;

procedure Change(const AID, AValue: TNyxText);
var
  LField: TJSHTMLElement;
begin
  LField := Field(AID);
  TJSHTMLInputElement(LField).value := AValue;
  LField.dispatchEvent(TJSEvent.new('change'));
end;

procedure Failed(const AReason: TNyxText);
begin
  document.body.setAttribute(CResult, 'failed');
  document.body.setAttribute(CError, AReason + ' / stage ' + IntToStr(GStage) +
    ' / checks ' + IntToStr(GChecks));
end;

procedure RetainedInput;
begin
  Check(Field('notes-editor') = GInput, 'Panel changes and admission retain the same canvas input');
  Check(GInput.value = CDraft, 'The independent uncommitted English draft survives');
  Check((GInput.selectionStart = 5) and (GInput.selectionEnd = 9),
    'The independent input keeps its exact selection range');
  Check(Field('studio-code') = GEditor, 'The adjacent source editor keeps its identity');
end;

{ The external Pascal input driver presses the actual browser canvas handle.
  This fixture observes ordinary editor/worker behavior; all design authoring
  was already performed through revision-aware semantic MCP operations. }
procedure Poll;
var
  LStatus: TJSHTMLElement;
  LGuide: TJSHTMLElement;
begin
  try
    Inc(GPolls);

    if GPolls > 1200 then
    begin
      raise Exception.Create('Ordinary guide Studio journey timed out');
    end;
    case GStage of
      0:
        begin
          LStatus := Find('studio-agents-status');

          if ((Pos('compact=1', window.location.search) = 0) or (window.innerWidth = 390)) and
            (LStatus <> nil) and (Pos('Agents edit', LStatus.textContent) > 0) and
            (Find('notes-editor') <> nil) then
          begin
            Check(Pos('Cards with space', document.title) > 0, 'Explicit semantic project is observed');
            Required('action-agents').click;
            Required('action-code').click;
            GEditor := TJSHTMLTextAreaElement(Field('studio-code'));
            GBefore := GEditor.value;
            Check((Pos('INyxMemo', GBefore) > 0) and (Pos('.Layout(nlAbsolute)', GBefore) > 0),
              'Ordinary Studio consumes crafted typed MCP source');
            Required('notes-editor').click;
            GInput := TJSHTMLTextAreaElement(Field('notes-editor'));
            GInput.value := CDraft;
            GInput.selectionStart := 5;
            GInput.selectionEnd := 9;

            if window.innerWidth <= 960 then
            begin
              Required('action-panel-design').click;
            end;
            Check(Find(NyxCanvasResizeGripID(nraBoth)) <> nil, 'Actual public canvas handle is mounted');
            GStage := 5;
          end;
        end;
      5:
        begin
          { Panel navigation and a driver-requested viewport change deliver
            ordinary resize frames before pointer capture starts. }

          if Find(NyxCanvasResizeGripID(nraBoth)) <> nil then
          begin
            document.body.setAttribute('data-nyx-guides-input', 'ready');
            GStage := 1;
          end;
        end;
      1:
        begin

          if Pos('matches other-editor', Required('studio-status').textContent) > 0 then
          begin
            Check((Pos('217', Required('studio-status').textContent) > 0) and
              (Pos('143', Required('studio-status').textContent) > 0), 'Real pointer listeners snap both sibling dimensions');
            LGuide := TJSHTMLElement(document.querySelector('[data-nyx-alignment-guide="0"]'));
            Check((LGuide <> nil) and (LGuide.getBoundingClientRect.width = 217),
              'Browser paints the proposed equal-width measurement');
            Check(window.getComputedStyle(LGuide).getPropertyValue('pointer-events') = 'none',
              'Guide paint cannot intercept browser input');
            Check(GEditor.value = GBefore, 'Pointer preview never edits accepted Pascal');
            RetainedInput;
            document.body.setAttribute('data-nyx-guides-input', 'release');
            GStage := 2;
          end;
        end;
      2:
        begin

          if Pos('Design / Pascal updated', Required('studio-status').textContent) = 1 then
          begin
            GAfter := TJSHTMLTextAreaElement(Field('studio-code')).value;
            Check((GAfter <> GBefore) and (Pos('.Width(217)', GAfter) > 0) and
              (Pos('.Height(143)', GAfter) > 0), 'Ordinary browser worker publishes snapped paired source');
            Check(document.querySelectorAll('[data-nyx-alignment-guide]').length = 0,
              'Release removes all transient guide geometry');
            RetainedInput;
            Required('action-undo').click;
            GStage := 3;
          end;
        end;
      3:
        begin

          if TJSHTMLTextAreaElement(Field('studio-code')).value = GBefore then
          begin
            Check(True, 'One synchronized editor Undo restores exact source');
            RetainedInput;
            Required('action-redo').click;
            GStage := 4;
          end;
        end;
      4:
        begin

          if TJSHTMLTextAreaElement(Field('studio-code')).value = GAfter then
          begin
            Check(True, 'One synchronized editor Redo restores exact source');
            RetainedInput;
            document.body.setAttribute(CResult, 'passed');
            document.body.setAttribute(CChecks, IntToStr(GChecks));
            Exit;
          end;
        end;
    end;
    window.setTimeout(@Poll, 25);
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;
procedure ObserveCompact;
var
  LBody: TJSHTMLElement;
  LResult: TNyxText;
begin
  Inc(GPolls);

  if GFrame.contentDocument <> nil then
  begin
    LBody := TJSHTMLElement(GFrame.contentDocument.body);
    LResult := LBody.getAttribute(CResult);

    if LResult = 'passed' then
    begin
      document.body.setAttribute(CResult, 'passed');
      document.body.setAttribute(CChecks, LBody.getAttribute(CChecks));
      Exit;
    end;

    if LResult = 'failed' then
    begin
      Failed(LBody.getAttribute(CError));
      Exit;
    end;
  end;

  if GPolls > 270 then
  begin
    Failed('Exact-390 Studio journey did not finish');
    Exit;
  end;
  window.setTimeout(@ObserveCompact, 100);
end;

begin
  { The project must be prepared semantically. The fixture only drives input
    behavior that the document API cannot establish; it never imports or authors
    its own replacement document. Synthetic clicks are not trusted hardware. }

  if Pos('workspace=', window.location.search) = 0 then
  begin
    Failed('Supply an explicit MCP-owned alignment workspace in the URL');
  end
  else if Pos('host=1&', window.location.search) > 0 then
  begin
    GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
    GFrame.style.cssText := 'width:390px;height:900px;border:0;display:block;';
    TJSHTMLElement(document.body).style.setProperty('margin', '0');
    GFrame.src := window.location.pathname +
      StringReplace(window.location.search, 'host=1&', '', []);
    document.body.appendChild(GFrame);
    window.setTimeout(@ObserveCompact, 100);
  end
  else
  begin
    GStudio := TNyxStudio.Create;
    GStudio.Run(False);
    GStudio.ConnectAgents;
    window.setTimeout(@Poll, 25);
  end;
end.
