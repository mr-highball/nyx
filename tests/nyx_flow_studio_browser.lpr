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
program nyx_flow_studio_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, JS, Web, nyx.text, nyx.studio.browser, nyx.studio.drag;

const
  CResult = 'data-nyx-flow';
  CInput = 'data-nyx-flow-input';
  CError = 'data-nyx-flow-error';

var
  GStudio: TNyxStudio;
  GEditor: TJSHTMLTextAreaElement;
  GBefore, GAfter: TNyxText;
  GStage, GPolls, GChecks: Integer;

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

  if not ((Result is TJSHTMLTextAreaElement) or (Result is TJSHTMLSelectElement)) then
  begin
    Result := TJSHTMLElement(Result.querySelector('textarea,select'));
  end;

  if Result = nil then
  begin
    raise Exception.Create('Missing ordinary Studio input: ' + AID);
  end;
end;

procedure Input(const AValue: TNyxText);
begin
  document.body.setAttribute(CInput, AValue);
end;

procedure RetainedSource;
begin
  Check(Field('studio-code') = GEditor, 'Ordinary source control retains identity');
end;

procedure Proposal(const ACaption: TNyxText; AVertical: Boolean);
var
  LInk: TJSHTMLElement;
begin
  Check(Pos(ACaption, Required('studio-status').textContent) > 0,
    'Actual host drag resolves the expected typed edge');
  LInk := TJSHTMLElement(document.querySelector('[data-nyx-drop-edge="0"]'));
  Check(LInk <> nil, 'Actual adapter paints insertion geometry');

  if AVertical then
  begin
    Check(LInk.getBoundingClientRect.width = 3, 'Row insertion is a three-pixel strip');
  end
  else
  begin
    Check(LInk.getBoundingClientRect.height = 3, 'Column insertion is a three-pixel strip');
  end;
  Check(window.getComputedStyle(LInk).getPropertyValue('pointer-events') = 'none',
    'Insertion paint cannot intercept host input');
  Check(GEditor.value = GBefore, 'Hover leaves accepted Pascal exact');
  RetainedSource;
end;

{ This observer never authors a document or fabricates a drag lease. An external
  Pascal driver intercepts actual browser source data and supplies host drag
  input. Ordinary Studio, its worker and shared paired history own admission. }
procedure Poll;
var
  LStatus: TJSHTMLElement;
  LChoice: TJSHTMLSelectElement;
begin
  try
    Inc(GPolls);

    if GPolls > 2400 then
    begin
      raise Exception.Create('Flow Studio input journey timed out');
    end;
    case GStage of
      0:
        begin
          LStatus := Find('studio-agents-status');

          if (LStatus <> nil) and (Pos('Agents edit', LStatus.textContent) > 0) and
            (Find('notes-editor') <> nil) and
            ((Pos('compact=1', window.location.search) = 0) or (window.innerWidth = 390)) then
          begin
            Check(Pos('Space to build', document.title) > 0, 'Explicit English semantic project is observed');
            Required('action-agents').click;
            Required('action-code').click;
            GEditor := TJSHTMLTextAreaElement(Field('studio-code'));
            GBefore := GEditor.value;
            Check(Pos('INyxMemo', GBefore) > 0, 'Crafted specialized MCP source is consumed');
            Required('notes-editor').click;

            if window.innerWidth <= 960 then
            begin
              Required('action-panel-design').click;
            end;
            LChoice := TJSHTMLSelectElement(Field(NyxStudioDropPositionID));
            LChoice.value := NyxStudioAutomaticPlacement;
            LChoice.dispatchEvent(TJSEvent.new('change'));
            Check(Required(NyxStudioDragMoveID).getBoundingClientRect.height >= 32,
              'Compact and desktop canvas expose the ordinary drag source');
            Input('ready');
            GStage := 1;
          end;
        end;
      1:
        begin

          if Pos('Drop before', Required('studio-status').textContent) = 1 then
          begin
            Proposal('Drop before', False);
            Input('release');
            GStage := 2;
          end;
        end;
      2:
        begin

          if GEditor.value <> GBefore then
          begin
            GAfter := GEditor.value;
            Check(Required('right-layout').querySelector('[data-node="notes-editor"]') <> nil,
              'Actual worker reparents the memo into the other panel');
            Check(TJSHTMLTextAreaElement(Field('notes-editor')).value = 'Keep this English draft.',
              'Accepted English memo text survives structural admission');
            Check(document.querySelectorAll('[data-nyx-drop-edge]').length = 0,
              'Drop retires transient insertion paint');
            RetainedSource;
            Required('action-undo').click;
            GStage := 3;
          end;
        end;
      3:
        begin

          if GEditor.value = GBefore then
          begin
            Check(Required('left-layout').querySelector('[data-node="notes-editor"]') <> nil,
              'One Undo restores the exact source and authored parent');
            RetainedSource;
            Required('action-redo').click;
            GStage := 4;
          end;
        end;
      4:
        begin

          if GEditor.value = GAfter then
          begin
            Check(True, 'One Redo restores the exact paired publication');
            Required('action-undo').click;
            GStage := 5;
          end;
        end;
      5:
        begin

          if GEditor.value = GBefore then
          begin
            Required('notes-editor').click;
            Input('cancel-ready');
            GStage := 6;
          end;
        end;
      6:
        begin

          if Pos('Drop before', Required('studio-status').textContent) = 1 then
          begin
            Proposal('Drop before', False);
            Input('cancel');
            GStage := 7;
          end;
        end;
      7:
        begin

          if (document.body.getAttribute(CInput) = 'cancel-ended') and
            (Pos('Ready to design', Required('studio-status').textContent) = 1) then
          begin
            Check(GEditor.value = GBefore, 'Actual host drag cancellation preserves accepted source');
            Check(Required('left-layout').querySelector('[data-node="notes-editor"]') <> nil,
              'Cancellation retains the accepted authored parent');
            Check(document.querySelectorAll('[data-nyx-drop-edge]').length = 0,
              'Refused release clears insertion paint');
            RetainedSource;
            Required('right-layout').click;
            Input('row-ready');
            GStage := 8;
          end;
        end;
      8:
        begin

          if Pos('Drop before', Required('studio-status').textContent) = 1 then
          begin
            Proposal('Drop before', True);
            Input('row-release');
            GStage := 9;
          end;
        end;
      9:
        begin

          if GEditor.value <> GBefore then
          begin
            Check(Required('right-layout').compareDocumentPosition(Required('left-layout')) = 4,
              'Actual row drop reorders sibling panels');
            RetainedSource;
            Required('action-undo').click;
            GStage := 10;
          end;
        end;
      10:
        begin

          if GEditor.value = GBefore then
          begin
            Check(True, 'Row reordering has one exact paired Undo');
            RetainedSource;
            document.body.setAttribute(CResult, 'passed');
            document.body.setAttribute('data-nyx-flow-checks', IntToStr(GChecks));
            Exit;
          end;
        end;
    end;
    window.setTimeout(@Poll, 25);
  except
    on LException: Exception do
    begin
      document.body.setAttribute(CResult, 'failed');
      document.body.setAttribute(CError, LException.Message + ' / stage ' + IntToStr(GStage));
    end;
  end;
end;

begin

  if Pos('workspace=', window.location.search) = 0 then
  begin
    document.body.setAttribute(CResult, 'failed');
    document.body.setAttribute(CError, 'Supply an explicit semantic flow workspace');
  end
  else
  begin
    GStudio := TNyxStudio.Create;
    GStudio.Run(False);
    GStudio.ConnectAgents;
    window.setTimeout(@Poll, 25);
  end;
end.
