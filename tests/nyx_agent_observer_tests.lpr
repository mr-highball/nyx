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

program nyx_agent_observer_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, JS, Web, nyx.text, nyx.studio.browser;

var
  GStudio: TNyxStudio;
  GPhase: Integer;
  GCount: Integer;
  GPolls: Integer;
  GSource: TNyxText;
  GFrame: TJSHTMLIFrameElement;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
end;

function Field(const AID: TNyxText): TJSHTMLElement;
begin
  Result := Find(AID);

  if (Result <> nil) and not ((Result is TJSHTMLInputElement) or
    (Result is TJSHTMLTextAreaElement)) then
  begin
    Result := TJSHTMLElement(Result.querySelector('input,textarea'));
  end;
end;

procedure Click(const AID: TNyxText);
var
  LButton: TJSHTMLElement;
begin
  LButton := Find(AID);

  if LButton = nil then
  begin
    raise Exception.Create('Missing agent control: ' + AID);
  end;
  TJSHTMLButtonElement(LButton).click;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GCount);
end;

procedure Change(const AID, AValue: TNyxText);
var
  LField: TJSHTMLElement;
begin
  LField := Field(AID);

  if LField = nil then
  begin
    raise Exception.Create('Missing agent editor: ' + AID);
  end;
  TJSHTMLInputElement(LField).value := AValue;
  LField.dispatchEvent(TJSEvent.new('change'));
end;

procedure Poll;
var
  LStatus: TNyxText;
  LActivity: TNyxText;
  LBadge: TJSHTMLElement;
  LTitle: TJSHTMLElement;
begin
  try
    Inc(GPolls);

    if GPolls > 800 then
    begin
      raise Exception.Create('Observer timed out in phase ' + IntToStr(GPhase));
    end;
    LStatus := '';
    LActivity := '';

    if Find('studio-agents-status') <> nil then
    begin
      LStatus := Find('studio-agents-status').textContent;
    end;

    if Find('studio-agents-activity') <> nil then
    begin
      LActivity := Find('studio-agents-activity').textContent;
    end;
    LBadge := Find('observer-badge');
    case GPhase of
      0:
        begin

          if Pos('Agents edit', LStatus) > 0 then
          begin
            Check(not TJSHTMLButtonElement(Find('action-agent-readOnly')).disabled, 'Connected operator permission control');

            if Find('action-panel-project') <> nil then
            begin
              Click('action-panel-project');
            end;
            Change('project-title', 'Agent observer ready');

            if Find('action-panel-design') <> nil then
            begin
              Click('action-panel-design');
            end;
            GPhase := 1;
          end;
        end;
      1:
        begin

          if (LBadge <> nil) and (LBadge.textContent = 'Live 🌙漢字') then
          begin
            Check(True, 'Already-open Nyx canvas receives MCP-created badge');
            Click('action-code');
            GSource := TJSHTMLTextAreaElement(Field('studio-code')).value;
            Check((Pos('INyxBadge', GSource) > 0) and (Pos('Live 🌙漢字', GSource) > 0),
              'Agent edit reaches crafted specialized companion source');
            Check(Pos('nyx_transaction', LActivity) > 0, 'Activity visibly identifies semantic edit');
            Click('action-undo');
            GPhase := 2;
          end;
        end;
      2:
        begin

          if LBadge = nil then
          begin
            Check(True, 'Editor Undo reverses complete agent transaction');
            Click('action-redo');
            GPhase := 3;
          end;
        end;
      3:
        begin

          if LBadge <> nil then
          begin
            Check(True, 'Editor Redo restores agent transaction');
            Click('action-agent-readOnly');
            GPhase := 4;
          end;
        end;
      4:
        begin

          if (Pos('readOnly', LStatus) > 0) and (Pos('refused: Agent edits require', LActivity) > 0) then
          begin
            Check(Find('action-agent-readOnly').getAttribute('data-variant') = 'primary',
              'Read-only choice visibly active');
            Check(LBadge <> nil, 'Refused agent mutation retains mounted design');
            Click('action-agent-edit');
            GPhase := 5;
          end;
        end;
      5:
        begin
          if Pos('Agent observer changed', document.title) > 0 then
          begin
            Check(True, 'Enabled agent edits resume in same shared session');
            GSource := TJSHTMLTextAreaElement(Field('studio-code')).value;
            Change('studio-code', GSource + #10 + '{ pending observer draft 🌙 }');
            GPhase := 6;
          end;
        end;
      6:
        begin

          if Pos('refused: Resolve the pending Pascal draft', LActivity) > 0 then
          begin
            Check(TJSHTMLTextAreaElement(Field('studio-code')).value = GSource + #10 + '{ pending observer draft 🌙 }',
              'Pending exact draft survives refused MCP mutation');
            Click('action-reset-source');
            GPhase := 7;
          end;
        end;
      7:
        begin

          if (LBadge <> nil) and (LBadge.textContent = 'Agent observer finished') then
          begin
            Check(TJSHTMLTextAreaElement(Field('studio-code')).value <>
              GSource + #10 + '{ pending observer draft 🌙 }', 'Draft restoration reaches shared session');
            Check(Pos('studio-agent-conflict', document.body.innerHTML) = 0, 'No spurious conflict after ordered publications');
            document.body.setAttribute('data-nyx-agent-observer', 'passed');
            document.body.setAttribute('data-nyx-agent-observer-count', IntToStr(GCount));
            document.body.setAttribute('data-nyx-agent-observer-width', IntToStr(window.innerWidth));
            { Stop polling while retaining the mounted UI for DOM/screenshot
              inspection. The shell/document remain owned by the test app. }
            GPhase := 8;
            Exit;
          end;
        end;
    end;
    window.setTimeout(@Poll, 100);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-nyx-agent-observer', 'failed');
      document.body.setAttribute('data-nyx-agent-observer-error', LException.Message);
      if Find('studio-agents-status') <> nil then
      begin
        document.body.setAttribute('data-nyx-agent-observer-sync', Find('studio-agents-status').textContent);
      end;
      if Field('project-title') <> nil then
      begin
        document.body.setAttribute('data-nyx-agent-observer-title', TJSHTMLInputElement(Field('project-title')).value);
      end;
      GStudio.Free;
      GStudio := nil;
    end;
  end;
end;

procedure PollFrame;
var
  LBody: TJSHTMLElement;
  LState: TNyxText;
begin
  LBody := TJSHTMLElement(GFrame.contentDocument.body);

  if LBody <> nil then
  begin
    LState := LBody.getAttribute('data-nyx-agent-observer');

    if (LState = 'passed') or (LState = 'failed') then
    begin
      document.body.setAttribute('data-nyx-agent-observer', LState);
      document.body.setAttribute('data-nyx-agent-observer-count', LBody.getAttribute('data-nyx-agent-observer-count'));
      document.body.setAttribute('data-nyx-agent-observer-width', LBody.getAttribute('data-nyx-agent-observer-width'));
      document.body.setAttribute('data-nyx-agent-observer-error', LBody.getAttribute('data-nyx-agent-observer-error'));
      Exit;
    end;
  end;
  window.setTimeout(@PollFrame, 100);
end;

begin

  if Pos('host=1', window.location.search) > 0 then
  begin
    GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
    GFrame.style.setProperty('width', '390px');
    GFrame.style.setProperty('height', '950px');
    GFrame.style.setProperty('border', '0');
    GFrame.src := 'agent-observer.html';
    document.body.appendChild(GFrame);
    window.setTimeout(@PollFrame, 100);
  end
  else
  begin
    GStudio := TNyxStudio.Create;
    GStudio.Run(False);
    GStudio.ConnectAgents;
    Click('action-agents');
    window.setTimeout(@Poll, 100);
  end;
end.
