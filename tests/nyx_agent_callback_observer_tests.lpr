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
program nyx_agent_callback_observer_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, JS, Web, nyx.text, nyx.studio.browser;

var
  GStudio: TNyxStudio;
  GPhase: Integer;
  GCount: Integer;
  GPolls: Integer;
  GOrderedSource: TNyxText;
  GRemovedSource: TNyxText;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
end;

procedure Click(const AID: TNyxText);
begin

  if Find(AID) = nil then
  begin
    raise Exception.Create('Missing callback observer control: ' + AID);
  end;
  TJSHTMLButtonElement(Find(AID)).click;
end;

function Source: TNyxText;
var
  LControl: TJSHTMLElement;
begin
  LControl := Find('studio-code');

  if not (LControl is TJSHTMLTextAreaElement) then
  begin
    LControl := TJSHTMLElement(LControl.querySelector('textarea'));
  end;
  Result := TJSHTMLTextAreaElement(LControl).value;
end;

function Policy: TNyxText;
var
  LControl: TJSHTMLElement;
begin
  LControl := Find('event-click-policy');

  if not (LControl is TJSHTMLSelectElement) then
  begin
    LControl := TJSHTMLElement(LControl.querySelector('select'));
  end;
  Result := TJSHTMLSelectElement(LControl).value;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GCount);
end;

procedure Poll;
var
  LCount: TNyxText;
  LSecond: TNyxText;
  LFirst: TJSHTMLElement;
begin
  try
    Inc(GPolls);

    if GPolls > 900 then
    begin
      raise Exception.Create('Callback observer timeout in phase ' + IntToStr(GPhase));
    end;
    LCount := '';

    if Find('event-click-count') <> nil then
    begin
      LCount := Find('event-click-count').textContent;
    end;
    LSecond := document.body.getAttribute('data-callback-second');
    LFirst := Find('event-click-callback-0-source');
    case GPhase of
      0:
        begin

          if (Find('studio-agents-status') <> nil) and
            (Pos('Agents edit', Find('studio-agents-status').textContent) > 0) then
          begin
            Click('action-code');
            Click('inspector-tab-events');
            document.body.setAttribute('data-callback-phase', 'ready');
            GPhase := 1;
          end;
        end;
      1:
        begin

          if LCount = '2 registrations' then
          begin
            Check(Pos('TODO:', Source) > 0, 'Live accepted TODO implementations reach the code pane');
            Check(Pos('nyx_callbacks', Find('studio-agents-activity').textContent) > 0,
              'Operator sees semantic callback activity');
            document.body.setAttribute('data-callback-phase', 'added');
            GPhase := 2;
          end;
        end;
      2:
        begin

          if (LSecond <> '') and (LFirst <> nil) and
            (Pos(LSecond, LFirst.textContent) > 0) and
            (Policy = 'ui-queue') then
          begin
            GOrderedSource := Source;
            Check(LCount = '2 registrations', 'Inspector visibly retains two ordered registrations');
            document.body.setAttribute('data-callback-phase', 'moved');
            GPhase := 3;
          end;
        end;
      3:
        begin

          if LCount = '1 registrations' then
          begin
            GRemovedSource := Source;
            Check(Pos('procedure ' + LSecond + '.Invoke', GRemovedSource) > 0,
              'Warned semantic removal retains the Pascal implementation');
            Click('action-undo');
            GPhase := 4;
          end;
        end;
      4:
        begin

          if LCount = '2 registrations' then
          begin
            Check(Source = GOrderedSource, 'Ordinary editor Undo restores the exact ordered pair');
            document.body.setAttribute('data-callback-phase', 'undone');
            GPhase := 5;
          end;
        end;
      5:
        begin

          if LCount = '1 registrations' then
          begin
            Check(Source = GRemovedSource, 'Semantic Redo restores the exact reviewed removal');
            Check(Pos('studio-agent-conflict', document.body.innerHTML) = 0, 'No observer conflict');
            document.body.setAttribute('data-callback-phase', 'passed');
            document.body.setAttribute('data-nyx-callback-observer', 'passed');
            document.body.setAttribute('data-nyx-callback-checks', IntToStr(GCount));
            Exit;
          end;
        end;
    end;
    window.setTimeout(@Poll, 100);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-callback-phase', 'failed');
      document.body.setAttribute('data-callback-error', LException.Message);
    end;
  end;
end;

begin
  GStudio := TNyxStudio.Create;
  GStudio.Run(False);
  GStudio.ConnectAgents;
  Click('action-agents');
  window.setTimeout(@Poll, 100);
end.
