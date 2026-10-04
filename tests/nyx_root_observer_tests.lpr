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
program nyx_root_observer_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, JS, Web, nyx.text, nyx.studio.browser, nyx.studio.rootview;

var
  GStudio: TNyxStudio;
  GPhase: Integer;
  GPolls: Integer;
  GChecks: Integer;
  GAuthored: TNyxText;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
end;

procedure Click(const AID: TNyxText);
begin

  if Find(AID) = nil then
  begin
    raise Exception.Create('Missing root observer control: ' + AID);
  end;
  Find(AID).click;
end;

function Source: TNyxText;
var
  LCode: TJSHTMLElement;
begin
  LCode := Find('studio-code');

  if not (LCode is TJSHTMLTextAreaElement) then
  begin
    LCode := TJSHTMLElement(LCode.querySelector('textarea'));
  end;
  Result := TJSHTMLTextAreaElement(LCode).value;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Phase(const AName: TNyxText);
begin
  document.body.setAttribute('data-root-phase', AName);
  GPolls := 0;
  Inc(GPhase);
end;

{ An ordinary Nyx Studio observes semantic creation/removal, displays its public
  confirmation controls and executes its real Undo action. This page owns no
  alternative document model and never calls MCP or injects editor scripts. }
procedure Poll;
begin
  try
    Inc(GPolls);

    if GPolls >= 600 then
    begin
      raise Exception.Create('Root observer exceeded its bounded wait');
    end;
    case GPhase of
      0:
        begin

          if (Find('studio-agents-status') <> nil) and
            (Pos('Agents edit', Find('studio-agents-status').textContent) > 0) then
          begin
            Click('action-code');
            Phase('ready');
          end;
        end;
      1:
        begin

          if (Pos('''review-workshop''', Source) > 0) and
            (Pos('// TODO: implement ', Source) > 0) and
            (Find('active-view-label').textContent = 'review-definition') then
          begin
            GAuthored := Source;

            if Find('action-panel-project') <> nil then
            begin
              Click('action-panel-project');
            end;
            Click(NyxStudioReviewRootID);
            Check(Find('root-removal-warning') <> nil, 'Actual Nyx confirmation renders its retained-code warning');
            Check(TJSHTMLButtonElement(Find(NyxStudioRemoveRootID)).disabled,
              'Referenced reusable definition is visibly blocked');
            Click(NyxStudioCancelRootID);
            Check(Find('studio-root-removal') = nil, 'Cancel dismisses only the confirmation');
            Phase('authored');
          end;
        end;
      2:
        begin

          if (Pos('''review-workshop''', Source) = 0) and
            (Pos('// TODO: implement ', Source) > 0) then
          begin
            Check(Find('active-view-label').textContent = 'home', 'Observer follows the surviving home view');
            Check(Pos('nyx_roots', Find('studio-agents-activity').textContent) > 0,
              'Agent root cleanup is visible in ordinary activity');
            Check(Find('review-definition') = nil, 'Removed runtime view is retired from the canvas');
            Click('action-undo');
            Phase('undo-requested');
          end;
        end;
      3:
        begin

          if Source = GAuthored then
          begin
            Check(True, 'Ordinary Studio Undo restores the exact authored pair source');
            Phase('undone');
          end;
        end;
      4:
        begin

          if (Pos('''review-workshop''', Source) = 0) and
            (Pos('// TODO: implement ', Source) > 0) then
          begin
            Check(Pos('studio-agent-conflict', document.body.innerHTML) = 0, 'No synchronization conflict');
            Phase('passed');
            document.body.setAttribute('data-nyx-root-observer', 'passed');
            document.body.setAttribute('data-nyx-root-observer-checks', IntToStr(GChecks));
            Exit;
          end;
        end;
    end;
    window.setTimeout(@Poll, 100);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-root-phase', 'failed');
      document.body.setAttribute('data-root-error', LException.Message);
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
