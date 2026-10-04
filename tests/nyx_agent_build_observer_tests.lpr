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

program nyx_agent_build_observer_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Web, nyx.text, nyx.studio.browser;

var
  GStudio: TNyxStudio;
  GPhase: Integer;
  GPolls: Integer;
  GCount: Integer;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
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
  LActivity: TJSHTMLElement;
  LDiagnostics: TJSHTMLElement;
begin
  try
    Inc(GPolls);

    if GPolls > 1800 then
    begin
      raise Exception.Create('Build observer timed out at phase ' + IntToStr(GPhase));
    end;
    LActivity := Find('studio-agents-activity');
    LDiagnostics := Find('studio-compiler-diagnostics');
    case GPhase of
      0:
        begin

          if (Find('studio-agents-status') <> nil) and
            (Pos('Agents edit', Find('studio-agents-status').textContent) > 0) then
          begin
            TJSHTMLButtonElement(Find('action-code')).click;
            document.body.setAttribute('data-build-phase', 'ready');
            GPhase := 1;
          end;
        end;
      1:
        begin

          if (LActivity <> nil) and (Pos('nyx_build', LActivity.textContent) > 0) and
            (Pos('running', LActivity.textContent) > 0) then
          begin
            Check(Pos('running', LActivity.textContent) > 0, 'Observing Studio sees compiler job start');
            document.body.setAttribute('data-build-phase', 'working');
            GPhase := 2;
          end;
        end;
      2:
        begin

          if (LDiagnostics <> nil) and
            (Pos('MissingApplicationFunction', LDiagnostics.textContent) > 0) then
          begin
            Check(Pos('failed', LActivity.textContent) > 0, 'Observing Studio sees compiler failure');
            Check(Find('action-compiler-diagnostic-0') <> nil, 'Mapped compiler location has ordinary source action');
            Check(Find('studio-agent-conflict') = nil, 'Agent build retains an unconflicted observer');
            document.body.setAttribute('data-build-phase', 'diagnostics');
            GPhase := 3;
          end;
        end;
      3:
        begin

          if (LDiagnostics <> nil) and (Find('studio-compiler-stale') <> nil) then
          begin
            Check(Pos('earlier Pascal', Find('studio-compiler-stale').textContent) > 0,
              'Later edit visibly marks prior compiler diagnostics stale');
            document.body.setAttribute('data-build-phase', 'passed');
            document.body.setAttribute('data-nyx-build-observer', 'passed');
            document.body.setAttribute('data-nyx-build-observer-checks', IntToStr(GCount));
            Exit;
          end;
        end;
    end;
    window.setTimeout(@Poll, 100);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-build-phase', 'failed');
      document.body.setAttribute('data-build-error', LException.Message);
    end;
  end;
end;

begin
  GStudio := TNyxStudio.Create;
  GStudio.Run(False);
  GStudio.ConnectAgents;
  TJSHTMLButtonElement(Find('action-agents')).click;
  window.setTimeout(@Poll, 100);
end.
