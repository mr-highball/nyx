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

program nyx_agent_bridge_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, JS, Web, nyx.text, nyx.studio.session, nyx.studio.projects,
  nyx.studio.agentbridge, nyx.studio.agentview, nyx.studio.workspaces;

var
  GSession: TNyxStudioSession;
  GBridge: TNyxStudioAgentBridge;
  GLocal: TNyxText;
  GShared: TNyxText;
  GDraft: TNyxText;
  GPhase: Integer;
  GChecks: Integer;
  GRevision: Integer;
  GPolls: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Poll;
var
  LState: TNyxStudioAgentView;
  LIndex: Integer;
begin
  try
    Inc(GPolls);

    if GPolls > 400 then
    begin
      raise Exception.Create('Bridge protection exceeded its functional poll budget');
    end;
    LState := GBridge.State;
    case GPhase of
      0:
        begin

          if LState.Conflict then
          begin
            Check(not GBridge.SourceSynchronized,
              'Protected local conflict cannot borrow source for server diagnostic navigation');
            Check(EncodeNyxProject(GSession.ProjectSnapshot) = GLocal,
              'Connecting to another active design retains recovered pair and draft');
            Check(Pos('retained', LState.Status) > 0, 'Conflict tells operator local work is retained');
            GBridge.Pause;
            Check(not GBridge.Enabled and not GBridge.State.Busy,
              'Pause terminates observation and any outstanding request');
            Check(EncodeNyxProject(GSession.ProjectSnapshot) = GLocal, 'Pause preserves exact local files');
            GBridge.Connect;
            GPhase := 1;
          end;
        end;
      1:
        begin

          if LState.Conflict then
          begin
            Check(EncodeNyxProject(GSession.ProjectSnapshot) = GLocal, 'Reconnect still protects local recovery');
            GBridge.AcceptRemote;
            GPhase := 2;
          end;
        end;
      2:
        begin

          if LState.Connected and not LState.Conflict and not LState.Busy then
          begin
            Check((GSession.Document.Title <> 'Recovered local workshop') and
              (GSession.DraftSource = GSession.Source), 'Explicit operator resolution adopts shared pair');
            GRevision := LState.Revision;
            GShared := EncodeNyxProject(GSession.ProjectSnapshot);
            Check(GBridge.SourceSynchronized,
              'Exact resolved observer frame admits its locally retained source');
            { Synchronous typing cannot receive an XHR acknowledgement between
              these calls. Keep the in-flight head and coalesce unsent draft
              updates; preserve the exact final Unicode draft, with two commits. }
            for LIndex := 1 to 80 do
            begin
              GDraft := GSession.Source + #10 + '// Draft ' + IntToStr(LIndex) + TNyxText(' 🌙漢字');
              GSession.SetSourceDraft(GDraft);

              if (LIndex = 1) or (LIndex = 80) then
              begin
                Check(not GBridge.SourceSynchronized,
                  'Unsynchronized local source cannot be substituted into a compiler report');
              end;
              GBridge.RecordLocal;
            end;
            GPhase := 3;
          end;
        end;
      3:
        begin

          if (LState.Revision >= GRevision + 2) and not LState.Busy then
          begin
            Check(not LState.Conflict and (LState.Revision = GRevision + 2),
              'Unsent draft coalescing preserves in-flight acknowledgement and bounds revisions');
            Check(GSession.DraftSource = GDraft, 'Coalesced draft retains exact last supplementary Unicode text');
            Check(GBridge.SourceSynchronized,
              'Acknowledged exact pair restores source identity without replacing the draft');
            { Restore the borrowed qualification project's original accepted pair.
              This fixture does not leave its private source draft behind. }
            GSession.DiscardSourceDraft;
            GBridge.RecordLocal;
            GPhase := 4;
          end;
        end;
      4:
        begin

          if (LState.Revision = GRevision + 3) and not LState.Busy and GBridge.SourceSynchronized then
          begin
            Check(EncodeNyxProject(GSession.ProjectSnapshot) = GShared,
              'Acknowledged cleanup restores the exact original shared pair');
            GBridge.Pause;
            GBridge.Free;
            GBridge := nil;
            GSession.Free;
            GSession := nil;
            document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' live bridge checks';
            document.body.setAttribute('data-nyx-agent-bridge', 'passed');
            Exit;
          end;
        end;
    end;
    window.setTimeout(@Poll, 100);
  except
    on LException: Exception do
    begin
      GBridge.Free;
      GBridge := nil;
      GSession.Free;
      GSession := nil;
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-nyx-agent-bridge', 'failed');
    end;
  end;
end;

begin
  GSession := TNyxStudioSession.Create;

  if window.location.search = '' then
  begin
    GBridge := TNyxStudioAgentBridge.Create(GSession, nil);
  end
  else
  begin
    { An explicitly selected owned project avoids touching a service's primary
      project. Unknown or additional query fields never fall back to primary. }

    if (Pos('?workspace=', window.location.search) <> 1) or
      (Pos('&', window.location.search) <> 0) then
    begin
      raise Exception.Create('Bridge fixture requires one explicit project reference');
    end;
    GBridge := TNyxStudioAgentBridge.Create(GSession, nil,
      NyxWorkspace(decodeURIComponent(Copy(window.location.search, 12, MaxInt))));
  end;
  GSession.SetTitle('Recovered local workshop');
  GSession.SetSourceDraft(GSession.Source + #10 + '// recovered companion 🌙漢字');
  GLocal := EncodeNyxProject(GSession.ProjectSnapshot);
  GBridge.Connect;
  window.setTimeout(@Poll, 100);
end.
