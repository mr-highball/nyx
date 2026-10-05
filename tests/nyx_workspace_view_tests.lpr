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
program nyx_workspace_view_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.events, nyx.behavior,
  nyx.studio.agents, nyx.studio.workspaces, nyx.studio.agentview,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Interfaces, Forms, StdCtrls, nyx.render.lcl;{$endif}

type
  { Borrowed adapter event observer. It records semantic navigation/consent
    only; full service navigation is qualified by the real protocol journey. }
  TWorkspaceActions = class
  public
    Count: Integer;
    Reference: TNyxWorkspaceRef;
    LastAction: TNyxText;
    procedure Handle(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;
  {$ifndef PAS2JS}
  TButtonAccess = class(TCustomButton);
  {$endif}

var
  GChecks: Integer;
  GPrimary: TNyxAgentSession;
  GProjects: TNyxStudioWorkspaces;
  GDocument: TNyxDocument;
  GActions: TWorkspaceActions;
  {$ifdef PAS2JS}
  GRenderer: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  {$else}
  GRenderer: TNyxLCLRenderer;
  GHost: TForm;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TWorkspaceActions.Handle(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if AEvent.Trigger <> ntClick then
  begin
    Exit;
  end;
  Inc(Count);
  LastAction := ANode.ID;

  if ANode.Extensions.Has(NyxStudioWorkspaceJumpKey) then
  begin
    Reference := NyxWorkspace(ANode.Extensions.Value(NyxStudioWorkspaceJumpKey).AsText);
  end
  else if ANode.Extensions.Has(NyxStudioWorkspaceCloseKey) then
  begin
    Reference := NyxWorkspace(ANode.Extensions.Value(NyxStudioWorkspaceCloseKey).AsText);
  end;
end;

procedure Click(const AID: TNyxText);
begin
  {$ifdef PAS2JS}
  GRenderer.ElementFor(AID).click;
  {$else}
  TButtonAccess(GRenderer.ControlFor(AID)).Click;
  Application.ProcessMessages;
  {$endif}
end;

function Text(const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := GRenderer.ElementFor(AID).textContent;
  {$else}
  Result := TNyxText(TLabel(GRenderer.ControlFor(AID)).Caption);
  {$endif}
end;

procedure Release;
begin
  FreeAndNil(GRenderer);
  {$ifdef PAS2JS}

  if GHost <> nil then
  begin
    GHost.remove;
    GHost := nil;
  end;
  {$else}
  FreeAndNil(GHost);
  {$endif}
  FreeAndNil(GDocument);
  FreeAndNil(GActions);
  FreeAndNil(GProjects);
  FreeAndNil(GPrimary);
end;

procedure Run;
var
  LFirst: TNyxWorkspaceRef;
  LSecond: TNyxWorkspaceRef;
  LState: TNyxStudioAgentView;
  LRoot: TNyxNode;
  LBefore: TNyxText;
begin
  {$ifndef PAS2JS}
  Application.Initialize;
  {$endif}
  GPrimary := TNyxAgentSession.Create;
  GProjects := TNyxStudioWorkspaces.Create(GPrimary, 'view-fixture');
  LFirst := GProjects.CreateProject('Moon workshop 🌙漢字', nwbEmpty, GPrimary.Revision);
  LSecond := GProjects.CreateProject('Second workshop', nwbEmpty, GPrimary.Revision);
  GProjects.RecordRequest('connection-a', 'Scooty', LFirst);
  GProjects.RecordRequest('connection-b', 'Scooty', LFirst);
  LState := DefaultNyxStudioAgentView;
  LState.Connected := True;
  LState.CanCloseWorkspace := True;
  LState.Status := 'Agents edit';
  LState.Workspaces := GProjects.Observe;
  LState.CloseWorkspace := LSecond;
  LState.CloseRevision := GProjects.Find(LSecond).Revision;
  LState.CloseLabel := 'Second workshop';
  GDocument := TNyxDocument.Create;
  LRoot := TNyxNode.Create(nkPage, 'workspace-view-root');
  GDocument.AddPage(LRoot);
  LRoot.Add(BuildNyxStudioAgents(LState));
  GActions := TWorkspaceActions.Create;
  GActions.Reference := NyxPrimaryWorkspace;
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(GHost);
  GRenderer := TNyxBrowserRenderer.Create;
  {$else}
  GHost := TForm.Create(nil);
  GHost.SetBounds(0, 0, 800, 700);
  GRenderer := TNyxLCLRenderer.Create;
  {$endif}
  GRenderer.OnEvent := GActions.Handle;
  GRenderer.Render(GDocument, LRoot, GHost);
  {$ifndef PAS2JS}
  GHost.Show;
  Application.ProcessMessages;
  {$endif}
  Check(Pos(TNyxText('Moon workshop 🌙漢字'), Text('studio-agent-workspace-label-' + LFirst.ID)) > 0,
    'Actual target label retains supplementary Unicode project intent');
  Check(Pos('2 agent sessions', Text('studio-agent-workspace-revision-' + LFirst.ID)) > 0,
    'Actual target presents both connections in one project');
  Check(Text('studio-agent-workspace-connection-' + LFirst.ID + '-0') <>
    Text('studio-agent-workspace-connection-' + LFirst.ID + '-1'),
    'Identical friendly actor names retain visibly distinct public session identities');
  Check(Pos('editing this project', Text('studio-agent-workspace-current-primary')) > 0,
    'Primary context is explicitly marked as the current editor');
  Check(Pos('Unsaved work', Text('studio-agent-workspace-close-explanation')) > 0,
    'Actual target warning describes discarded project content before confirmation');
  LBefore := GProjects.Observe.ToJSON;
  Click('studio-agent-workspace-jump-' + LFirst.ID);
  Check((GActions.Count = 1) and (GActions.Reference.ID = LFirst.ID),
    'Actual target jump emits the exact typed project reference');
  Click('studio-agent-workspace-close-' + LSecond.ID);
  Check((GActions.Count = 2) and (GActions.Reference.ID = LSecond.ID),
    'Actual target close request retains its own reference, independently of list labels');
  Click('action-workspace-close-cancel');
  Check(GActions.LastAction = 'action-workspace-close-cancel',
    'Actual target provides a distinct Keep project action');
  Click('action-workspace-close-confirm');
  Check(GActions.LastAction = 'action-workspace-close-confirm',
    'Actual target emits confirmation separately from requesting the warning');
  Check(GProjects.Observe.ToJSON = LBefore,
    'View actions alone cannot mutate or close any authoritative session');
end;

begin
  try
    try
      Run;
    finally
      Release;
    end;
    WriteLn('PASS ', GChecks, ' actual workspace view checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-workspace-view', 'passed');
    document.body.setAttribute('data-nyx-workspace-view-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-workspace-view', 'failed');
      document.body.setAttribute('data-nyx-workspace-view-error', LException.Message);
      {$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
