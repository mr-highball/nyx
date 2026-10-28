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
program nyx_studio_project_navigation_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.codec,
  nyx.controls, nyx.studio.projects, nyx.studio.agentview,
  nyx.studio.projectnavigation, nyx.studio.workspaces
  {$ifdef PAS2JS}, JS, Web
  {$else}, nyx.studio.mcp, nyx.studio.directories, nyx.studio.outputs,
    nyx.studio.agents{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Shared;
var
  LState: TNyxStudioProjectNavigation;
  LAgents: TNyxStudioAgentView;
  LControls: INyxColumn;
  LPair: TNyxProjectPair;
  LDocument: TNyxDocument;
  LRefused: Boolean;
begin
  LPair := NewNyxStudioProjectPair('A fresh project 😀');
  LDocument := TNyxCodec.Decode(LPair.Design);
  try
    Check(LDocument.Title = TNyxText('A fresh project 😀'), 'Exact Unicode project name');
    Check((LDocument.Count = 1) and (LDocument.Pages[0].ID = 'home'), 'Useful empty home page');
    Check(LDocument.Pages[0].Count = 0, 'No demonstration content or output prerequisite');
  finally
    LDocument.Free;
  end;
  LRefused := False;
  try
    NewNyxStudioProjectPair('   ');
  except
    on ENyxModel do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Blank names refuse before creation');
  LState := Default(TNyxStudioProjectNavigation);
  LAgents := DefaultNyxStudioAgentView;
  LControls := BuildNyxStudioProjectNavigation(LState, LAgents);
  Check(LControls.Node.Find('action-project-new') <> nil, 'New is always discoverable');
  Check(LControls.Node.Find('action-project-browse') <> nil, 'Open is always discoverable');
  Check(LControls.Node.Find('action-project-save-current') <> nil, 'Save is always discoverable');
  Check(LControls.Node.Find('action-project-switch') <> nil, 'Switch is always discoverable');
  Check(RouteNyxStudioProjectNavigation(LState, LControls.Node.Find('action-project-new'), ntClick),
    'New opens its presentation without editing a project');
  LControls := BuildNyxStudioProjectNavigation(LState, LAgents);
  Check(LControls.Node.Find('studio-new-project-unavailable') <> nil,
    'Older/offline hosts explain creation capability');
  LAgents.Connected := True;
  LAgents.CanCreateWorkspace := True;
  LControls := BuildNyxStudioProjectNavigation(LState, LAgents);
  Check(LControls.Node.Find('action-project-create').Prop('enabled') = 'true',
    'Acknowledged operator can create independently of agent permissions');
  LControls.Node.Find('studio-new-project-name').Configure.Value('Another 😀').Done;
  RouteNyxStudioProjectNavigation(LState, LControls.Node.Find('studio-new-project-name'), ntChange);
  Check(LState.Name = TNyxText('Another 😀'), 'Unfinished name survives shell rebuild');
  RouteNyxStudioProjectNavigation(LState, LControls.Node.Find('action-project-switch'), ntClick);
  LAgents.Workspaces := NyxArray([
    NyxObject([NyxField('workspace', NyxData('')), NyxField('label', NyxData('Main')),
      NyxField('session', NyxObject([NyxField('title', NyxData('My work'))]))]),
    NyxObject([NyxField('workspace', NyxData('stable.project-7')), NyxField('label', NyxData('Other')),
      NyxField('session', NyxObject([NyxField('title', NyxData('Another project'))]))])]);
  LControls := BuildNyxStudioProjectNavigation(LState, LAgents);
  Check(LControls.Node.Find('studio-project-current-0') <> nil, 'Current project identified');
  Check(LControls.Node.Find('studio-project-switch-1').Extensions.Value(
    NyxStudioWorkspaceJumpKey).AsText = 'stable.project-7', 'Switch carries exact opaque identity');
  Check(Pos('Other', LControls.Node.Find('studio-project-switch-1').Prop('text')) > 0,
    'Project label is visible without hovering on touch hosts');
end;

{$ifndef PAS2JS}
procedure PrivateLifecycle;
var
  LEngine: TNyxStudioMCP;
  LSeed: TNyxAgentSession;
  LOutputs: TNyxOutputConfiguration;
  LClaim: TNyxDataValue;
  LBefore: TNyxDataValue;
  LReply: TNyxDataValue;
  LAfter: TNyxDataValue;
  LRequest: TNyxDataValue;
  LWorkingPair: TNyxProjectPair;
  LToken: TNyxText;
  LCreated: TNyxWorkspaceRef;
  LRefused: Boolean;
begin

  if (ParamCount <> 1) or DirectoryExists(ParamStr(1)) then
  begin
    raise Exception.Create('Supply a new external qualification runtime directory');
  end;
  LSeed := TNyxAgentSession.Create;
  LOutputs := TNyxOutputConfiguration.Create;
  LEngine := nil;
  try
    LEngine := TNyxStudioMCP.Create(TNyxStudioDirectories.ForRepository(ParamStr(1)),
      19210, 19211, LOutputs.Encode);
    LClaim := LEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', LSeed.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
        .Field('project')), NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    LToken := LClaim.Field('token').AsText;
    LBefore := LEngine.EditorExchange(LToken, NyxObject([NyxField('op', NyxData('observe'))]));
    LWorkingPair := NewNyxStudioProjectPair('Existing work');
    LWorkingPair.Pending := True;
    LWorkingPair.DraftBase := LWorkingPair.Source;
    LWorkingPair.Draft := LWorkingPair.Source + TNyxText(#10'{ unfinished author text 😀 }');
    LBefore := LEngine.EditorExchange(LToken, NyxObject([
      NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', LBefore.Field('session').Field('revision')),
      NyxField('project', NyxData(EncodeNyxProject(LWorkingPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    Check(LBefore.Field('session').Field('pendingDraft').AsBoolean,
      'Lifecycle test starts with a real pending Pascal draft');
    Check(LBefore.Field('session').Field('canUndo').AsBoolean,
      'Lifecycle test starts with independent paired Undo history');
    Check(LBefore.Field('workspaceCreation').AsBoolean, 'Private capability advertised');
    LRequest := NyxObject([NyxField('op', NyxData('create-workspace')),
      NyxField('expectedRevision', LBefore.Field('session').Field('revision')),
      NyxField('name', NyxData('New workspace 😀'))]);
    LRefused := False;
    try
      LEngine.EditorExchange('wrong-capability', LRequest);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Creation requires private operator capability');
    LReply := LEngine.EditorExchange(LToken, LRequest);
    LCreated := NyxWorkspace(LReply.Field('createdWorkspace').AsText);
    Check(LCreated.ID <> '', 'Creation returns independent identity');
    Check(LReply.Field('workspaces').Count = LBefore.Field('workspaces').Count + 1,
      'Creation enters ordinary project roster');
    Check(LReply.Field('project').AsText = LBefore.Field('project').AsText,
      'Creation leaves current paired work exact');
    Check(LReply.Field('session').ToJSON = LBefore.Field('session').ToJSON,
      'Creation retains current revision, selection and history');
    LAfter := LEngine.EditorExchange(LToken, NyxWithWorkspace(
      NyxObject([NyxField('op', NyxData('observe'))]), LCreated));
    Check(LAfter.Field('session').Field('title').AsText = TNyxText('New workspace 😀'), 'New project is queryable');
    Check(not DecodeNyxProject(LAfter.Field('project').AsText).Pending, 'Fresh project has no pending source draft');
    LRefused := False;
    try
      LEngine.EditorExchange(LToken, NyxObject([NyxField('op', NyxData('create-workspace')),
        NyxField('expectedRevision', NyxData(-1)), NyxField('name', NyxData('Stale'))]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Stale creation is refused');
    LAfter := LEngine.EditorExchange(LToken, NyxObject([NyxField('op', NyxData('observe'))]));
    Check(LAfter.Field('workspaces').Count = LReply.Field('workspaces').Count,
      'Refused creation adds no project');
    LEngine.EditorExchange(LToken, NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('disabled'))]));
    LAfter := LEngine.EditorExchange(LToken, LRequest);
    Check(LAfter.Field('workspaces').Count = LReply.Field('workspaces').Count + 1,
      'Disabling agents does not disable human project creation');
  finally
    LEngine.Free;
    LOutputs.Free;
    LSeed.Free;
  end;
end;
{$endif}

begin
  try
    Shared;
    {$ifndef PAS2JS}PrivateLifecycle;{$endif}
    WriteLn('PASS ', GChecks, ' project navigation checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-test-result', 'PASS ' + IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL: ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-test-result', 'FAIL: ' + LException.Message);
      {$else}ExitCode := 1;{$endif}
    end;
  end;
end.
