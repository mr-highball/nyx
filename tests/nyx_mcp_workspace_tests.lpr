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
program nyx_mcp_workspace_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.studio.builds, base64, fphttpclient, nyx.text, nyx.data, nyx.model,
  nyx.codec, nyx.codegen, nyx.studio.projects,
  nyx.studio.session, nyx.studio.agents, nyx.studio.workspaces,
  nyx.test.mcp.client, nyx.test.browser.host;

type
  { Only operator typing/navigation uses the physical browser. Project creation,
    composition, inspection and history use real semantic MCP at exact revisions. }
  TProjectBrowser = class(TNyxBrowserHost)
  public
    procedure Click(const AID: TNyxText; AHoldForObservation: Boolean = False);
    function Value(const ASelector: TNyxText): TNyxText;
    procedure WaitText(const ASelector, AText: TNyxText);
    procedure AppendDraft(const AText: TNyxText);
    procedure Phone;
    procedure ResizeSource(const AKey: TNyxText; AKeyCode: Integer);
    procedure CheckPresentationHealth;
    procedure Capture;
  end;

var
  GChecks: Integer;
  GClient: TNyxMCPTestClient;
  GOther: TNyxMCPTestClient;
  GBrowser: TProjectBrowser;
  GBase: TNyxText;
  GDirectory: TNyxText;
  GToken: TNyxText;
  GBaseline: TNyxDataValue;

procedure Save(const APath, AText: TNyxText);
var
  LFile: TFileStream;
begin
  ForceDirectories(ExtractFileDir(GDirectory + APath));
  LFile := TFileStream.Create(GDirectory + APath, fmCreate);
  try

    if AText <> '' then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
end;

function Load(const APath: TNyxText): TNyxText;
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if LFile.Size > 4096 then
    begin
      raise Exception.Create('Owned fixture project references exceed their bounded size');
    end;
    SetLength(Result, LFile.Size);
    SetCodePage(RawByteString(Result), CP_UTF8, False);

    if Result <> '' then
    begin
      LFile.ReadBuffer(Result[1], Length(Result));
    end;
  finally
    LFile.Free;
  end;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Tool(AClient: TNyxMCPTestClient; const AName: TNyxText;
  const AArguments: TNyxDataValue): TNyxDataValue; forward;

procedure TProjectBrowser.Click(const AID: TNyxText; AHoldForObservation: Boolean);
var
  LNode: Integer;
  LQuad: TNyxDataValue;
  LX: Double;
  LY: Double;
  LStarted: QWord;
  LPrepared: Boolean;
  LIdentity: Integer;
begin
  { Agent observations may replace shell controls between read-only CDP calls.
    Re-resolve only preparation; once a physical press is sent it is never
    replayed, since a project navigation may already have been admitted. }
  LStarted := GetTickCount64;
  repeat
    LPrepared := False;
    try
      LNode := Node('[data-node="' + AID + '"]');
      Command('DOM.scrollIntoViewIfNeeded', NyxObject([NyxField('nodeId', NyxData(LNode))]));
      LQuad := Command('DOM.getBoxModel', NyxObject([
        NyxField('nodeId', NyxData(LNode))])).Field('model').Field('content');
      LPrepared := True;
    except
      on LException: Exception do
      begin

        if (Pos('Could not find node', LException.Message) = 0) and
          (Pos('Cannot find node', LException.Message) = 0) then
        begin
          raise;
        end;
      end;
    end;

    if not LPrepared then
    begin

      if GetTickCount64 - LStarted > 5000 then
      begin
        raise Exception.Create('Navigation control remained unavailable: ' + AID);
      end;
      Sleep(20);
    end;
  until LPrepared;
  LX := (LQuad.Item(0).AsNumber + LQuad.Item(2).AsNumber) / 2;
  LY := (LQuad.Item(1).AsNumber + LQuad.Item(5).AsNumber) / 2;
  Command('Input.dispatchMouseEvent', NyxObject([NyxField('type', NyxData('mousePressed')),
    NyxField('x', NyxData(LX)), NyxField('y', NyxData(LY)),
    NyxField('button', NyxData('left')), NyxField('clickCount', NyxData(1))]));

  if AHoldForObservation then
  begin
    LIdentity := Command('DOM.describeNode', NyxObject([
      NyxField('nodeId', NyxData(Node('[data-node="' + AID + '"]')))]))
      .Field('node').Field('backendNodeId').AsInteger;
    Tool(GClient, 'nyx_session', NyxObject([]));
    LStarted := GetTickCount64;
    repeat
      Pump;
      Sleep(20);
    until GetTickCount64 - LStarted >= 1500;
    Check(Command('DOM.describeNode', NyxObject([
      NyxField('nodeId', NyxData(Node('[data-node="' + AID + '"]')))]))
        .Field('node').Field('backendNodeId').AsInteger = LIdentity,
      'Observed agent activity retains the actual pressed navigation button through release');
  end;
  Command('Input.dispatchMouseEvent', NyxObject([NyxField('type', NyxData('mouseReleased')),
    NyxField('x', NyxData(LX)), NyxField('y', NyxData(LY)),
    NyxField('button', NyxData('left')), NyxField('clickCount', NyxData(1))]));
end;

function TProjectBrowser.Value(const ASelector: TNyxText): TNyxText;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    try
      Exit(Command('Accessibility.getPartialAXTree', NyxObject([
        NyxField('nodeId', NyxData(Node(ASelector))),
        NyxField('fetchRelatives', NyxData(False))])).Field('nodes').Item(0)
          .Field('value').Field('value').AsText);
    except
      on LException: Exception do
      begin

        if ((Pos('Could not find node', LException.Message) = 0) and
          (Pos('Cannot find node', LException.Message) = 0)) or
          (GetTickCount64 - LStarted > 5000) then
        begin
          raise;
        end;
      end;
    end;
    Sleep(20);
  until False;
end;

procedure TProjectBrowser.WaitText(const ASelector, AText: TNyxText);
var
  LStarted: QWord;
  LReady: Boolean;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;
    LReady := False;
    try
      LReady := Pos(AText, Command('DOM.getOuterHTML', NyxObject([
        NyxField('nodeId', NyxData(Node(ASelector)))])).Field('outerHTML').AsText) > 0;
    except
      on Exception do
      begin
        { Navigation replaces DOM inspection handles; resolve the actual new
          control on the next bounded observation rather than injecting a script. }
      end;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Project editor did not reach expected control state: ' + ASelector);
    end;
    Sleep(20);
  until LReady;
  Check(True, 'Full editor reaches ' + AText);
end;

procedure TProjectBrowser.AppendDraft(const AText: TNyxText);
begin
  Command('DOM.focus', NyxObject([NyxField('nodeId', NyxData(
    Node('textarea[data-node="studio-code"]')))]));
  Command('Input.dispatchKeyEvent', NyxObject([NyxField('type', NyxData('keyDown')),
    NyxField('key', NyxData('End')), NyxField('code', NyxData('End')),
    NyxField('windowsVirtualKeyCode', NyxData(35)), NyxField('modifiers', NyxData(2))]));
  Command('Input.dispatchKeyEvent', NyxObject([NyxField('type', NyxData('keyUp')),
    NyxField('key', NyxData('End')), NyxField('code', NyxData('End')),
    NyxField('windowsVirtualKeyCode', NyxData(35)), NyxField('modifiers', NyxData(2))]));
  Command('Input.insertText', NyxObject([NyxField('text', NyxData(AText))]));
end;

procedure TProjectBrowser.Capture;
begin
  Save('editor.html', Command('DOM.getOuterHTML', NyxObject([
    NyxField('nodeId', NyxData(Node('body')))])).Field('outerHTML').AsText);
  Save('preview.png', DecodeStringBase64(Command('Page.captureScreenshot', NyxObject([
    NyxField('format', NyxData('png')), NyxField('captureBeyondViewport', NyxData(False))]))
      .Field('data').AsText));
end;

procedure TProjectBrowser.Phone;
var
  LMetrics: TNyxDataValue;
begin
  Command('Emulation.setDeviceMetricsOverride', NyxObject([
    NyxField('width', NyxData(390)), NyxField('height', NyxData(844)),
    NyxField('deviceScaleFactor', NyxData(1)), NyxField('mobile', NyxData(False))]));
  LMetrics := Command('Page.getLayoutMetrics', NyxObject([]));
  Check(LMetrics.Field('cssLayoutViewport').Field('clientWidth').AsInteger = 390,
    'Ordinary Studio uses an actual 390 CSS-pixel viewport');
end;

procedure TProjectBrowser.CheckPresentationHealth;
var
  LBody: TNyxText;
begin
  LBody := Command('DOM.getOuterHTML', NyxObject([
    NyxField('nodeId', NyxData(Node('body')))])).Field('outerHTML').AsText;
  Check((Pos('Unsupported editor presentation packet', LBody) = 0) and
    (Pos('Browser component is not mounted', LBody) = 0) and
    (Pos('Editor preferences need review', LBody) = 0),
    'Post-restoration activity and warning actions report no presentation error');
end;

procedure TProjectBrowser.ResizeSource(const AKey: TNyxText; AKeyCode: Integer);
begin
  Command('DOM.focus', NyxObject([NyxField('nodeId', NyxData(
    Node('[data-node="studio-split"] .nyx-split-divider')))]));
  Command('Input.dispatchKeyEvent', NyxObject([NyxField('type', NyxData('keyDown')),
    NyxField('key', NyxData(AKey)), NyxField('code', NyxData(AKey)),
    NyxField('windowsVirtualKeyCode', NyxData(AKeyCode))]));
  Command('Input.dispatchKeyEvent', NyxObject([NyxField('type', NyxData('keyUp')),
    NyxField('key', NyxData(AKey)), NyxField('code', NyxData(AKey)),
    NyxField('windowsVirtualKeyCode', NyxData(AKeyCode))]));
end;

function Tool(AClient: TNyxMCPTestClient; const AName: TNyxText;
  const AArguments: TNyxDataValue): TNyxDataValue;
begin
  Result := AClient.Tool(AName, AArguments);

  if Result.Field('isError').AsBoolean then
  begin
    raise Exception.Create('Semantic project request refused: ' + Result.ToJSON);
  end;
  Result := Result.Field('structuredContent');
end;

function Observe(const AWorkspace: TNyxWorkspaceRef): TNyxDataValue;
begin
  Result := NyxTestEditorExchange(GBase, '/api/agents', GToken,
    NyxWithWorkspace(NyxObject([NyxField('op', NyxData('observe')),
      NyxField('after', NyxData(0))]), AWorkspace));
end;

procedure EqualFrame(const ABefore, AAfter: TNyxDataValue);
var
  LIndex: Integer;
const
  CKeys: array[0..8] of TNyxText = ('revision', 'title', 'selection', 'view',
    'pages', 'components', 'pendingDraft', 'canUndo', 'canRedo');
begin
  Check(ABefore.Field('project').AsText = AAfter.Field('project').AsText,
    'Switching retains exact accepted/draft/base bytes');
  for LIndex := 0 to High(CKeys) do
  begin
    Check(ABefore.Field('session').Field(CKeys[LIndex]).ToJSON =
      AAfter.Field('session').Field(CKeys[LIndex]).ToJSON,
      'Switching retains independent ' + CKeys[LIndex]);
  end;
end;

function CreateProject(const AOperation, ALabel: TNyxText): TNyxWorkspaceRef;
begin
  Result := NyxWorkspace(Tool(GClient, 'nyx_workspaces', NyxObject([
    NyxField('mode', NyxData('create')),
    NyxField('expectedRevision', GBaseline.Field('session').Field('revision')),
    NyxField('operationId', NyxData(AOperation)), NyxField('label', NyxData(ALabel)),
    NyxField('base', NyxData('empty'))])).Field('workspace').AsText);
end;

function JumpID(const AWorkspace: TNyxWorkspaceRef): TNyxText;
var
  LItems: TNyxDataValue;
  LIndex: Integer;
begin
  { Open projects intentionally survive previous failed runs and disconnects.
    Resolve the exact semantic handle; neither a friendly label nor a fixed
    list position identifies the project this journey owns. }
  LItems := Tool(GClient, 'nyx_workspaces', NyxObject([
    NyxField('mode', NyxData('list'))])).Field('items');
  for LIndex := 0 to LItems.Count - 1 do
  begin

    if LItems.Item(LIndex).Field('workspace').AsText = AWorkspace.ID then
    begin
      if AWorkspace.ID = '' then
      begin
        Exit('studio-agent-workspace-jump-primary');
      end;
      Exit('studio-agent-workspace-jump-' + AWorkspace.ID);
    end;
  end;
  raise Exception.Create('Exact owned project has no semantic list entry');
end;

function WorkspaceItem(const AItems: TNyxDataValue;
  const AWorkspace: TNyxWorkspaceRef): TNyxDataValue;
var
  LIndex: Integer;
begin
  for LIndex := 0 to AItems.Count - 1 do
  begin

    if AItems.Item(LIndex).Field('workspace').AsText = AWorkspace.ID then
    begin
      Exit(AItems.Item(LIndex));
    end;
  end;
  raise Exception.Create('Exact project is missing from observer metadata');
end;

procedure Compose(const AWorkspace: TNyxWorkspaceRef; const ATitle, ARoot: TNyxText);
var
  LState: TNyxDataValue;
begin
  LState := Tool(GClient, 'nyx_session', NyxWithWorkspace(NyxObject([]), AWorkspace));
  LState := Tool(GClient, 'nyx_transaction', NyxWithWorkspace(NyxObject([
    NyxField('expectedRevision', LState.Field('revision')),
    NyxField('operationId', NyxData('compose-project')),
    NyxField('operations', NyxArray([
      NyxObject([NyxField('op', NyxData('title')), NyxField('value', NyxData(ATitle))]),
      NyxObject([NyxField('op', NyxData('create')), NyxField('root', NyxData('page')),
        NyxField('kind', NyxData('column')), NyxField('id', NyxData(ARoot))]),
      NyxObject([NyxField('op', NyxData('create')), NyxField('parent', NyxData(ARoot)),
        NyxField('kind', NyxData('heading')), NyxField('id', NyxData(ARoot + '-title')),
        NyxField('properties', NyxObject([NyxField('text', NyxData(ATitle))]))])]))]), AWorkspace));
  Tool(GClient, 'nyx_select', NyxWithWorkspace(NyxObject([
    NyxField('expectedRevision', LState.Field('revision')), NyxField('operationId', NyxData('activate-project')),
    NyxField('id', NyxData(ARoot)), NyxField('activate', NyxData(True))]), AWorkspace));
end;

procedure ResetOwnedProject(const AWorkspace: TNyxWorkspaceRef);
var
  LBefore: TNyxDataValue;
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
begin
  { Explicit optional reuse is confined to this owned fixture stage and exact
    supplied handles. Keep their previous frames as failed-run evidence. This
    never evicts a registry entry or disguises an automatic server reset. }
  LBefore := Observe(AWorkspace);
  Save('before-reuse-' + AWorkspace.ID + '.json', LBefore.ToJSON);
  LDocument := TNyxDocument.Create;
  try
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
  finally
    LDocument.Free;
  end;
  NyxTestEditorExchange(GBase, '/api/agents', GToken, NyxWithWorkspace(NyxObject([
    NyxField('op', NyxData('commit')),
    NyxField('expectedRevision', LBefore.Field('session').Field('revision')),
    NyxField('project', NyxData(EncodeNyxProject(LPair))),
    NyxField('selection', NyxData('')), NyxField('view', NyxData(''))]), AWorkspace));
end;

function ExportSource(const AWorkspace: TNyxWorkspaceRef): TNyxText;
var
  LLines: TNyxStrings;
  LValue: TNyxDataValue;
  LLine: Integer;
  LIndex: Integer;
  LRevision: Integer;
begin
  LRevision := Tool(GClient, 'nyx_session', NyxWithWorkspace(NyxObject([]), AWorkspace))
    .Field('revision').AsInteger;
  LLines := TNyxStrings.Create;
  try
    LLine := 1;
    repeat
      LValue := Tool(GClient, 'nyx_source', NyxWithWorkspace(NyxObject([
        NyxField('line', NyxData(LLine)), NyxField('count', NyxData(80))]), AWorkspace));
      Check(LValue.Field('revision').AsInteger = LRevision,
        'Bounded project source export remains at one exact revision');
      for LIndex := 0 to LValue.Field('lines').Count - 1 do
      begin
        LLines.Add(LValue.Field('lines').Item(LIndex).AsText);
      end;
      Inc(LLine, LValue.Field('lines').Count);
    until LLine > LValue.Field('totalLines').AsInteger;
    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

function SubmitBuild(const AWorkspace: TNyxWorkspaceRef;
  const ATarget, AOutput: TNyxText): TNyxText;
var
  LArguments: TNyxDataValue;
  LRevision: Integer;
begin
  LRevision := Tool(GClient, 'nyx_session', NyxWithWorkspace(NyxObject([]), AWorkspace))
    .Field('revision').AsInteger;
  LArguments := NyxWithWorkspace(NyxObject([NyxField('mode', NyxData('request')),
    NyxField('expectedRevision', NyxData(LRevision)),
    NyxField('operationId', NyxData('project-' + ATarget)),
    NyxField('target', NyxData(ATarget)), NyxField('scope', NyxData('application')),
    NyxField('outputID', NyxData(AOutput))]), AWorkspace);
  Result := Tool(GClient, 'nyx_build', LArguments).Field('job').AsText;
  Check(Tool(GClient, 'nyx_build', LArguments).Field('job').AsText = Result,
    'Exact project build retry retains its immutable receipt');
end;

procedure FinishBuild(const AWorkspace, AOtherWorkspace: TNyxWorkspaceRef;
  const AJob, ATarget, ASource: TNyxText);
var
  LStarted: QWord;
  LStatus: TNyxDataValue;
  LHTTP: TFPHTTPClient;
  LStream: TMemoryStream;
  LCompiled: TNyxText;
begin
  LStatus := GClient.Tool('nyx_build', NyxWithWorkspace(NyxObject([
    NyxField('mode', NyxData('status')), NyxField('job', NyxData(AJob))]), AOtherWorkspace));
  Check(LStatus.Field('isError').AsBoolean, 'Another project cannot read a job by handle alone');
  LStarted := GetTickCount64;
  repeat
    LStatus := Tool(GClient, 'nyx_build', NyxWithWorkspace(NyxObject([
      NyxField('mode', NyxData('status')), NyxField('job', NyxData(AJob)),
      NyxField('severity', NyxData('error')), NyxField('limit', NyxData(3))]), AWorkspace));

    if GetTickCount64 - LStarted > 90000 then
    begin
      raise Exception.Create('Project compiler exceeded its qualification budget');
    end;
    Sleep(100);
  until NyxBuildJobTerminal(ParseNyxBuildJobState(LStatus.Field('state').AsText));
  Save('build-' + ATarget + '.json', LStatus.ToJSON);
  Check((LStatus.Field('state').AsText = 'succeeded') and
    LStatus.Field('currentSource').AsBoolean and
    (LStatus.Field('workspace').AsText = AWorkspace.ID),
    'Actual compiler completes in the immutable project while another editor is observed');
  LHTTP := TFPHTTPClient.Create(nil);
  LStream := TMemoryStream.Create;
  try
    LHTTP.IOTimeout := 10000;
    LHTTP.HTTPMethod('GET', GBase + '/' + LStatus.Field('compiledSource').AsText, LStream, [200]);
    SetLength(LCompiled, LStream.Size);
    SetCodePage(RawByteString(LCompiled), CP_UTF8, False);
    LStream.Position := 0;

    if LStream.Size > 0 then
    begin
      LStream.ReadBuffer(LCompiled[1], LStream.Size);
    end;
    Check(LCompiled = ASource, 'Actual ' + ATarget + ' compiler receives the exact project export');
  finally
    LStream.Free;
    LHTTP.Free;
  end;
end;

procedure ScopeRefusals(const AWorkspace: TNyxWorkspaceRef);
var
  LBefore: TNyxDataValue;
  LReply: TNyxDataValue;
  LRevision: Integer;
begin
  LBefore := Observe(AWorkspace);
  LRevision := LBefore.Field('session').Field('revision').AsInteger;
  LReply := GClient.Tool('nyx_session', NyxObject([
    NyxField('workspace', NyxData('another-service.project-1'))]));
  Check(LReply.Field('isError').AsBoolean and
    not NyxAgentHas(LReply.Field('structuredContent'), 'revision'),
    'Foreign project refuses without exposing a substitute primary revision');
  LReply := GClient.Tool('nyx_session', NyxObject([
    NyxField('workspace', NyxData(AWorkspace.ID)), NyxField('review', NyxData('review-1'))]));
  Check(LReply.Field('isError').AsBoolean, 'Conflicting project/review routing refuses before any context read');
  NyxTestEditorExchange(GBase, '/api/agents', GToken,
    NyxObject([NyxField('op', NyxData('configure')), NyxField('permission', NyxData('readOnly'))]));
  try
    Check(Tool(GClient, 'nyx_session', NyxWithWorkspace(NyxObject([]), AWorkspace))
      .Field('permission').AsText = 'readOnly', 'Global operator reduction immediately reaches another project');
    LReply := GClient.Tool('nyx_transaction', NyxWithWorkspace(NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('readonly-project-refusal')),
      NyxField('operations', NyxArray([NyxObject([
        NyxField('op', NyxData('title')), NyxField('value', NyxData('Must refuse'))])]))]), AWorkspace));
    Check(LReply.Field('isError').AsBoolean, 'Read-only projects refuse semantic mutations');
    NyxTestEditorExchange(GBase, '/api/agents', GToken,
      NyxObject([NyxField('op', NyxData('configure')), NyxField('permission', NyxData('disabled'))]));
    Check(GClient.Tool('nyx_session', NyxWithWorkspace(NyxObject([]), AWorkspace))
      .Field('isError').AsBoolean, 'Disabled permission refuses scoped agent access');
  finally
    NyxTestEditorExchange(GBase, '/api/agents', GToken,
      NyxObject([NyxField('op', NyxData('configure')), NyxField('permission', NyxData('edit'))]));
  end;
  EqualFrame(LBefore, Observe(AWorkspace));
  EqualFrame(GBaseline, Observe(NyxPrimaryWorkspace));
end;

procedure HistoryAndPreview(const AWorkspace: TNyxWorkspaceRef;
  const AJob: TNyxText; var AFrame: TNyxDataValue);
var
  LState: TNyxDataValue;
  LReply: TNyxDataValue;
  LBefore: TNyxText;
begin
  LBefore := AFrame.Field('project').AsText;
  LState := Tool(GClient, 'nyx_history', NyxWithWorkspace(NyxObject([
    NyxField('expectedRevision', AFrame.Field('session').Field('revision')),
    NyxField('operationId', NyxData('independent-undo')),
    NyxField('direction', NyxData('undo'))]), AWorkspace));
  Check(Observe(AWorkspace).Field('project').AsText <> LBefore,
    'Project Undo operates on its own real paired history');
  Check(not Tool(GClient, 'nyx_build', NyxWithWorkspace(NyxObject([
    NyxField('mode', NyxData('status')), NyxField('job', NyxData(AJob))]), AWorkspace))
      .Field('currentSource').AsBoolean, 'Changed project pair makes its prior diagnostics stale');
  Tool(GClient, 'nyx_history', NyxWithWorkspace(NyxObject([
    NyxField('expectedRevision', LState.Field('revision')),
    NyxField('operationId', NyxData('independent-redo')),
    NyxField('direction', NyxData('redo'))]), AWorkspace));
  AFrame := Observe(AWorkspace);
  Check(AFrame.Field('project').AsText = LBefore, 'Project Redo restores its exact accepted pair');
  Check(Tool(GClient, 'nyx_build', NyxWithWorkspace(NyxObject([
    NyxField('mode', NyxData('status')), NyxField('job', NyxData(AJob))]), AWorkspace))
      .Field('currentSource').AsBoolean, 'Exact paired Redo restores source-qualified diagnostic currentness');
  LReply := GClient.Tool('nyx_preview', NyxWithWorkspace(NyxObject([
    NyxField('expectedRevision', AFrame.Field('session').Field('revision')),
    NyxField('view', NyxData('second-page')), NyxField('width', NyxData(390)),
    NyxField('height', NyxData(640)), NyxField('capture', NyxData(True))]), AWorkspace));
  Check(not LReply.Field('isError').AsBoolean and (LReply.Field('content').Count = 3) and
    (LReply.Field('structuredContent').Field('workspace').AsText = AWorkspace.ID),
    'Selective real MCP rendering retains the explicit project and supplies its PNG');
  Save('semantic-project-preview.png', DecodeStringBase64(LReply.Field('content').Item(2).Field('data').AsText));
end;

procedure WaitDraft(const AWorkspace: TNyxWorkspaceRef; const AText: TNyxText);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat

    if Pos(AText, DecodeNyxProject(Observe(AWorkspace).Field('project').AsText).Draft) > 0 then
    begin
      Check(True, 'Ordinary editor draft is acknowledged in its exact project');
      Exit;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Project draft was not acknowledged');
    end;
    Sleep(50);
  until False;
end;

procedure Run;
var
  LStudio: TNyxStudioSession;
  LConnect: TNyxDataValue;
  LFirst: TNyxWorkspaceRef;
  LSecond: TNyxWorkspaceRef;
  LFirstFrame: TNyxDataValue;
  LSecondFrame: TNyxDataValue;
  LPrimaryDraft: TNyxText;
  LProjectDraft: TNyxText;
  LStarted: QWord;
  LReply: TNyxDataValue;
  LReferences: TNyxDataValue;
  LOutput: TNyxText;
  LSource: TNyxText;
  LBrowserJob: TNyxText;
  LNativeJob: TNyxText;
const
  CPrimaryDraft: TNyxText = #10 + '// Primary project stays here 🌙';
  CProjectDraft: TNyxText = #10 + '// Project A keeps this independent draft 漢字';
  CCode: TNyxText = 'textarea[data-node="studio-code"]';
begin
  GBase := ParamStr(1);
  GDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));

  if (Pos('/build/project-workspaces/operator-stage/.codex/config.toml',
    StringReplace(ExpandFileName(ParamStr(2)), '\', '/', [rfReplaceAll])) = 0) and
    (Pos('/build/review-workspaces/projects/stage/.codex/config.toml',
    StringReplace(ExpandFileName(ParamStr(2)), '\', '/', [rfReplaceAll])) = 0) then
  begin
    raise Exception.Create('Use the owned project fixture configuration, never a live user service');
  end;
  LStudio := TNyxStudioSession.Create;
  try
    LConnect := NyxTestEditorExchange(GBase, '/api/agents/connect', '', NyxObject([
      NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(LStudio.ProjectSnapshot))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GToken := LConnect.Field('token').AsText;
    { Explicit reset is confined to this independently owned fixture stage.
      Previous private runs keep their own evidence rather than erasing it. }
    NyxTestEditorExchange(GBase, '/api/agents', GToken, NyxObject([
      NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', LConnect.Field('state').Field('session').Field('revision')),
      NyxField('project', NyxData(EncodeNyxProject(LStudio.ProjectSnapshot))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
  finally
    LStudio.Free;
  end;
  GClient := TNyxMCPTestClient.Create(ParamStr(2), 'Scooty project fixture');
  GOther := TNyxMCPTestClient.Create(ParamStr(2), 'Scooty project fixture');
  Check(GClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools').Count = 17,
    'Initialized candidate exposes seventeen focused semantic tools');
  WriteLn('Journey: initial ordinary editor');
  Flush(Output);
  GBrowser := TProjectBrowser.Create(GBase + '/', GDirectory + 'full-editor');

  if (ParamCount > 4) and (ParamStr(5) = '390') then
  begin
    GBrowser.Phone;
  end;
  GBrowser.Click('action-agents');
  GBrowser.WaitText('[data-node="studio-agents-status"]', 'Agents edit');
  GBrowser.Click('action-code');
  GBrowser.ResizeSource('Home', 36);
  GBrowser.WaitText('[data-node="studio-split"] .nyx-split-divider', 'aria-valuenow="10"');
  GBrowser.AppendDraft(CPrimaryDraft);
  WaitDraft(NyxPrimaryWorkspace, CPrimaryDraft);
  LPrimaryDraft := GBrowser.Value(CCode);
  GBaseline := Observe(NyxPrimaryWorkspace);
  Save('primary-baseline.json', GBaseline.ToJSON);
  WriteLn('Journey: semantic project composition');
  Flush(Output);
  if ParamCount > 3 then
  begin

    if Pos('/build/project-workspaces/',
      StringReplace(ExpandFileName(ParamStr(4)), '\', '/', [rfReplaceAll])) = 0 then
    begin
      raise Exception.Create('Reuse references must be an explicit owned fixture artifact');
    end;
    LReferences := TNyxDataValue.ParseJSON(Load(ParamStr(4)));
    LFirst := NyxWorkspace(LReferences.Field('first').AsText);
    LSecond := NyxWorkspace(LReferences.Field('second').AsText);
    ResetOwnedProject(LFirst);
    ResetOwnedProject(LSecond);
  end
  else
  begin
    LFirst := CreateProject('project-a', 'Project A');
    LSecond := CreateProject('project-b', 'Project B');
  end;
  Save('project-references.json', NyxObject([NyxField('first', NyxData(LFirst.ID)),
    NyxField('second', NyxData(LSecond.ID))]).ToJSON);
  Compose(LFirst, 'First project 🌙', 'first-page');
  Compose(LSecond, 'Second project 漢字', 'second-page');
  Tool(GOther, 'nyx_session', NyxWithWorkspace(NyxObject([]), LFirst));
  GBrowser.WaitText('[data-node="' + StringReplace(JumpID(LFirst), '-jump-', '-label-', []) + '"]', 'Project A');
  WriteLn('Journey: jump into Project A');
  Flush(Output);
  GBrowser.Click(JumpID(LFirst), True);
  GBrowser.WaitText('[data-node="studio-subtitle"]', 'First project');
  GBrowser.WaitText('[data-node="studio-agents-status"]', 'Agents edit');

  if (ParamCount > 4) and (ParamStr(5) = '390') then
  begin
    GBrowser.Click('action-panel-project');
  end;
  Check(Pos('First project', GBrowser.Value('[data-node="project-title"] input')) > 0,
    'Jump mounts the full project editor, including its ordinary title control');

  if (ParamCount > 4) and (ParamStr(5) = '390') then
  begin
    GBrowser.Click('action-panel-design');
  end;
  GBrowser.Click('action-code');
  GBrowser.ResizeSource('End', 35);
  GBrowser.WaitText('[data-node="studio-split"] .nyx-split-divider', 'aria-valuenow="90"');
  Check(GBrowser.Value(CCode) <> LPrimaryDraft,
    'Project A has its own live public Pascal control/source');
  GBrowser.AppendDraft(CProjectDraft);
  WaitDraft(LFirst, CProjectDraft);
  LProjectDraft := GBrowser.Value(CCode);
  LFirstFrame := Observe(LFirst);
  LSecondFrame := Observe(LSecond);
  Save('project-a-baseline.json', LFirstFrame.ToJSON);
  Save('project-b-baseline.json', LSecondFrame.ToJSON);
  EqualFrame(GBaseline, Observe(NyxPrimaryWorkspace));
  WriteLn('Journey: return to primary project');
  Flush(Output);
  GBrowser.Click(JumpID(NyxPrimaryWorkspace));
  GBrowser.WaitText('[data-node="studio-subtitle"]', 'Untitled project');
  GBrowser.WaitText('[data-node="studio-agents-status"]', 'Agents edit');
  GBrowser.WaitText('[data-node="studio-split"] .nyx-split-divider', 'aria-valuenow="10"');
  Check(GBrowser.Value(CCode) = LPrimaryDraft,
    'Returning restores exact primary pending Unicode source and visible split');
  EqualFrame(GBaseline, Observe(NyxPrimaryWorkspace));
  EqualFrame(LFirstFrame, Observe(LFirst));
  EqualFrame(LSecondFrame, Observe(LSecond));
  WriteLn('Journey: compile exact Project B while observing primary');
  Flush(Output);
  LSource := ExportSource(LSecond);
  Save('source/nyx.generated.view.pas', LSource);
  LOutput := Tool(GClient, 'nyx_build', NyxWithWorkspace(NyxObject([
    NyxField('mode', NyxData('outputs'))]), LSecond)).Field('outputID').AsText;
  LBrowserJob := SubmitBuild(LSecond, 'browser', LOutput);
  LNativeJob := SubmitBuild(LSecond, 'lcl', LOutput);
  WriteLn('Journey: return to Project A');
  Flush(Output);
  GBrowser.Click(JumpID(LFirst));
  GBrowser.WaitText('[data-node="studio-subtitle"]', 'First project');
  GBrowser.WaitText('[data-node="studio-agents-status"]', 'Agents edit');
  GBrowser.WaitText('[data-node="studio-split"] .nyx-split-divider', 'aria-valuenow="90"');
  Check(GBrowser.Value(CCode) = LProjectDraft,
    'Second jump restores Project A independent draft and presentation');
  EqualFrame(LFirstFrame, Observe(LFirst));
  FinishBuild(LSecond, LFirst, LBrowserJob, 'browser', LSource);
  FinishBuild(LSecond, LFirst, LNativeJob, 'lcl', LSource);
  EqualFrame(LFirstFrame, Observe(LFirst));
  EqualFrame(GBaseline, Observe(NyxPrimaryWorkspace));
  EqualFrame(LSecondFrame, Observe(LSecond));
  HistoryAndPreview(LSecond, LBrowserJob, LSecondFrame);
  EqualFrame(LFirstFrame, Observe(LFirst));
  EqualFrame(GBaseline, Observe(NyxPrimaryWorkspace));
  Save('primary-after.json', Observe(NyxPrimaryWorkspace).ToJSON);
  Save('project-a-after.json', Observe(LFirst).ToJSON);
  Save('project-b-after.json', Observe(LSecond).ToJSON);
  ScopeRefusals(LSecond);
  { The staged older backend cannot admit the new operator close route yet.
    Qualify the shared warning/cancel path only; never claim successful closure
    from the presence of controls or its native compilation. }
  GBrowser.Click(StringReplace(JumpID(LSecond), '-jump-', '-close-', []));
  GBrowser.WaitText('[data-node="studio-agent-workspace-close-explanation"]', 'Unsaved work');
  GBrowser.Click('action-workspace-close-cancel');
  EqualFrame(LSecondFrame, Observe(LSecond));
  { Disconnect both authenticated agents while observing their project. The
    projects and full editor must remain; only connection presence disappears. }
  GClient.Close;
  GOther.Close;
  LStarted := GetTickCount64;
  repeat
    LReply := Observe(LFirst);

    if WorkspaceItem(LReply.Field('workspaces'), LFirst).Field('connections').Count = 0 then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Project connections did not retire');
    end;
    Sleep(50);
  until False;
  EqualFrame(LFirstFrame, Observe(LFirst));
  EqualFrame(GBaseline, Observe(NyxPrimaryWorkspace));
  EqualFrame(LSecondFrame, Observe(LSecond));
  Check(GBrowser.Value(CCode) = LProjectDraft,
    'Observing project remains open after every agent disconnects');
  GBrowser.CheckPresentationHealth;
  GBrowser.Capture;
end;

begin
  try
    try
      try
        Run;
      except
        on LException: Exception do
        begin

          if GBrowser <> nil then
          begin
            try
              GBrowser.Capture;
            except
              on Exception do
              begin
                { Retain the original workflow failure if capturing a dying
                  navigation target also refuses. Teardown still runs below. }
              end;
            end;
          end;
          raise;
        end;
      end;
    finally
      try
        GBrowser.Free;
      finally
        try

          if GClient <> nil then
          begin
            GClient.Close;
          end;
        finally
          GClient.Free;
          try

            if GOther <> nil then
            begin
              GOther.Close;
            end;
          finally
            GOther.Free;
          end;
        end;
      end;
    end;
    WriteLn('PASS ', GChecks, ' real concurrent project checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
