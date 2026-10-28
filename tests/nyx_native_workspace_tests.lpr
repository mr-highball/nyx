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

program nyx_native_workspace_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Types, Forms, Controls, StdCtrls, Graphics, IntfGraphics, FPWritePNG,
  nyx.text, nyx.data, nyx.model, nyx.codec, nyx.editing, nyx.editing.lcl,
  nyx.render.lcl, nyx.types, nyx.menu, nyx.menu.lcl,
  nyx.studio.lcl, nyx.studio.workspaces, nyx.studio.agentview,
  nyx.studio.projects, nyx.studio.projectstore, nyx.studio.agents, nyx.studio.exchange,
  nyx.studio.exchange.lcl, nyx.test.mcp.client;

type
  { Exposes ordinary widget activation to the native regression. The control
    remains borrowed from Studio and must survive until its callback returns. }
  TControlAccess = class(TControl);
  { Copies a native notification failure before the borrowed application/form
    is retired, so asynchronous failures cannot become a false test pass. }
  TNativeFailureObserver = class
    Error: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  { Counts distinguish a deferred replacement from its canceled predecessor.
    The actual worker must never invoke this borrowed receiver outside the UI. }
  TExchangeObserver = class
    Replies: Integer;
    Ticks: Integer;
    OnUIThread: Boolean;
    Status: Integer;
    procedure Reply(AStatus: Integer; const AText: TNyxText);
    procedure Tick;
  end;

var
  GClient: TNyxMCPTestClient;
  GStudio: TNyxNativeStudio;
  GForm: TForm;
  GObserver: TNativeFailureObserver;
  GChecks: Integer;
  GSerial: Integer;
  GDirectory: TNyxText;
  GIdentity: TNyxText;
  GRoot: TNyxText;
  GReplyID: TNyxText;

procedure TNativeFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.ClassName + ': ' + AException.Message;
end;

procedure TExchangeObserver.Reply(AStatus: Integer; const AText: TNyxText);
begin
  Inc(Replies);
  OnUIThread := GetCurrentThreadID = MainThreadID;
  Status := AStatus;
end;

procedure TExchangeObserver.Tick;
begin
  Inc(Ticks);
  OnUIThread := GetCurrentThreadID = MainThreadID;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if GObserver.Error <> '' then
  begin
    raise Exception.Create('Native workspace notification: ' + GObserver.Error);
  end;

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Operation: TNyxText;
begin
  Inc(GSerial);
  Result := GIdentity + '-' + IntToStr(GSerial);
end;

procedure Save(const AName, AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(GDirectory + AName, fmCreate);
  try

    if AText <> '' then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;

function PrimaryFrame(const ASession: TNyxDataValue): TNyxText;
const
  CFields: array[0..9] of TNyxText = ('revision', 'permission', 'title',
    'selection', 'view', 'pages', 'components', 'pendingDraft', 'canUndo', 'canRedo');
var
  LIndex: Integer;
begin
  Result := '';
  for LIndex := Low(CFields) to High(CFields) do
  begin
    Result := Result + ASession.Field(CFields[LIndex]).ToJSON + #10;
  end;
end;

function Tool(const AName: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
var
  LReply: TNyxDataValue;
begin
  LReply := GClient.Tool(AName, AArguments);

  if LReply.Field('isError').AsBoolean then
  begin
    raise Exception.Create('Semantic native workspace operation refused: ' +
      LReply.Field('content').ToJSON);
  end;
  Result := LReply.Field('structuredContent');
end;

function Scoped(const AWorkspace: TNyxWorkspaceRef; const AName: TNyxText;
  const AArguments: TNyxDataValue): TNyxDataValue;
begin
  Result := Tool(AName, NyxWithWorkspace(AArguments, AWorkspace));
end;

function Session(const AWorkspace: TNyxWorkspaceRef): TNyxDataValue;
begin
  Result := Scoped(AWorkspace, 'nyx_session', NyxObject([]));
end;

function Snapshot(const AWorkspace: TNyxWorkspaceRef): TNyxDataValue;
var
  LClaim: TNyxDataValue;
begin
  { One exact paired snapshot is necessary for baseline preservation. The editor
    response's capability is never returned, logged or saved by this helper. }
  LClaim := NyxTestEditorExchange(ParamStr(1), '/api/agents/connect', '',
    NyxWithWorkspace(NyxObject([NyxField('op', NyxData('claim'))]), AWorkspace));
  Result := LClaim.Field('state').Copy;
end;

function ReadBytes(const APath: TNyxText; AMaximumBytes: Integer = 4096): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if LStream.Size > AMaximumBytes then
    begin
      raise Exception.Create('Explicit test context references exceed their byte budget');
    end;
    SetLength(Result, LStream.Size);

    if Result <> '' then
    begin
      LStream.ReadBuffer(Result[1], Length(Result));
    end;
  finally
    LStream.Free;
  end;
end;

function CreateProject(const ALabel: TNyxText; APrimaryRevision: Integer): TNyxWorkspaceRef;
var
  LReceipt: TNyxDataValue;
begin
  LReceipt := Tool('nyx_workspaces', NyxObject([
    NyxField('mode', NyxData('create')), NyxField('label', NyxData(ALabel)),
    NyxField('base', NyxData('empty')), NyxField('expectedRevision', NyxData(APrimaryRevision)),
    NyxField('operationId', NyxData(Operation))]));
  Result := NyxWorkspace(LReceipt.Field('workspace').AsText);
  Save(ALabel + '-receipt.json', LReceipt.ToJSON);
end;

procedure Compose(const AWorkspace: TNyxWorkspaceRef; const ATitle: TNyxText);
var
  LFrame: TNyxDataValue;
begin
  LFrame := Session(AWorkspace);
  Scoped(AWorkspace, 'nyx_transaction', NyxObject([
    NyxField('expectedRevision', LFrame.Field('revision')),
    NyxField('operationId', NyxData(Operation)),
    NyxField('operations', NyxArray([
      NyxObject([NyxField('op', NyxData('title')), NyxField('value', NyxData(ATitle))]),
      NyxObject([NyxField('op', NyxData('create')), NyxField('root', NyxData('page')),
        NyxField('kind', NyxData('page')), NyxField('id', NyxData(GRoot)),
        NyxField('properties', NyxObject([NyxField('padding', NyxData(24)),
          NyxField('gap', NyxData(12))]))]),
      NyxObject([NyxField('op', NyxData('create')), NyxField('parent', NyxData(GRoot)),
        NyxField('kind', NyxData('heading')), NyxField('id', NyxData(GRoot + '-title')),
        NyxField('properties', NyxObject([NyxField('text', NyxData(ATitle))]))]),
      NyxObject([NyxField('op', NyxData('create')), NyxField('parent', NyxData(GRoot)),
        NyxField('kind', NyxData('memo')), NyxField('id', NyxData(GReplyID)),
        NyxField('properties', NyxObject([NyxField('text', NyxData('Your ideas')),
          NyxField('value', NyxData('Ready for your ideas.')), NyxField('height', NyxData(120))]))])
    ]))]));
  LFrame := Session(AWorkspace);
  Scoped(AWorkspace, 'nyx_select', NyxObject([
    NyxField('expectedRevision', LFrame.Field('revision')), NyxField('id', NyxData(GRoot)),
    NyxField('activate', NyxData(True)), NyxField('operationId', NyxData(Operation))]));
end;

procedure ChangeValue(const AWorkspace: TNyxWorkspaceRef; const AValue: TNyxText);
var
  LFrame: TNyxDataValue;
begin
  LFrame := Session(AWorkspace);
  Scoped(AWorkspace, 'nyx_transaction', NyxObject([
    NyxField('expectedRevision', LFrame.Field('revision')),
    NyxField('operationId', NyxData(Operation)),
    NyxField('operations', NyxArray([
      NyxObject([NyxField('op', NyxData('update')), NyxField('id', NyxData(GReplyID)),
        NyxField('properties', NyxObject([NyxField('value', NyxData(AValue))]))])
    ]))]));
end;

procedure Cleanup(const AWorkspace: TNyxWorkspaceRef; const ABefore: TNyxDataValue);
var
  LFrame: TNyxDataValue;
  LReview: TNyxDataValue;
  LRoots: TNyxDataValue;
  LPair: TNyxProjectPair;
  LDocument: TNyxDocument;
  LTitle: TNyxText;
begin
  LPair := DecodeNyxProject(ABefore.Field('project').AsText);
  LDocument := TNyxCodec.Decode(LPair.Design);
  try
    LTitle := LDocument.Title;
  finally
    LDocument.Free;
  end;
  LFrame := Session(AWorkspace);
  Scoped(AWorkspace, 'nyx_transaction', NyxObject([
    NyxField('expectedRevision', LFrame.Field('revision')),
    NyxField('operationId', NyxData(Operation)),
    NyxField('operations', NyxArray([NyxObject([
      NyxField('op', NyxData('title')), NyxField('value', NyxData(LTitle))])]))]));
  LFrame := Session(AWorkspace);
  LRoots := NyxArray([NyxObject([
    NyxField('root', NyxData('page')), NyxField('id', NyxData(GRoot))])]);
  LReview := Scoped(AWorkspace, 'nyx_roots', NyxObject([
    NyxField('mode', NyxData('review')), NyxField('expectedRevision', LFrame.Field('revision')),
    NyxField('roots', LRoots)]));
  Save(AWorkspace.ID + '-cleanup-review.json', LReview.ToJSON);
  Scoped(AWorkspace, 'nyx_roots', NyxObject([
    NyxField('mode', NyxData('apply')), NyxField('expectedRevision', LFrame.Field('revision')),
    NyxField('roots', LRoots), NyxField('reviewID', LReview.Field('reviewID')),
    NyxField('operationId', NyxData(Operation))]));
  LFrame := Session(AWorkspace);
  Scoped(AWorkspace, 'nyx_select', NyxObject([
    NyxField('expectedRevision', LFrame.Field('revision')),
    NyxField('id', ABefore.Field('session').Field('view')), NyxField('activate', NyxData(True)),
    NyxField('operationId', NyxData(Operation))]));
  LFrame := Session(AWorkspace);
  Scoped(AWorkspace, 'nyx_select', NyxObject([
    NyxField('expectedRevision', LFrame.Field('revision')),
    NyxField('id', ABefore.Field('session').Field('selection')),
    NyxField('operationId', NyxData(Operation))]));
  Check(Snapshot(AWorkspace).Field('project').AsText = ABefore.Field('project').AsText,
    'reviewed cleanup restores the exact original test-project pair');
end;

procedure Pump;
begin
  Application.ProcessMessages;
  Sleep(10);
end;

procedure QualifyTransport(const AWorkspace: TNyxWorkspaceRef);
var
  LExchange: TNyxLCLEditorExchange;
  LObserver: TExchangeObserver;
  LBody: TNyxText;
  LStarted: QWord;
  LRejected: Boolean;
begin
  LObserver := TExchangeObserver.Create;
  LExchange := nil;
  try
    LRejected := False;
    try
      LExchange := TNyxLCLEditorExchange.Create('http://127.0.0.1:8278/remote');
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LExchange = nil), 'native transport rejects a non-origin before network work');
    LExchange := TNyxLCLEditorExchange.Create(ParamStr(1));
    LBody := NyxWithWorkspace(NyxObject([NyxField('op', NyxData('claim'))]), AWorkspace).ToJSON;
    LExchange.Post(True, '', LBody, LObserver.Reply);
    Check(LObserver.Replies = 0, 'native HTTP returns before delivering its UI notification');
    LExchange.CancelRequest;
    LExchange.Post(True, '', LBody, LObserver.Reply);
    LExchange.Schedule(1, LObserver.Tick);
    LExchange.CancelTick;
    LStarted := GetTickCount64;
    repeat
      Pump;

      if GetTickCount64 - LStarted > 60000 then
      begin
        raise Exception.Create('Deferred transport completion exceeded its functional wait');
      end;
    until LObserver.Replies > 0;
    Check((LObserver.Replies = 1) and (LObserver.Status = 200) and LObserver.OnUIThread,
      'only the deferred replacement replies and it runs on the UI thread');
    Check(LObserver.Ticks = 0, 'canceled UI timer has no borrowed receiver notification');
    LExchange.Post(True, '', LBody, LObserver.Reply);
    LExchange.Schedule(1, LObserver.Tick);
    FreeAndNil(LExchange);
    Application.ProcessMessages;
    Check((LObserver.Replies = 1) and (LObserver.Ticks = 0),
      'destroying active transport suppresses request and timer callbacks');
  finally
    LExchange.Free;
    LObserver.Free;
  end;
end;

procedure Capture(const AName: TNyxText);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(GForm.ClientWidth, GForm.ClientHeight);
    GForm.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(GDirectory + AName + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

procedure WaitReady(const AWorkspace: TNyxWorkspaceRef; const AValue: TNyxText = '');
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;

    if GStudio.Agents.Conflict then
    begin
      raise Exception.Create(GStudio.Agents.Status);
    end;

    if GStudio.Agents.Connected and (GStudio.Agents.Workspace.ID = AWorkspace.ID) and
      (GStudio.CanvasView.Root <> nil) and (GStudio.Session.Document.Find(GReplyID) <> nil) and
      (GStudio.Session.ActiveViewID = GRoot) then
    begin

      if (AValue = '') or (GStudio.Session.Document.Find(GReplyID).Prop('value') = AValue) then
      begin
        { One more queue pass mounts the canvas proposed by the protocol callback. }
        Application.ProcessMessages;

        if GStudio.CanvasView.Root.ID = GRoot then
        begin
          Exit;
        end;
      end;
    end;

    if GetTickCount64 - LStarted > 60000 then
    begin
      Save('workspace-wait-status.txt', GStudio.Status);
      Capture('workspace-wait-failed');
      raise Exception.Create('Native workspace observation exceeded its functional wait');
    end;
  until False;
end;

procedure Click(const AID: TNyxText);
var
  LControl: TControl;
  LPaints: Integer;
  LStarted: QWord;
begin
  if GStudio.ShellView.Root.Find(AID) <> nil then
  begin
    LControl := GStudio.ShellView.ControlFor(AID);
  end
  else
  begin
    LControl := GStudio.SourceView.ControlFor(AID);
  end;
  Check(LControl <> nil, 'actual editor action exists: ' + AID);
  LPaints := GStudio.PaintCount;
  TControlAccess(LControl).Click;
  Check(GStudio.PaintCount = LPaints, 'native action retains its widget through callback: ' + AID);
  { Host resize can require another deferred paint after this callback. A
    single ProcessMessages pass does not promise that an optional pane exists. }
  LStarted := GetTickCount64;
  repeat
    Pump;

    if not GStudio.PresentationPending then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise Exception.Create('Native presentation did not settle after ' + AID);
    end;
  until False;
end;

{ Compact Studio intentionally hides desktop command faces. Exercise its real
  popup family and physical action widgets instead of invoking a hidden face. }
procedure PascalAction;
var
  LDirect: TControl;
  LParent: INyxLCLMenu;
  LChild: INyxMenu;
  LNative: INyxLCLMenu;
  LControl: TControl;
  LStarted: QWord;
begin
  LDirect := GStudio.ShellView.ControlFor('action-code');

  if LDirect.IsVisible then
  begin
    Click('action-code');
    Exit;
  end;
  Click('action-actions');
  LParent := GStudio.ActionMenu;
  Check((LParent <> nil) and LParent.IsOpen, 'compact action family is visibly open');
  LControl := LParent.Presentation.Renderer.ControlFor('studio-menu-view');
  Check(LControl.IsVisible, 'View is an actual visible popup action');
  TControlAccess(LControl).Click;
  Pump;
  LChild := LParent.Submenu(NyxPart('view'));
  Check((LChild <> nil) and LChild.IsOpen and Supports(LChild, INyxLCLMenu, LNative),
    'View opens its independent native submenu');
  LControl := LNative.Presentation.Renderer.ControlFor('studio-menu-code');
  Check(LControl.IsVisible, 'Pascal source is an actual visible submenu action');
  TControlAccess(LControl).Click;
  LStarted := GetTickCount64;
  repeat
    Pump;

    if not GStudio.PresentationPending then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise Exception.Create('Compact Pascal navigation did not reach its presentation');
    end;
  until False;
end;

procedure WaitPublication(const AWorkspace: TNyxWorkspaceRef; const AValue: TNyxText;
  APending: Boolean);
var
  LStarted: QWord;
  LFrame: TNyxDataValue;
  LNode: TNyxDataValue;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;
    LFrame := Session(AWorkspace);
    LNode := Scoped(AWorkspace, 'nyx_node', NyxObject([
      NyxField('id', NyxData(GReplyID)), NyxField('keys', NyxArray([NyxData('value')]))]));

    if (LFrame.Field('pendingDraft').AsBoolean = APending) and
      (LNode.Field('properties').Item(0).Field('value').AsText = AValue) and
      (GStudio.Agents.Revision = LFrame.Field('revision').AsInteger) then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise Exception.Create('Native publication was not acknowledged by its project');
    end;
  until False;
end;

{ Use the same visible Project entry on compact hosts. Scrolling its own lane
  to the beginning makes the four primary actions reachable without changing
  canvas/source positions or accepting any document edit. }
procedure ProjectPanel;
var
  LLeft: TControl;
begin

  if GStudio.ShellView.Root.Find('action-panel-project') <> nil then
  begin
    Click('action-panel-project');
  end;
  LLeft := GStudio.ShellView.ControlFor('studio-left');

  if LLeft is TScrollBox then
  begin
    TScrollBox(LLeft).VertScrollBar.Position := 0;
  end;
  Pump;
end;

procedure WaitProject(const AWorkspace: TNyxWorkspaceRef; const ATitle: TNyxText);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;

    if GStudio.Agents.Conflict then
    begin
      raise Exception.Create(GStudio.Agents.Status);
    end;

    if GStudio.Agents.Connected and (GStudio.Agents.Workspace.ID = AWorkspace.ID) and
      (GStudio.Session.Document.Title = ATitle) and not GStudio.SourceBusy and
      (GStudio.CanvasView.Root <> nil) and (GStudio.CanvasView.Root.ID = 'home') then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 60000 then
    begin
      Save('project-wait-status.txt', GStudio.Status);
      Capture('project-wait-failed');
      raise Exception.Create('New project/open did not reach its ordinary native canvas');
    end;
  until False;
end;

procedure SwitchProject(const AWorkspace: TNyxWorkspaceRef);
var
  LIndex: Integer;
  LNode: TNyxNode;
  LJump: TNyxText;
  LStarted: QWord;
begin
  ProjectPanel;
  Click('action-project-switch');
  LJump := '';
  for LIndex := 0 to GStudio.Agents.Workspaces.Count - 1 do
  begin
    LNode := GStudio.ShellView.Root.Find('studio-project-switch-' + IntToStr(LIndex));

    if (LNode <> nil) and
      (LNode.Extensions.Value(NyxStudioWorkspaceJumpKey).AsText = AWorkspace.ID) then
    begin
      LJump := LNode.ID;
    end;
  end;
  Check(LJump <> '', 'visible switcher identifies the exact project');
  Capture('project-actions-switcher');
  Click(LJump);
  { A project retains its panel selection too. Wait for its acknowledged mount
    before revealing Design; clicking the departing shell would target a stale
    context, while requiring a canvas in the active Project lane is impossible. }
  LStarted := GetTickCount64;
  repeat
    Pump;

    if (GStudio.Agents.Workspace.ID = AWorkspace.ID) and
      not GStudio.PresentationPending then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise Exception.Create('Visible switcher did not mount its acknowledged project');
    end;
  until False;

  if GStudio.ShellView.Root.Find('action-panel-design') <> nil then
  begin
    Click('action-panel-design');
  end;
end;

procedure ProjectActions(const AOriginal: TNyxWorkspaceRef; const ABefore: TNyxText;
  const ARange: TNyxTextSelection; const ACreated: TNyxWorkspaceRef);
const
  CProjectActions: array[0..3] of TNyxText = ('action-project-new',
    'action-project-browse', 'action-project-save-current', 'action-project-switch');
var
  LName: TEdit;
  LControl: TControl;
  LPoint: TPoint;
  LIndex: Integer;
  LStarted: QWord;
  LCreated: TNyxWorkspaceRef;
  LPair: TNyxText;
  LRevision: TNyxText;
  LStore: TNyxProjectStore;
  LCode: TMemo;
begin
  Check(GStudio.Agents.CanCreateWorkspace, 'current service advertises operator project creation');
  Check(GStudio.Agents.CanCloseWorkspace, 'current service negotiates operator lifecycle independently');
  ProjectPanel;
  for LIndex := Low(CProjectActions) to High(CProjectActions) do
  begin
    LControl := GStudio.ShellView.ControlFor(CProjectActions[LIndex]);
    Check((LControl <> nil) and LControl.IsVisible, 'primary project action is visible: ' + CProjectActions[LIndex]);
    LPoint := GForm.ScreenToClient(LControl.ClientToScreen(Point(0, 0)));
    Check((LPoint.X >= 0) and (LPoint.Y >= 0) and
      (LPoint.X + LControl.Width <= GForm.ClientWidth) and
      (LPoint.Y + LControl.Height <= GForm.ClientHeight),
      'primary project action fits actual client bounds: ' + CProjectActions[LIndex]);
  end;
  Capture('project-actions-before-new');
  LCreated := ACreated;

  if LCreated.ID <> '' then
  begin
    { The earlier input run already created this exact disposable project. A
      repaired continuation reuses it instead of consuming another project slot. }
    SwitchProject(LCreated);
  end
  else
  begin
    Click('action-project-new');
    LName := TEdit(GStudio.ShellView.InputFor('studio-new-project-name'));
    Check(LName <> nil, 'New project exposes its ordinary name field');
    LName.SetFocus;
    LName.Text := 'A new native project';
    Click('action-project-create');
    LStarted := GetTickCount64;
    repeat
      Pump;

      if GStudio.Agents.Conflict then
      begin
        raise Exception.Create(GStudio.Agents.Status);
      end;

      if (GStudio.Agents.Workspace.ID <> AOriginal.ID) and
        (GStudio.Session.Document.Title = 'A new native project') then
      begin
        LCreated := GStudio.Agents.Workspace;
        Break;
      end;

      if GetTickCount64 - LStarted > 60000 then
      begin
        raise Exception.Create('Visible New project did not create and select an independent editor');
      end;
    until False;
  end;
  WaitProject(LCreated, 'A new native project');
  Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) <> ABefore, 'New project has an independent pair');
  Check((GStudio.Session.Document.Count = 1) and
    (GStudio.Session.Document.Pages[0].Count = 0), 'New project starts with one empty page');
  Check(not GStudio.Session.ProjectSnapshot.Pending, 'New project does not inherit unfinished Pascal');
  Check(Snapshot(AOriginal).Field('project').AsText = ABefore, 'creation retains the exact original pair and draft');
  ProjectPanel;
  Click('action-project-save-current');
  LName := TEdit(GStudio.ShellView.InputFor('project-file-name'));
  Check(LName <> nil, 'Save project exposes storage without a guessed filename');
  LName.SetFocus;
  LName.Text := 'native-new-project';
  Click('action-project-save');
  LPair := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
  LStore := TNyxProjectStore.Create(GDirectory + 'projects');
  try
    Check(LStore.ReadProject('native-new-project', LRevision) = LPair,
      'ordinary Save writes the exact new design/Pascal/draft pair');
  finally
    LStore.Free;
  end;
  ProjectPanel;
  Click('action-project-browse');
  Check(GStudio.ShellView.InputFor('project-file-name') <> nil, 'Open project exposes ordinary saved-file choices');
  Click('action-project-open');

  if GStudio.ShellView.Root.Find('action-panel-design') <> nil then
  begin
    { Opening a project retains the active Project lane. Reveal its canvas by
      ordinary navigation before requiring an actual compact canvas mount. }
    Click('action-panel-design');
  end;
  WaitProject(LCreated, 'A new native project');
  Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LPair, 'ordinary Open retains the saved pair exactly');
  SwitchProject(AOriginal);
  WaitReady(AOriginal, 'Native input / 🌙 漢字');
  Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = ABefore, 'visible Switch restores the exact pending Pascal pair');
  LCode := TMemo(GStudio.CodeView.InputFor('studio-code'));
  Check((LCode <> nil) and (CaptureNyxLCLSelection(LCode).Start = ARange.Start) and
    (CaptureNyxLCLSelection(LCode).Finish = ARange.Finish), 'visible project lifecycle retains source scalar selection');
  Check(Session(AOriginal).Field('canUndo').AsBoolean, 'visible project lifecycle preserves original authoritative Undo');
  Capture('project-actions-returned');
end;

{ Resume a retained failed journey without accepting, clearing or replacing its
  unfinished source. The caller has verified the exact revision and paired file;
  ordinary navigation must preserve that same draft before the remaining actions. }
procedure ResumeProjectActions(const AFirst: TNyxWorkspaceRef;
  const ASecond: TNyxWorkspaceRef; const APair: TNyxText;
  const ACreated: TNyxWorkspaceRef);
var
  LCode: TMemo;
  LRange: TNyxTextSelection;
begin
  Application.Initialize;
  Application.OnException := GObserver.Failed;
  GForm := TForm.Create(nil);
  GForm.Caption := 'Native Nyx Studio integration';
  GForm.SetBounds(30, 30, StrToInt(ParamStr(5)), StrToInt(ParamStr(6)));
  GForm.ClientWidth := StrToInt(ParamStr(5));
  GForm.ClientHeight := StrToInt(ParamStr(6));
  GForm.Show;
  GStudio := TNyxNativeStudio.Create(GForm, GDirectory + 'projects');
  GStudio.Run;
  GStudio.ConnectService(ParamStr(1), AFirst);
  WaitReady(AFirst, 'Native input / 🌙 漢字');
  PascalAction;
  LCode := TMemo(GStudio.CodeView.InputFor('studio-code'));
  LCode.SetFocus;
  LRange := NyxTextSelection(TNyxText(LCode.Text), 4, 7);
  SelectNyxLCLText(LCode, LRange);
  Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = APair,
    'resumed native editor retains the exact protected pair');
  GStudio.JumpWorkspace(ASecond);
  WaitReady(ASecond, 'Prepared in the other project.');
  GStudio.JumpWorkspace(AFirst);
  WaitReady(AFirst, 'Native input / 🌙 漢字');
  Save('returned-display-status.txt', GStudio.Status);
  Capture('returned-display');
  Check(Pos('Display needs attention', GStudio.Status) <> 1,
    'returning to a compact project completes optional-panel restoration');
  ProjectActions(AFirst, APair, LRange, ACreated);
  GStudio.Free;
  GStudio := nil;
  Application.ProcessMessages;
  Check(GObserver.Error = '', 'resumed native editor retires queued notifications');
  WriteLn('PASS ', GChecks, ' resumed actual native project lifecycle checks');
end;

var
  LBaseline: TNyxDataValue;
  LList: TNyxDataValue;
  LFirst: TNyxWorkspaceRef;
  LSecond: TNyxWorkspaceRef;
  LBefore: TNyxText;
  LDraft: TNyxText;
  LReply: TMemo;
  LCode: TMemo;
  LRange: TNyxTextSelection;
  LFrame: TNyxDataValue;
  LGuid: TGUID;
  LReferences: TNyxDataValue;
  LFirstBefore: TNyxDataValue;
  LSecondBefore: TNyxDataValue;
  LPaints: Integer;
  LWidth: Integer;
  LHeight: Integer;
  LReuseFile: TNyxText;
  LReuseOwned: Boolean;
  LReuseRoot: Boolean;
  LResumePair: TNyxText;
  LCreated: TNyxWorkspaceRef;
begin
  GClient := nil;
  GStudio := nil;
  GForm := nil;
  GObserver := TNativeFailureObserver.Create;
  try

    if not (ParamCount in [3, 4, 5, 6]) or
      (Pos('http://127.0.0.1:', ParamStr(1)) <> 1) or
      not FileExists(ParamStr(2)) then
    begin
      raise Exception.Create('Supply owned qualification service, private fixture MCP config and output directory');
    end;
    GDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));
    ForceDirectories(GDirectory);
    LReuseFile := '';

    if ParamCount = 5 then
    begin
      LReuseFile := ParamStr(5);
    end
    else if ParamCount = 6 then
    begin
      { Explicit owned context input permits a repaired test to reuse its
        retained projects without resetting the service or consuming variants. }
      LReuseFile := GetEnvironmentVariable('NYX_TEST_WORKSPACE_CONTEXTS');
    end;
    LReuseOwned := LReuseFile <> '';
    LWidth := 1280;
    LHeight := 900;

    if ParamCount = 6 then
    begin
      Check((ParamStr(4) = 'project-actions') or (ParamStr(4) = 'project-actions-resume'),
        'explicit current-service project lifecycle mode');
      LWidth := StrToInt(ParamStr(5));
      LHeight := StrToInt(ParamStr(6));
    end;
    CreateGUID(LGuid);
    GIdentity := Copy(GUIDToString(LGuid), 2, 36);
    GRoot := 'native-review-' + Copy(GIdentity, 1, 8);
    GReplyID := GRoot + '-reply';
    LReuseRoot := False;
    GClient := TNyxMCPTestClient.Create(ParamStr(2), 'Scooty native Studio integration');
    LBaseline := Tool('nyx_session', NyxObject([]));
    LList := Tool('nyx_workspaces', NyxObject([NyxField('mode', NyxData('list'))]));
    Save('workspaces-before.json', LList.ToJSON);
    WriteLn('Existing workspace summaries: ', LList.Field('items').Count);
    Flush(Output);

    if ParamCount = 4 then
    begin
      Check(ParamStr(4) = 'probe', 'optional native workspace mode is probe');
      WriteLn('PASS bounded read-only workspace capacity probe');
    end
    else
    begin

      if LReuseOwned then
      begin

        if ParamCount = 5 then
        begin
          Check((ParamStr(4) = 'reuse-owned') or (ParamStr(4) = 'cleanup-owned'),
            'existing context reuse or recovery is explicit');
        end;
        LReferences := TNyxDataValue.ParseJSON(ReadBytes(LReuseFile));
        LFirst := NyxWorkspace(LReferences.Field('first').Field('workspace').AsText);
        LSecond := NyxWorkspace(LReferences.Field('second').Field('workspace').AsText);
        Check((LFirst.ID <> '') and (LSecond.ID <> '') and (LFirst.ID <> LSecond.ID),
          'explicit test context references are independent and never primary');
        Check(Session(LFirst).Field('revision').AsInteger =
          LReferences.Field('first').Field('revision').AsInteger, 'first existing context revision is exact');
        Check(Session(LSecond).Field('revision').AsInteger =
          LReferences.Field('second').Field('revision').AsInteger, 'second existing context revision is exact');
        LResumePair := '';

        if (ParamCount = 6) and (ParamStr(4) = 'project-actions-resume') then
        begin
          LResumePair := ReadBytes(LReferences.Field('pendingPair').AsText, 512 * 1024);
          Check(Session(LFirst).Field('pendingDraft').AsBoolean and
            not Session(LSecond).Field('pendingDraft').AsBoolean and
            (Snapshot(LFirst).Field('project').AsText = LResumePair),
            'resumption requires the exact retained protected pair');
          LCreated := NyxPrimaryWorkspace;

          if NyxAgentHas(LReferences, 'createdWorkspace') then
          begin
            LCreated := NyxWorkspace(LReferences.Field('createdWorkspace').AsText);
            Check((LCreated.ID <> '') and
              (Snapshot(LCreated).Field('project').AsText =
                ReadBytes(LReferences.Field('createdPair').AsText, 512 * 1024)),
              'continued lifecycle reuses only its exact earlier created pair');
          end;
        end
        else
        begin
          Check(not Session(LFirst).Field('pendingDraft').AsBoolean and
            not Session(LSecond).Field('pendingDraft').AsBoolean,
            'existing test contexts have no protected draft');
        end;

        if (ParamCount = 6) and NyxAgentHas(LReferences, 'root') then
        begin
          { A failed current-service journey can retain its exact authored page.
            Reset only that fixture's values; do not append another equivalent
            review page on each repaired-input rerun. }
          GRoot := LReferences.Field('root').AsText;
          Check(Pos('native-review-', GRoot) = 1, 'reused current-service root belongs to this fixture');
          GReplyID := GRoot + '-reply';
          LReuseRoot := True;
        end;

        if (ParamCount = 5) and (ParamStr(4) = 'cleanup-owned') then
        begin
          { A failed journey may leave its own review page. Recover only that
            explicitly recorded root at exact revisions, through a fresh actor's
            reviewed semantic operation. Never replace a project to clean it. }
          GRoot := LReferences.Field('root').AsText;
          Check(Pos('native-review-', GRoot) = 1, 'recovery root belongs to this fixture');
          LFirstBefore := TNyxDataValue.ParseJSON(ReadBytes(
            LReferences.Field('first').Field('original').AsText, 128 * 1024));
          LSecondBefore := TNyxDataValue.ParseJSON(ReadBytes(
            LReferences.Field('second').Field('original').AsText, 128 * 1024));
        end
        else
        begin
          LFirstBefore := Snapshot(LFirst);
          LSecondBefore := Snapshot(LSecond);
          Save('first-original.json', LFirstBefore.ToJSON);
          Save('second-original.json', LSecondBefore.ToJSON);
          Save('owned-review.json', NyxObject([
            NyxField('root', NyxData(GRoot)),
            NyxField('first', NyxData(LFirst.ID)),
            NyxField('second', NyxData(LSecond.ID))]).ToJSON);
        end;
      end
      else
      begin
        Check(LList.Field('items').Count <= 7, 'two owned native projects fit the service budget');
        LFirst := CreateProject('Native input workshop', LBaseline.Field('revision').AsInteger);
        LSecond := CreateProject('Native component workshop', LBaseline.Field('revision').AsInteger);
      end;

      if (ParamCount = 6) and (ParamStr(4) = 'project-actions-resume') then
      begin
        ResumeProjectActions(LFirst, LSecond, LResumePair, LCreated);
      end
      else if (ParamCount = 5) and (ParamStr(4) = 'cleanup-owned') then
      begin
        Cleanup(LFirst, LFirstBefore);
        Cleanup(LSecond, LSecondBefore);
        Check(PrimaryFrame(Tool('nyx_session', NyxObject([]))) = PrimaryFrame(LBaseline),
          'reviewed recovery retains the primary frame');
        WriteLn('PASS ', GChecks, ' exact reviewed recovery checks');
      end
      else
      begin
        if LReuseRoot then
        begin
          ChangeValue(LFirst, 'Ready for your ideas.');
          ChangeValue(LSecond, 'Ready for your ideas.');
        end
        else
        begin
          Compose(LFirst, 'Input workshop');
          Compose(LSecond, 'Component workshop');
        end;

        Application.Initialize;
        Application.OnException := GObserver.Failed;
        QualifyTransport(LFirst);
        GForm := TForm.Create(nil);
        GForm.Caption := 'Native Nyx Studio integration';
        GForm.SetBounds(30, 30, LWidth, LHeight);
        GForm.ClientWidth := LWidth;
        GForm.ClientHeight := LHeight;
        GForm.Show;
        GStudio := TNyxNativeStudio.Create(GForm, GDirectory + 'projects');
        GStudio.Run;
        GStudio.ConnectService(ParamStr(1), LFirst);
        WaitReady(LFirst, 'Ready for your ideas.');
        Check(GStudio.Session.Document.Title = 'Input workshop',
          'native full editor adopts exact MCP-authored project');
        Check(GStudio.Agents.Workspaces.Count >= 3,
          'native Agents presentation lists concurrent project sessions');
        Check(GStudio.CanvasView.InputFor(GReplyID) is TMemo,
          'MCP document mounts its actual native memo');

        ChangeValue(LFirst, 'Live from a semantic tool.');
        WaitReady(LFirst, 'Live from a semantic tool.');
        LReply := TMemo(GStudio.CanvasView.InputFor(GReplyID));
        Check(TNyxText(LReply.Text) = 'Live from a semantic tool.',
          'observing native control receives semantic MCP edits');
        Check(GStudio.Agents.Activity.Count > 0, 'native user sees semantic agent activity');

        LReply.Text := 'Native input / 🌙 漢字';
        WaitPublication(LFirst, 'Native input / 🌙 漢字', False);
        Check(GStudio.Session.Document.Find(GReplyID).Prop('value') = TNyxText('Native input / 🌙 漢字'),
          'actual native input publishes exact supplementary and multilingual text');
        Click('action-undo');
        WaitReady(LFirst, 'Live from a semantic tool.');
        Click('action-redo');
        WaitReady(LFirst, 'Native input / 🌙 漢字');
        Check(Session(LFirst).Field('canUndo').AsBoolean,
          'native history commands use the authoritative project history');

        PascalAction;
        Save('pascal-action-state.json', NyxObject([
          NyxField('status', NyxData(GStudio.Status)),
          NyxField('presentationPending', NyxData(GStudio.PresentationPending)),
          NyxField('sourceMounted', NyxData(GStudio.CodeView.Root <> nil))]).ToJSON);
        Capture('pascal-action');
        LCode := TMemo(GStudio.CodeView.InputFor('studio-code'));
        { Native input inserts the host line ending while the editing adapter
          preserves the accepted file's existing LF prefix. Qualify those actual
          inserted bytes, rather than requiring an LF the widget did not enter. }
        LDraft := GStudio.Session.Source + TNyxText(LineEnding) +
          TNyxText('{ Private draft / 🌙 漢字 }') + TNyxText(LineEnding);
        LCode.Text := LDraft;
        LCode.SetFocus;
        LRange := NyxTextSelection(TNyxText(LCode.Text), 4, 7);
        SelectNyxLCLText(LCode, LRange);
        WaitPublication(LFirst, 'Native input / 🌙 漢字', True);
        LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
        Save('first-before-switch.nyxproject', LBefore);

        ChangeValue(LSecond, 'Prepared in the other project.');
        GStudio.JumpWorkspace(LSecond);
        WaitReady(LSecond, 'Prepared in the other project.');
        Check(GStudio.Session.Document.Title = 'Component workshop',
          'project jump mounts the second full editor');
        Check(not GStudio.Session.ProjectSnapshot.Pending,
          'second project receives no first-project draft');
        Check(Session(LFirst).Field('pendingDraft').AsBoolean,
          'departure retains pending source on the first service context');

        ChangeValue(LSecond, 'A second independent edit.');
        WaitReady(LSecond, 'A second independent edit.');
        Click('action-undo');
        WaitReady(LSecond, 'Prepared in the other project.');
        Check(Session(LFirst).Field('pendingDraft').AsBoolean,
          'second-project Undo cannot alter another project');

        GStudio.JumpWorkspace(LFirst);
        WaitReady(LFirst, 'Native input / 🌙 漢字');
        Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore,
          'return preserves exact accepted/design/draft/base bytes');
        Check(GStudio.Session.DraftSource = LDraft,
          'return restores the exact private Unicode Pascal draft');
        LCode := TMemo(GStudio.CodeView.InputFor('studio-code'));
        Check(LCode <> nil, 'return restores optional code visibility');
        Check((CaptureNyxLCLSelection(LCode).Start = LRange.Start) and
          (CaptureNyxLCLSelection(LCode).Finish = LRange.Finish),
          'return restores the source scalar range');
        Check(Screen.ActiveControl = LCode, 'return restores actual source focus');
        Check(GStudio.CanvasView.ControlFor(GReplyID).Height > 100,
          'returned full canvas has a useful actual memo viewport');

        LFrame := Session(LFirst);
        Check(LFrame.Field('canUndo').AsBoolean, 'first project retains authoritative Undo');
        Check(PrimaryFrame(Tool('nyx_session', NyxObject([]))) = PrimaryFrame(LBaseline),
          'native concurrent editor retains published primary frame');
        Save('primary-before.json', LBaseline.ToJSON);
        Save('primary-after.json', Tool('nyx_session', NyxObject([])).ToJSON);
        Save('first-after.json', Session(LFirst).ToJSON);
        Save('second-after.json', Session(LSecond).ToJSON);
        if ParamCount = 6 then
        begin
          ProjectActions(LFirst, LBefore, LRange, NyxPrimaryWorkspace);
        end
        else
        begin
          Check(not GStudio.Agents.CanCloseWorkspace,
            'older staged service exposes unavailable closure without claiming it ran');
        end;

        if ParamCount = 5 then
        begin
          Click('action-reset-source');
          WaitPublication(LFirst, 'Native input / 🌙 漢字', False);
          PascalAction;
          LReply := TMemo(GStudio.CanvasView.InputFor(GReplyID));
          LReply.Text := 'Your next idea starts here.';
          WaitPublication(LFirst, 'Your next idea starts here.', False);
          Click('action-agents');
          Capture('native-service-desktop');
          LPaints := GStudio.PaintCount;
          GForm.ClientWidth := 390;
          repeat
            Pump;
          until GStudio.PaintCount > LPaints;
          Check(GForm.ClientWidth = 390, 'native connected editor uses an actual compact viewport');
          Check(TNyxText(TMemo(GStudio.CanvasView.InputFor(GReplyID)).Text) =
            TNyxText('Your next idea starts here.'), 'compact connected canvas retains English presentation');
          Capture('native-service-narrow');
          Cleanup(LFirst, LFirstBefore);
          Cleanup(LSecond, LSecondBefore);
        end;

        GStudio.RequestRefresh;
        GStudio.Free;
        GStudio := nil;
        Application.ProcessMessages;
        Check(GObserver.Error = '', 'native destruction cancels protocol and queued paints before session release');
        WriteLn('PASS ', GChecks, ' actual native service/workspace editor checks');
      end;
    end;
  except
    on LException: Exception do
    begin
      WriteLn(StdErr, 'FAIL ', LException.Message, ' at check ', GChecks);
      ExitCode := 1;
    end;
  end;

  if GClient <> nil then
  begin
    GClient.Close;
  end;
  GStudio.Free;
  GForm.Free;
  GClient.Free;
  GObserver.Free;
end.
