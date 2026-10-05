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
  Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, IntfGraphics, FPWritePNG,
  nyx.text, nyx.data, nyx.model, nyx.codec, nyx.editing, nyx.editing.lcl,
  nyx.render.lcl, nyx.studio.lcl, nyx.studio.workspaces, nyx.studio.agentview,
  nyx.studio.projects, nyx.studio.agents, nyx.studio.exchange,
  nyx.studio.exchange.lcl, nyx.test.mcp.client;

type
  TControlAccess = class(TControl);
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
      raise Exception.Create('Native workspace observation exceeded its functional wait');
    end;
  until False;
end;

procedure Click(const AID: TNyxText);
var
  LControl: TControl;
  LPaints: Integer;
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
  Application.ProcessMessages;
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
begin
  GClient := nil;
  GStudio := nil;
  GForm := nil;
  GObserver := TNativeFailureObserver.Create;
  try

    if not (ParamCount in [3, 4, 5]) or
      (Pos('http://127.0.0.1:', ParamStr(1)) <> 1) or
      (Pos('build', ParamStr(2)) = 0) then
    begin
      raise Exception.Create('Supply owned qualification service, private fixture MCP config and output directory');
    end;
    GDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));
    ForceDirectories(GDirectory);
    CreateGUID(LGuid);
    GIdentity := Copy(GUIDToString(LGuid), 2, 36);
    GRoot := 'native-review-' + Copy(GIdentity, 1, 8);
    GReplyID := GRoot + '-reply';
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

      if ParamCount = 5 then
      begin
        Check((ParamStr(4) = 'reuse-owned') or (ParamStr(4) = 'cleanup-owned'),
          'existing context reuse or recovery is explicit');
        LReferences := TNyxDataValue.ParseJSON(ReadBytes(ParamStr(5)));
        LFirst := NyxWorkspace(LReferences.Field('first').Field('workspace').AsText);
        LSecond := NyxWorkspace(LReferences.Field('second').Field('workspace').AsText);
        Check((LFirst.ID <> '') and (LSecond.ID <> '') and (LFirst.ID <> LSecond.ID),
          'explicit test context references are independent and never primary');
        Check(Session(LFirst).Field('revision').AsInteger =
          LReferences.Field('first').Field('revision').AsInteger, 'first existing context revision is exact');
        Check(Session(LSecond).Field('revision').AsInteger =
          LReferences.Field('second').Field('revision').AsInteger, 'second existing context revision is exact');
        Check(not Session(LFirst).Field('pendingDraft').AsBoolean and
          not Session(LSecond).Field('pendingDraft').AsBoolean,
          'existing test contexts have no protected draft');

        if ParamStr(4) = 'cleanup-owned' then
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

      if (ParamCount = 5) and (ParamStr(4) = 'cleanup-owned') then
      begin
        Cleanup(LFirst, LFirstBefore);
        Cleanup(LSecond, LSecondBefore);
        Check(PrimaryFrame(Tool('nyx_session', NyxObject([]))) = PrimaryFrame(LBaseline),
          'reviewed recovery retains the primary frame');
        WriteLn('PASS ', GChecks, ' exact reviewed recovery checks');
      end
      else
      begin
        Compose(LFirst, 'Input workshop');
        Compose(LSecond, 'Component workshop');

        Application.Initialize;
        Application.OnException := GObserver.Failed;
        QualifyTransport(LFirst);
        GForm := TForm.Create(nil);
        GForm.Caption := 'Native Nyx Studio integration';
        GForm.SetBounds(30, 30, 1280, 900);
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

        Click('action-code');
        LCode := TMemo(GStudio.CodeView.InputFor('studio-code'));
        LDraft := GStudio.Session.Source + #10 + TNyxText('{ Private draft / 🌙 漢字 }') + #10;
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
        Check(not GStudio.Agents.CanCloseWorkspace,
          'older staged service exposes unavailable closure without claiming it ran');

        if ParamCount = 5 then
        begin
          Click('action-reset-source');
          WaitPublication(LFirst, 'Native input / 🌙 漢字', False);
          Click('action-code');
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
      WriteLn(StdErr, 'FAIL ', LException.Message);
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
