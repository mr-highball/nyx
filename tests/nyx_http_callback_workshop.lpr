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

program nyx_http_callback_workshop;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.model, nyx.types, nyx.source,
  nyx.callbacks, nyx.scheduler, nyx.studio.projects, nyx.studio.agents,
  nyx.studio.workspaces, nyx.studio.recovery, nyx.studio.recovery.native,
  nyx.studio.directories, nyx.studio.outputs, nyx.studio.builds,
  nyx.studio.buildexecutor, nyx.studio.buildjobs,
  nyx.studio.sourceprojection, nyx.studio.history, nyx.studio.callbackedits,
  nyx.studio.callbackintent, nyx.studio.handleredits, nyx.studio.handlerintent,
  nyx.test.mcp.client, nyx.test.browser.pipe, nyx.test.projection;

const
  COwnershipFile = 'callback-workshop-owned.nyx';
  COrigin: TNyxText = 'http://127.0.0.1:9080';
  CFirst: TNyxText = #10 + 'begin' + #10 +
    '  { A crafted response 🚀 𐐷. }' + #10 + '  AExecution.Cancel;' + #10 + 'end;';
  CSecond: TNyxText = #10 + 'begin' + #10 +
    '  { Failure belongs to this registration, not its siblings. }' + #10 +
    '  raise Exception.Create(''An authored failure 🚀 𐐷.'');' + #10 + 'end;';
  CCaretProbe: TNyxText = '{ Event navigation check. }' + #10;

var
  GClient: TNyxMCPTestClient;
  GBrowser: TNyxBrowserPipe;
  GEvidence: TNyxText;
  GRevision: Integer;
  GSerial: Integer;
  GChecks: Integer;
  GJobs: Integer;

{ All files are explicit native UTF-8 boundaries. The private seed/receipts are
  never served, printed or inserted into application properties. }
function ReadText(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try

    if (LStream.Size < 1) or (LStream.Size > 4 * 1024 * 1024) then
    begin
      raise Exception.Create('Callback workshop input exceeds its byte bound');
    end;
    SetLength(Result, LStream.Size);
    LStream.ReadBuffer(Result[1], Length(Result));
    Result := NyxDecodeUTF8(NyxEncodeUTF8(Result));
  finally
    LStream.Free;
  end;
end;

procedure WriteText(const APath, AText: TNyxText);
var
  LStream: TFileStream;
begin
  ForceDirectories(ExtractFileDir(APath));
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if AText <> '' then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;

procedure Save(const AName, AText: TNyxText);
begin
  WriteText(GEvidence + AName, AText);
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('HTTP callback workshop: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Seed only a NEW isolated runtime with the existing qualification fixture. This
  is an explicit trusted execution attachment, not an MCP-authored demo/import.
  The installed project-file tool's compiler-backed import gap stays separate.
  Actual FPC construction must reproduce the independent complete descriptor
  before anything is checkpointed. No listener or existing runtime is changed. }
procedure Seed;
var
  LDirectories: TNyxStudioDirectories;
  LProfile: TNyxOutputConfiguration;
  LVerifier: INyxRuntimeSourceVerifier;
  LProjection: INyxSourceProjection;
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LPair: TNyxProjectPair;
  LFrame: TNyxAgentRecoveryFrame;
  LPrimary: TNyxAgentSession;
  LRegistry: TNyxStudioWorkspaces;
  LStore: TNyxStudioRuntimeStore;
  LSource: TNyxText;
begin

  if (ParamCount <> 5) or DirectoryExists(ParamStr(5)) or FileExists(ParamStr(5)) then
  begin
    raise Exception.Create('Seed requires release, fixture unit, private profile and NEW runtime');
  end;
  LDocument := nil;
  LWorkspace := nil;
  LPrimary := nil;
  LRegistry := nil;
  LStore := nil;
  LProfile := nil;
  try
    { Current-source qualification owns a new repository runtime; installed
      qualification retains the verified, disjoint release admission. Neither
      mode can overwrite an existing runtime or enroll a user configuration. }

    if ParamStr(1) = 'repository-seed' then
    begin
      LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(2))
        .RunningIn(ParamStr(5)).EnrollingProject(ParamStr(5));
    end
    else
    begin
      LDirectories := TNyxStudioDirectories.ForRelease(ParamStr(2), ParamStr(5))
        .EnrollingProject(ParamStr(5));
    end;
    LProfile := TNyxOutputConfiguration.Decode(ReadText(ParamStr(4)));
    LSource := ReadText(ParamStr(3));
    LVerifier := NewNyxNativeRuntimeSourceVerifier(LDirectories, LProfile.Encode,
      TNyxCompilerLimits.Default, nil, spcChecked);
    LProjection := LVerifier.Verify(LSource);
    Check((LProjection.State = spsExecuted) and
      (LProjection.Design = ExpectedNyxProjectionDesign),
      'real constructor reproduces the whole independent fixture');
    AdmitNyxProjectedProject(NyxProjectPair(ExpectedNyxProjectionDesign, LSource),
      LProjection, LDocument, LWorkspace, LPair);
    LFrame := Default(TNyxAgentRecoveryFrame);
    LFrame.Revision := 17;
    LFrame.Claimed := True;
    LFrame.Permission := apEdit;
    LFrame.Session.Pair := LPair;
    LFrame.Session.AcceptedCheckpoint := LWorkspace.Capture;
    LFrame.Session.Selection := 'heading-1';
    LFrame.Session.View := 'notebook-1';
    LFrame.Session.NextID := 1;
    LPrimary := TNyxAgentSession.CreateRecovered(LFrame);
    LRegistry := TNyxStudioWorkspaces.Create(LPrimary, 'callback-workshop');
    LStore := TNyxStudioRuntimeStore.Create(LDirectories);
    LStore.Save(LPrimary, LRegistry);
    WriteText(LDirectories.OutputProfile, LProfile.Encode);
    WriteText(LDirectories.RuntimeRoot + 'initial-project.nyxpair', EncodeNyxProject(LPair));
    WriteText(LDirectories.RuntimeRoot + COwnershipFile, NyxObject([
      NyxField('kind', NyxData('isolated-callback-workshop')),
      NyxField('origin', NyxData(COrigin)),
      NyxField('release', NyxData(LDirectories.SourceRoot))]).ToJSON);
    WriteLn('PASS trusted isolated callback seed; no listener or existing project changed');
  finally
    LStore.Free;
    LRegistry.Free;
    LPrimary.Free;
    LWorkspace.Free;
    LDocument.Free;
    LProjection := nil;
    LVerifier := nil;
    LProfile.Free;
  end;
end;

{ Only authenticated semantic tools author the accepted pair. Error receipts
  stay private; console diagnostics expose no source/credentials/server bodies. }
function Call(const ATool: TNyxText; const AArgs: TNyxDataValue): TNyxDataValue;
var
  LReply: TNyxDataValue;
begin
  LReply := GClient.Tool(ATool, AArgs);

  if LReply.Field('isError').AsBoolean then
  begin
    Save('refusal-' + ATool + '.json', LReply.ToJSON);
    raise Exception.Create('Authenticated semantic operation refused: ' + ATool);
  end;
  Result := LReply.Field('structuredContent');
end;

procedure Refresh;
begin
  GRevision := Call('nyx_session', NyxObject([])).Field('revision').AsInteger;
end;

function Operation: TNyxText;
begin
  Inc(GSerial);
  Result := 'http-callback-' + TNyxText(IntToStr(GSerial));
end;

function Args(const AChanges: TNyxDataValue;
  const AReview: TNyxText = ''): TNyxDataValue;
var
  LFields: array of TNyxDataField;
begin
  SetLength(LFields, 4);
  LFields[0] := NyxField('mode', NyxData('apply'));
  LFields[1] := NyxField('expectedRevision', NyxData(GRevision));
  LFields[2] := NyxField('operationId', NyxData(Operation));
  LFields[3] := NyxField('changes', AChanges);

  if AReview <> '' then
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField('reviewID', NyxData(AReview));
  end;
  Result := NyxObject(LFields);
end;

{ Whole-pair exports are limited to this exact owned fixture. Every response is
  scalar-bounded at one immutable revision; user/other-workspace data is absent. }
function Pair: TNyxText;
var
  LReply: TNyxDataValue;
  LOffset: Integer;
begin
  Result := '';
  LOffset := 0;
  repeat
    LReply := Call('nyx_project', NyxObject([NyxField('mode', NyxData('export')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('part', NyxData('project')), NyxField('offset', NyxData(LOffset)),
      NyxField('count', NyxData(4096))]));
    Check(LReply.Field('revision').AsInteger = GRevision,
      'bounded pair export retains its exact revision');
    Result := Result + LReply.Field('text').AsText;
    LOffset := LReply.Field('nextOffset').AsInteger;
  until LOffset = LReply.Field('total').AsInteger;
end;

procedure Pump;
begin

  if GBrowser.RuntimeError <> '' then
  begin
    GBrowser.CaptureRuntimeError;
    raise Exception.Create('Ordinary Studio raised a browser runtime exception');
  end;
  Sleep(40);
end;

procedure WaitFace(const ASelector: TNyxText);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat

    if GBrowser.Exists(ASelector) then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      GBrowser.Capture('missing-face');
      raise Exception.Create('Ordinary observing face did not become ready');
    end;
    Pump;
  until False;
end;

procedure WaitSource(const ASource: TNyxText);
var
  LStarted: QWord;
  LActual: TNyxText;
begin
  LStarted := GetTickCount64;
  repeat

    if GBrowser.TryFieldValue('[data-node="studio-code"]', LActual) and
      (LActual = ASource) then
    begin
      Check(True, 'ordinary observing editor receives exact complete Pascal');
      Exit;
    end;

    if GetTickCount64 - LStarted > 20000 then
    begin
      GBrowser.Capture('source-refusal');
      raise Exception.Create('Ordinary observing source did not receive the exact pair');
    end;
    Pump;
  until False;
end;

procedure WaitText(const ASelector, AText: TNyxText);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat

    if Pos(AText, GBrowser.ElementHTML(ASelector)) > 0 then
    begin
      Check(True, 'ordinary Events inspector presents the semantic callback result');
      Exit;
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      GBrowser.Capture('events-refusal');
      raise Exception.Create('Ordinary observing Events did not receive the exact callbacks');
    end;
    Pump;
  until False;
end;

{ Poll the SAME immutable job to terminal state. An observation deadline retains
  the receipt and reports failure; it never resubmits or restarts a compiler. }
procedure AwaitPublication(const AReceipt: TNyxDataValue; const AName: TNyxText);
var
  LStarted: QWord;
  LStatus: TNyxDataValue;
  LState: TNyxText;
begin
  Save(AName + '-receipt.json', AReceipt.ToJSON);
  Check((AReceipt.Field('job').AsText <> '') and
    (AReceipt.Field('publication').Field('state').AsText = 'pending'),
    'handwritten callback proposal waits behind the owning compiler');
  Inc(GJobs);
  LStarted := GetTickCount64;
  repeat
    LStatus := Call('nyx_build', NyxObject([NyxField('mode', NyxData('status')),
      NyxField('job', AReceipt.Field('job')), NyxField('limit', NyxData(10))]));
    Save(AName + '-status.json', LStatus.ToJSON);
    LState := LStatus.Field('state').AsText;

    if (LState = 'succeeded') or (LState = 'failed') or (LState = 'cancelled') then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 120000 then
    begin
      raise Exception.Create('Same callback job exceeded observation budget; receipt retained');
    end;
    Pump;
  until False;
  Check((LState = 'succeeded') and
    (LStatus.Field('publication').Field('state').AsText = 'committed'),
    'actual constructor and whole-pair publication succeed');
  Check(LStatus.Field('publication').Field('revision').AsInteger = GRevision + 1,
    'one related semantic group publishes exactly one revision');
  Refresh;
  WaitSource(DecodeNyxProject(Pair).Source);
end;

{ History is semantic too. Exact accepted design and complete Pascal are the
  independent oracle, not registration counts or a successfully compiled stub. }
procedure History(const ADirection, AExpected: TNyxText);
begin
  Call('nyx_history', NyxObject([NyxField('direction', NyxData(ADirection)),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(Operation))]));
  Refresh;
  Check(Pair = AExpected, 'one semantic ' + ADirection + ' restores the exact pair');
  WaitSource(DecodeNyxProject(AExpected).Source);
end;

procedure SaveApplication(const AName, APacket: TNyxText);
var
  LPair: TNyxProjectPair;
begin
  LPair := DecodeNyxProject(APacket);
  Save(AName + '/nyx.projection.fixture.pas', LPair.Source);
  Save(AName + '/design.json', LPair.Design);
end;

{ Normal application compiler jobs remain separate from constructor admission.
  Exact accepted sources are retained for subsequent real runtime consumers. }
procedure CompileApplication(const ATarget, AName: TNyxText);
var
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LStarted: QWord;
  LSource: TNyxText;
begin
  LSource := DecodeNyxProject(Pair).Source;
  LReceipt := Call('nyx_build', NyxObject([NyxField('mode', NyxData('request')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(Operation)), NyxField('target', NyxData(ATarget)),
    NyxField('scope', NyxData('application')),
    NyxField('outputID', Call('nyx_build', NyxObject([
      NyxField('mode', NyxData('outputs'))])).Field('outputID'))]));
  Save(AName + '-' + ATarget + '-receipt.json', LReceipt.ToJSON);
  Inc(GJobs);
  LStarted := GetTickCount64;
  repeat
    LStatus := Call('nyx_build', NyxObject([NyxField('mode', NyxData('status')),
      NyxField('job', LReceipt.Field('job')), NyxField('limit', NyxData(10))]));
    Save(AName + '-' + ATarget + '-status.json', LStatus.ToJSON);

    if (LStatus.Field('state').AsText = 'succeeded') or
      (LStatus.Field('state').AsText = 'failed') then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 120000 then
    begin
      raise Exception.Create('Same application compiler job exceeded observation budget');
    end;
    Pump;
  until False;
  Check((LStatus.Field('state').AsText = 'succeeded') and
    LStatus.Field('currentSource').AsBoolean and
    (LStatus.Field('sourceFingerprint').AsText = NyxBuildFingerprint(LSource)),
    'ordinary ' + ATarget + ' compiler retains exact accepted application source');
end;

procedure Run;
var
  LRuntime: TNyxText;
  LInitial: TNyxText;
  LAdded: TNyxText;
  LAuthored: TNyxText;
  LMoved: TNyxText;
  LRemoved: TNyxText;
  LValue: TNyxDataValue;
  LArguments: TNyxDataValue;
  LChanges: TNyxDataValue;
  LFirst: TNyxCallbackRef;
  LSecond: TNyxCallbackRef;
  LFirstHandler: TNyxHandlerRef;
  LSecondHandler: TNyxHandlerRef;
  LFirstCode: TNyxText;
  LSecondCode: TNyxText;
  LPair: TNyxProjectPair;
  LPosition: Integer;
  LProbe: TNyxText;
  LStarted: QWord;
  LIndex: Integer;
begin

  if (ParamCount <> 3) then
  begin
    raise Exception.Create('Run requires exact owned runtime and NEW evidence directory');
  end;
  LRuntime := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));
  GEvidence := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));

  if DirectoryExists(GEvidence) or FileExists(GEvidence) then
  begin
    raise Exception.Create('Callback evidence output must be new');
  end;
  LValue := TNyxDataValue.ParseJSON(ReadText(LRuntime + COwnershipFile));
  Check((LValue.Field('kind').AsText = 'isolated-callback-workshop') and
    (LValue.Field('origin').AsText = COrigin), 'explicit isolated runtime ownership');
  ForceDirectories(GEvidence);
  GClient := TNyxMCPTestClient.Create(LRuntime + '.codex/config.toml',
    'Scooty observing callback workshop');
  Refresh;
  LInitial := Pair;
  Check(LInitial = ReadText(LRuntime + 'initial-project.nyxpair'),
    'isolated project is still its exact executed seed');
  Check(GClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools').Count = 23,
    'fresh authenticated discovery retains twenty-three tools');
  GBrowser := TNyxBrowserPipe.Create(COrigin + '/', GEvidence + 'observer', 1280, 960);
  WaitFace('[data-node="action-code"]');
  for LIndex := 1 to 30 do
  begin
    Pump;
  end;

  if GBrowser.Exists('[data-node="action-agent-accept"]') then
  begin
    GBrowser.Click('[data-node="action-agent-accept"]');
  end;
  GBrowser.Click('[data-node="action-code"]');
  WaitFace('[data-node="inspector-tab-events"]');
  GBrowser.Click('[data-node="inspector-tab-events"]');
  WaitSource(DecodeNyxProject(LInitial).Source);

  LChanges := CaptureNyxCallbackBatch(NyxCallbackPatch([
    NyxAddCallback('heading-1', NyxCallbackEvent(ntClick)),
    NyxAddCallback('heading-1', NyxCallbackEvent(ntClick)),
    NyxSetCallbackPolicy('heading-1', NyxCallbackEvent(ntClick), neUIQueue)])).ToData;
  LArguments := Args(LChanges);
  LValue := Call('nyx_callbacks', LArguments);
  LFirst := NyxCallbackID(LValue.Field('proposedCallbacks').Item(0)
    .Field('registration').AsText);
  LSecond := NyxCallbackID(LValue.Field('proposedCallbacks').Item(1)
    .Field('registration').AsText);
  LFirstHandler := NyxHandler(LValue.Field('proposedCallbacks').Item(0)
    .Field('handler').AsText);
  LSecondHandler := NyxHandler(LValue.Field('proposedCallbacks').Item(1)
    .Field('handler').AsText);
  Check(Call('nyx_callbacks', LArguments).ToJSON = LValue.ToJSON,
    'exact authenticated retry cannot duplicate proposed registrations');
  AwaitPublication(LValue, 'add');
  LAdded := Pair;
  WaitText('[data-node="event-click-count"]', '2 registrations');
  Check(GBrowser.Exists('[data-node="event-click-policy"] ' +
    'option[value="ui-queue"]:checked'), 'ordinary policy select observes UI queue');
  GBrowser.Capture('callback-added');

  LValue := Call('nyx_pascal', NyxObject([NyxField('mode', NyxData('inspect')),
    NyxField('handler', NyxData(LFirstHandler.Name)),
    NyxField('count', NyxData(4096))]));
  Check((LValue.Field('revision').AsInteger = GRevision) and
    (LValue.Field('nextOffset').AsInteger = LValue.Field('total').AsInteger),
    'first complete implementation window belongs to the guarded revision');
  LFirstCode := LValue.Field('text').AsText;
  LValue := Call('nyx_pascal', NyxObject([NyxField('mode', NyxData('inspect')),
    NyxField('handler', NyxData(LSecondHandler.Name)),
    NyxField('count', NyxData(4096))]));
  Check((LValue.Field('revision').AsInteger = GRevision) and
    (LValue.Field('nextOffset').AsInteger = LValue.Field('total').AsInteger),
    'second complete implementation window retains that same guarded revision');
  LSecondCode := LValue.Field('text').AsText;
  Check((Pos('TODO', LFirstCode) > 0) and (Pos('TODO', LSecondCode) > 0),
    'bounded semantic inspection retrieves both exact TODO bodies');
  LChanges := CaptureNyxHandlerBatch(NewNyxHandlerPatch([
    NyxHandlerEdit(LFirstHandler, LFirstCode, CFirst),
    NyxHandlerEdit(LSecondHandler, LSecondCode, CSecond)])).ToData;
  LValue := Call('nyx_pascal', Args(LChanges));
  AwaitPublication(LValue, 'bodies');
  LAuthored := Pair;
  LPair := DecodeNyxProject(LAuthored);
  Check((ReadNyxHandlerSource(LPair.Source, LFirstHandler).Code = CFirst) and
    (ReadNyxHandlerSource(LPair.Source, LSecondHandler).Code = CSecond),
    'both exact Unicode implementations are admitted together');
  SaveApplication('application-queued', LAuthored);
  GBrowser.Capture('callback-bodies');
  CompileApplication('browser', 'queued');
  CompileApplication('lcl', 'queued');
  History('undo', LAdded);
  History('redo', LAuthored);

  { Physical typing proves the real navigation caret, without script evaluation
    or reading private controller state. This intentionally creates an unfinished
    draft, then qualifies semantic refusal and explicit ordinary restoration. }
  GBrowser.Click('[data-node="event-click-callback-0-source"]');
  LPosition := NyxTextPosition(LPair.Source,
    ReadNyxHandlerSource(LPair.Source, LFirstHandler).Line, 1);
  LProbe := Copy(LPair.Source, 1, LPosition - 1) + CCaretProbe +
    Copy(LPair.Source, LPosition, MaxInt);
  GBrowser.TypeText(CCaretProbe);
  WaitSource(LProbe);
  LStarted := GetTickCount64;
  repeat
    LValue := Call('nyx_session', NyxObject([]));

    if LValue.Field('pendingDraft').AsBoolean then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      raise Exception.Create('Ordinary source draft was not observed by the server');
    end;
    Pump;
  until False;
  Refresh;
  Check(GClient.Tool('nyx_callbacks', Args(CaptureNyxCallbackBatch(NyxCallbackPatch([
    NyxSetCallbackPolicy('heading-1', NyxCallbackEvent(ntClick), neSequential)]))
    .ToData)).Field('isError').AsBoolean, 'pending ordinary draft refuses semantic mutation');
  GBrowser.Click('[data-node="action-reset-source"]');
  WaitSource(LPair.Source);
  LStarted := GetTickCount64;
  repeat
    LValue := Call('nyx_session', NyxObject([]));

    if not LValue.Field('pendingDraft').AsBoolean then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      raise Exception.Create('Explicit ordinary draft restoration was not acknowledged');
    end;
    Pump;
  until False;
  Refresh;
  Check(Pair = LAuthored, 'navigation input/restoration retains exact accepted pair');

  LChanges := CaptureNyxCallbackBatch(NyxCallbackPatch([
    NyxMoveCallback('heading-1', NyxCallbackEvent(ntClick), LSecond, 0),
    NyxSetCallbackPolicy('heading-1', NyxCallbackEvent(ntClick), neSequential)])).ToData;
  LValue := Call('nyx_callbacks', Args(LChanges));
  AwaitPublication(LValue, 'order');
  LMoved := Pair;
  WaitText('[data-node="event-click-callback-0-source"]', LSecondHandler.Name);
  Check(GBrowser.Exists('[data-node="event-click-policy"] ' +
    'option[value="sequential"]:checked'),
    'ordinary policy select observes the grouped sequential choice');
  SaveApplication('application-sequential', LMoved);
  CompileApplication('browser', 'sequential');
  CompileApplication('lcl', 'sequential');
  GBrowser.Resize(390, 844);
  WaitFace('[data-node="action-panel-inspector"]');
  GBrowser.Click('[data-node="action-panel-inspector"]');
  WaitText('[data-node="event-click-count"]', '2 registrations');
  WaitText('[data-node="event-click-callback-0-source"]', LSecondHandler.Name);
  GBrowser.Capture('callback-order-narrow');
  GBrowser.Resize(1280, 960);
  WaitFace('[data-node="event-click-count"]');

  LChanges := CaptureNyxCallbackBatch(NyxCallbackPatch([
    NyxRemoveCallback('heading-1', NyxCallbackEvent(ntClick), LFirst)])).ToData;
  Check(GClient.Tool('nyx_callbacks', Args(LChanges)).Field('isError').AsBoolean,
    'unreviewed removal refuses without deleting code');
  LValue := Call('nyx_callbacks', NyxObject([NyxField('mode', NyxData('review')),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('changes', LChanges)]));
  Check(Pos('retained', LValue.Field('callbacks').Item(0).Field('warning').AsText) > 0,
    'exact removal review explains that the implementation remains');
  LValue := Call('nyx_callbacks', Args(LChanges, LValue.Field('reviewID').AsText));
  AwaitPublication(LValue, 'remove');
  LRemoved := Pair;
  WaitText('[data-node="event-click-count"]', '1 registrations');
  Check(ReadNyxHandlerSource(DecodeNyxProject(LRemoved).Source, LFirstHandler).Code = CFirst,
    'reviewed removal retains the authored Pascal body');
  GBrowser.Capture('callback-removed');
  History('undo', LMoved);
  History('redo', LRemoved);
  Check(GJobs = 8, 'four constructor publications and four application compiler jobs');
  Save('result.json', NyxObject([NyxField('checks', NyxData(GChecks)),
    NyxField('jobs', NyxData(GJobs)), NyxField('revision', NyxData(GRevision))]).ToJSON);
  WriteLn('PASS ', GChecks, ' authenticated HTTP/ordinary callback checks; ', GJobs, ' compiler jobs');
end;

{ Failed input qualification can leave its OWN pending source and paired history.
  Restore through the ordinary public control, then undo only this new runtime's
  admitted commands until its exact seed pair returns. Never reset checkpoint
  files, restart a service, infer ownership from a title or touch the LAN host. }
procedure Restore;
var
  LRuntime: TNyxText;
  LInitial: TNyxText;
  LValue: TNyxDataValue;
  LIndex: Integer;
  LStarted: QWord;
begin

  if ParamCount <> 3 then
  begin
    raise Exception.Create('Restore requires exact owned runtime and NEW evidence directory');
  end;
  LRuntime := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));
  GEvidence := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));

  if DirectoryExists(GEvidence) or FileExists(GEvidence) then
  begin
    raise Exception.Create('Owned restoration evidence must be new');
  end;
  LValue := TNyxDataValue.ParseJSON(ReadText(LRuntime + COwnershipFile));
  Check((LValue.Field('kind').AsText = 'isolated-callback-workshop') and
    (LValue.Field('origin').AsText = COrigin), 'explicit restoration ownership');
  ForceDirectories(GEvidence);
  GClient := TNyxMCPTestClient.Create(LRuntime + '.codex/config.toml',
    'Scooty owned callback restoration');
  LInitial := ReadText(LRuntime + 'initial-project.nyxpair');
  Refresh;
  GBrowser := TNyxBrowserPipe.Create(COrigin + '/', GEvidence + 'observer', 1280, 960);
  WaitFace('[data-node="action-code"]');
  for LIndex := 1 to 30 do
  begin
    Pump;
  end;

  if GBrowser.Exists('[data-node="action-agent-accept"]') then
  begin
    GBrowser.Click('[data-node="action-agent-accept"]');
  end;
  GBrowser.Click('[data-node="action-code"]');
  WaitFace('[data-node="action-reset-source"]');
  GBrowser.Click('[data-node="action-reset-source"]');
  LStarted := GetTickCount64;
  repeat
    LValue := Call('nyx_session', NyxObject([]));

    if not LValue.Field('pendingDraft').AsBoolean then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      raise Exception.Create('Owned draft restoration was not acknowledged');
    end;
    Pump;
  until False;
  Refresh;
  for LIndex := 1 to 8 do
  begin

    if Pair = LInitial then
    begin
      Break;
    end;
    Call('nyx_history', NyxObject([NyxField('direction', NyxData('undo')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation))]));
    Refresh;
  end;
  Check(Pair = LInitial, 'ordinary restoration/semantic Undo retain the exact owned seed');
  WaitSource(DecodeNyxProject(LInitial).Source);
  GBrowser.Capture('owned-restored');
  WriteLn('PASS owned draft restoration and exact paired semantic Undo; history retained');
end;

begin
  try

    if (ParamStr(1) = 'seed') or (ParamStr(1) = 'repository-seed') then
    begin
      Seed;
    end
    else if ParamStr(1) = 'run' then
    begin
      Run;
    end
    else if ParamStr(1) = 'restore' then
    begin
      Restore;
    end
    else
    begin
      raise Exception.Create('Choose seed, repository-seed, run or restore; none starts a listener');
    end;
  except
    on LException: Exception do
    begin

      if GBrowser <> nil then
      begin
        GBrowser.Capture('journey-failed');
      end;
      WriteLn(StdErr, 'FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  GBrowser.Free;

  if GClient <> nil then
  begin
    GClient.Close;
    GClient.Free;
  end;
end.
