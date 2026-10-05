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
program nyx_mcp_review_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, fphttpclient, base64, nyx.text, nyx.data,
  nyx.studio.agents, nyx.studio.session, nyx.studio.projects,
  nyx.studio.reviews, nyx.studio.buildjobs, nyx.test.mcp.client,
  nyx.test.browser.host;

type
  { Actual Nyx Studio input/observation consumer. Only the user's initial typing
    and observing controls use host input. Review authorship, source and builds
    use bounded semantic MCP operations, never injected browser scripts. }
  TReviewBrowser = class(TNyxBrowserHost)
  public
    function Identity(const ASelector: TNyxText): Integer;
    function Value(const ASelector: TNyxText): TNyxText;
    function Text(const ASelector: TNyxText): TNyxText;
    procedure Click(const AID: TNyxText);
    procedure AppendDraft;
    procedure WaitText(const ASelector, AText: TNyxText);
    procedure WaitRevision(ARevision: Integer);
    procedure WaitRetired;
    procedure Capture;
  end;

var
  GClient: TNyxMCPTestClient;
  GOther: TNyxMCPTestClient;
  GObserver: TReviewBrowser;
  GWatcher: TReviewBrowser;
  GBase: TNyxText;
  GDirectory: TNyxText;
  GToken: TNyxText;
  GReview: TNyxReviewRef;
  GRevision: Integer;
  GChecks: Integer;
  GBaseline: TNyxDataValue;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function TReviewBrowser.Identity(const ASelector: TNyxText): Integer;
begin
  Result := Node(ASelector);
end;

function TReviewBrowser.Value(const ASelector: TNyxText): TNyxText;
begin
  Result := Command('Accessibility.getPartialAXTree', NyxObject([
    NyxField('nodeId', NyxData(Node(ASelector))),
    NyxField('fetchRelatives', NyxData(False))])).Field('nodes').Item(0)
    .Field('value').Field('value').AsText;
end;

function TReviewBrowser.Text(const ASelector: TNyxText): TNyxText;
begin
  Result := Command('DOM.getOuterHTML', NyxObject([
    NyxField('nodeId', NyxData(Node(ASelector)))])).Field('outerHTML').AsText;
end;

procedure TReviewBrowser.Click(const AID: TNyxText);
var
  LNode: Integer;
  LQuad: TNyxDataValue;
  LX: Double;
  LY: Double;
begin
  LNode := Node('[data-node="' + AID + '"]');
  Command('DOM.scrollIntoViewIfNeeded', NyxObject([NyxField('nodeId', NyxData(LNode))]));
  LQuad := Command('DOM.getBoxModel', NyxObject([
    NyxField('nodeId', NyxData(LNode))])).Field('model').Field('content');
  LX := (LQuad.Item(0).AsNumber + LQuad.Item(2).AsNumber) / 2;
  LY := (LQuad.Item(1).AsNumber + LQuad.Item(5).AsNumber) / 2;
  Command('Input.dispatchMouseEvent', NyxObject([NyxField('type', NyxData('mousePressed')),
    NyxField('x', NyxData(LX)), NyxField('y', NyxData(LY)),
    NyxField('button', NyxData('left')), NyxField('clickCount', NyxData(1))]));
  Command('Input.dispatchMouseEvent', NyxObject([NyxField('type', NyxData('mouseReleased')),
    NyxField('x', NyxData(LX)), NyxField('y', NyxData(LY)),
    NyxField('button', NyxData('left')), NyxField('clickCount', NyxData(1))]));
end;

procedure TReviewBrowser.AppendDraft;
const
  CDraft: TNyxText = #10 + '// User draft stays here 🌙漢字';
begin
  Command('DOM.focus', NyxObject([NyxField('nodeId', NyxData(Node('textarea[data-node="studio-code"]')))]));
  Command('Input.dispatchKeyEvent', NyxObject([NyxField('type', NyxData('keyDown')),
    NyxField('key', NyxData('End')), NyxField('code', NyxData('End')),
    NyxField('windowsVirtualKeyCode', NyxData(35)), NyxField('modifiers', NyxData(2))]));
  Command('Input.dispatchKeyEvent', NyxObject([NyxField('type', NyxData('keyUp')),
    NyxField('key', NyxData('End')), NyxField('code', NyxData('End')),
    NyxField('windowsVirtualKeyCode', NyxData(35)), NyxField('modifiers', NyxData(2))]));
  Command('Input.insertText', NyxObject([NyxField('text', NyxData(CDraft))]));
end;

procedure TReviewBrowser.WaitText(const ASelector, AText: TNyxText);
var
  LStarted: QWord;
  LReady: Boolean;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;
    LReady := False;
    try
      LReady := Pos(AText, Text(ASelector)) > 0;
    except
      on Exception do
      begin
        { The admitted observing control can appear on a subsequent refresh. }
      end;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Observing control did not reach its expected state: ' + ASelector);
    end;
    Sleep(20);
  until LReady;
  Check(True, 'Actual Nyx observing control: ' + ASelector);
end;

procedure TReviewBrowser.WaitRevision(ARevision: Integer);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Live review did not reach its expected revision');
    end;
    Sleep(20);
  until (Attribute('data-nyx-review-ready') = 'true') and
    (Attribute('data-nyx-review-revision') = IntToStr(ARevision));
  Check(True, 'Live rendered review reaches revision ' + IntToStr(ARevision));
end;

procedure TReviewBrowser.WaitRetired;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Live review did not observe retirement');
    end;
    Sleep(20);
  until Attribute('data-nyx-review-ready') = 'retired';
  Check(True, 'Retired observing view terminates without falling back to user work');
end;

procedure TReviewBrowser.Capture;
begin
  Save('preview.png', DecodeStringBase64(Command('Page.captureScreenshot', NyxObject([
    NyxField('format', NyxData('png')), NyxField('captureBeyondViewport', NyxData(False))]))
    .Field('data').AsText));
end;

function Active: TNyxDataValue;
begin
  Result := NyxTestEditorExchange(GBase, '/api/agents', GToken,
    NyxObject([NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
end;

procedure Preserved;
var
  LNow: TNyxDataValue;
  LIndex: Integer;
const
  CKeys: array[0..8] of TNyxText = ('revision', 'title', 'selection', 'view',
    'pages', 'components', 'pendingDraft', 'canUndo', 'canRedo');
begin
  LNow := Active;
  Check(LNow.Field('project').AsText = GBaseline.Field('project').AsText,
    'Real protocol retains exact active accepted/draft/base bytes');
  for LIndex := 0 to High(CKeys) do
  begin
    Check(LNow.Field('session').Field(CKeys[LIndex]).ToJSON =
      GBaseline.Field('session').Field(CKeys[LIndex]).ToJSON,
      'Real protocol retains active ' + CKeys[LIndex]);
  end;
  Check(LNow.Field('compiler').ToJSON = GBaseline.Field('compiler').ToJSON,
    'Review compiler reports never replace active user diagnostics');
end;

function Call(const ATool: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
var
  LReply: TNyxDataValue;
begin
  LReply := GClient.Tool(ATool, NyxWithReview(AArguments, GReview));

  if LReply.Field('isError').AsBoolean then
  begin
    raise Exception.Create('Semantic review request refused: ' + LReply.ToJSON);
  end;
  Result := LReply.Field('structuredContent');

  if NyxAgentHas(Result, 'revision') then
  begin
    GRevision := Result.Field('revision').AsInteger;
  end;

  if GReview.ID <> '' then
  begin
    Check(Result.Field('review').AsText = GReview.ID, 'Response carries the exact review context');
  end;
end;

procedure Refuse(AClient: TNyxMCPTestClient; const ATool: TNyxText;
  const AArguments: TNyxDataValue);
begin
  Check(AClient.Tool(ATool, AArguments).Field('isError').AsBoolean,
    'Foreign, stale or invalid semantic request refuses');
  Preserved;
end;

function Lifecycle(const AMode, AOperation: TNyxText): TNyxDataValue;
begin

  if AMode = 'create' then
  begin
    Result := NyxObject([NyxField('mode', NyxData(AMode)),
      NyxField('operationId', NyxData(AOperation)),
      NyxField('expectedRevision', GBaseline.Field('session').Field('revision')),
      NyxField('base', NyxData('empty')), NyxField('label', NyxData(TNyxText('Moonlit review 🌙')))]);
  end
  else
  begin
    Result := NyxObject([NyxField('mode', NyxData(AMode)),
      NyxField('operationId', NyxData(AOperation)), NyxField('review', NyxData(GReview.ID)),
      NyxField('expectedRevision', NyxData(GRevision))]);
  end;
end;

function Fetch(const APath: TNyxText; AStatus: Integer = 200): TNyxText;
var
  LHTTP: TFPHTTPClient;
  LStream: TMemoryStream;
begin
  LHTTP := TFPHTTPClient.Create(nil);
  LStream := TMemoryStream.Create;
  try
    LHTTP.IOTimeout := 10000;
    LHTTP.HTTPMethod('GET', GBase + '/' + APath, LStream, [200, 400, 404]);
    Check(LHTTP.ResponseStatusCode = AStatus, 'Read-only review endpoint returns its exact lifecycle status');
    SetLength(Result, LStream.Size);
    SetCodePage(RawByteString(Result), CP_UTF8, False);
    LStream.Position := 0;

    if LStream.Size > 0 then
    begin
      LStream.ReadBuffer(Result[1], LStream.Size);
    end;
  finally
    LHTTP.Free;
    LStream.Free;
  end;
end;

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

function ExportSource: TNyxText;
var
  LLines: TNyxStrings;
  LValue: TNyxDataValue;
  LLine: Integer;
  LIndex: Integer;
  LRevision: Integer;
begin
  LLines := TNyxStrings.Create;
  try
    LLine := 1;
    LRevision := GRevision;
    repeat
      LValue := Call('nyx_source', NyxObject([
        NyxField('line', NyxData(LLine)), NyxField('count', NyxData(80))]));
      Check(GRevision = LRevision, 'Bounded source export remains at one revision');
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

function Build(const ATarget, AScope, AOutput: TNyxText): TNyxDataValue;
var
  LArguments: TNyxDataValue;
  LJob: TNyxText;
  LStarted: QWord;
begin
  LArguments := NyxObject([NyxField('mode', NyxData('request')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('review-' + ATarget + '-' + AScope)),
    NyxField('target', NyxData(ATarget)), NyxField('scope', NyxData(AScope)),
    NyxField('outputID', NyxData(AOutput))]);

  if AScope <> 'application' then
  begin
    LArguments := TNyxDataValue.ParseJSON(Copy(LArguments.ToJSON, 1,
      Length(LArguments.ToJSON) - 1) + ',"view":"review-workshop"}');
  end;
  LJob := Call('nyx_build', LArguments).Field('job').AsText;
  Check(Call('nyx_build', LArguments).Field('job').AsText = LJob,
    'Exact review build retry returns its original immutable job');
  Refuse(GClient, 'nyx_build', NyxObject([NyxField('mode', NyxData('status')),
    NyxField('job', NyxData(LJob))]));
  LStarted := GetTickCount64;
  repeat
    Result := Call('nyx_build', NyxObject([NyxField('mode', NyxData('status')),
      NyxField('job', NyxData(LJob)), NyxField('severity', NyxData('error')),
      NyxField('limit', NyxData(3))]));

    if GetTickCount64 - LStarted > 90000 then
    begin
      raise Exception.Create('Review compiler job exceeded its qualification budget');
    end;
    Sleep(100);
  until Result.Field('state').AsText <> 'running';
  Check((Result.Field('state').AsText = 'succeeded') and
    Result.Field('currentSource').AsBoolean, 'Actual compiler succeeds in the independent review context');
  Save('build-' + ATarget + '-' + AScope + '.json', Result.ToJSON);
  WriteLn('Qualified review ', ATarget, '/', AScope);
  Flush(Output);
end;

procedure Run;
var
  LStudio: TNyxStudioSession;
  LValue: TNyxDataValue;
  LArguments: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LHandler: TNyxText;
  LOriginalBody: TNyxText;
  LImplementation: TNyxText;
  LSource: TNyxText;
  LOutput: TNyxText;
  LPreview: TNyxText;
  LDraft: TNyxText;
  LCodeIdentity: Integer;
  LStarted: QWord;
  LStatus: TNyxDataValue;
  LIndex: Integer;
  LPreviewReply: TNyxDataValue;
  LRetired: TNyxReviewRef;
const
  CCode: TNyxText = 'textarea[data-node="studio-code"]';
begin
  GBase := ParamStr(1);
  GDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));
  { This harness is explicitly for an independently staged service. Never give
    it a user's live production endpoint: its first ordinary claim establishes
    the disposable user fixture before the actual Studio input journey. }

  if Pos('/build/review-workspaces/stage/.codex/',
    StringReplace(ExpandFileName(ParamStr(2)), '\', '/', [rfReplaceAll])) = 0 then
  begin
    raise Exception.Create('Use the independently owned review-workspaces stage configuration');
  end;
  LStudio := TNyxStudioSession.Create;
  try
    LValue := NyxTestEditorExchange(GBase, '/api/agents/connect', '', NyxObject([
      NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(LStudio.ProjectSnapshot))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GToken := LValue.Field('token').AsText;
  finally
    LStudio.Free;
  end;
  GClient := TNyxMCPTestClient.Create(ParamStr(2), 'Same friendly actor');
  GOther := TNyxMCPTestClient.Create(ParamStr(2), 'Same friendly actor');
  LValue := GClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools');
  Check((LValue.Count = 16) and (LValue.Item(15).Field('name').AsText = 'nyx_reviews'),
    'Actual authenticated MCP discovers the review lifecycle tool');
  for LIndex := 0 to 14 do
  begin
    Check(Pos('"review"', LValue.Item(LIndex).Field('inputSchema').ToJSON) > 0,
      'Each existing tool advertises explicit review routing');
  end;
  GObserver := TReviewBrowser.Create(GBase + '/', GDirectory + 'studio');
  GObserver.Click('action-agents');
  GObserver.WaitText('[data-node="studio-agents-status"]', 'Agents edit');
  GObserver.Click('action-code');
  GObserver.AppendDraft;
  LStarted := GetTickCount64;
  repeat
    GBaseline := Active;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('The actual Studio draft was not acknowledged');
    end;
    Sleep(100);
  until GBaseline.Field('session').Field('pendingDraft').AsBoolean;
  LDraft := GObserver.Value(CCode);
  LCodeIdentity := GObserver.Identity(CCode);
  Save('user-baseline.json', GBaseline.ToJSON);
  Check(Pos(TNyxText('User draft stays here 🌙漢字'), LDraft) > 0,
    'The actual Studio has an independent supplementary Unicode draft');
  LArguments := Lifecycle('create', 'new-empty-review');
  LReceipt := Call('nyx_reviews', LArguments);
  GReview := NyxReview(LReceipt.Field('review').AsText);
  GRevision := LReceipt.Field('session').Field('revision').AsInteger;
  Check(GClient.Tool('nyx_reviews', LArguments).Field('structuredContent').ToJSON = LReceipt.ToJSON,
    'Exact real create retry returns the original workspace');
  Refuse(GOther, 'nyx_session', NyxWithReview(NyxObject([]), GReview));
  Check(GOther.Tool('nyx_reviews', NyxObject([NyxField('mode', NyxData('list'))]))
    .Field('structuredContent').Field('total').AsInteger = 0,
    'Identical friendly names do not share transport ownership');
  Call('nyx_transaction', NyxObject([NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('compose-review')),
    NyxField('operations', TNyxDataValue.ParseJSON(
      '[{"op":"create","kind":"page","id":"review-workshop","root":"page","properties":{"gap":16,"padding":24}},' +
      '{"op":"create","kind":"heading","id":"review-title","parent":"review-workshop","properties":{"text":"Moonlit workshop 🌙漢字"}},' +
      '{"op":"create","kind":"input","id":"review-quantity","parent":"review-workshop","properties":{"text":"Quantity","value":"","placeholder":"Digits only"}},' +
      '{"op":"create","kind":"button","id":"review-apply","parent":"review-workshop","properties":{"text":"Apply"}}]'))]));
  Call('nyx_select', NyxObject([NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('activate-review')),
    NyxField('id', NyxData('review-workshop')), NyxField('activate', NyxData(True))]));
  GObserver.WaitText('[data-node="studio-agent-review-owner-0"]', 'revision ' + IntToStr(GRevision));
  Check((GObserver.Identity(CCode) = LCodeIdentity) and (GObserver.Value(CCode) = LDraft),
    'Review observation preserves the actual Studio code control and exact unsent draft');
  LValue := Active.Field('reviews').Item(0);
  LPreview := LValue.Field('preview').AsText;
  GWatcher := TReviewBrowser.Create(GBase + '/' + LPreview, GDirectory + 'watcher');
  GWatcher.WaitRevision(GRevision);
  GWatcher.WaitText('[data-node="review-title"]', 'Moonlit workshop');
  Check(TNyxDataValue.ParseJSON(Fetch(StringReplace(LPreview, 'agent-review.html?',
    'api/agents/review?', []) + '&after=' + IntToStr(GRevision))).Count = 2,
    'Unchanged live observations contain only revision/context metadata');
  LValue := Call('nyx_callbacks', NyxObject([NyxField('mode', NyxData('apply')),
    NyxField('operationId', NyxData('review-validator')),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('changes',
    TNyxDataValue.ParseJSON('[{"op":"add","id":"review-quantity","event":{"trigger":"before-text-input"}}]'))]));
  LHandler := LValue.Field('callbacks').Item(0).Field('handler').AsText;
  LOriginalBody := Call('nyx_pascal', NyxObject([NyxField('mode', NyxData('inspect')),
    NyxField('handler', NyxData(LHandler)), NyxField('count', NyxData(2048))])).Field('text').AsText;
  LImplementation := #10 + 'var' + #10 + '  LIndex: Integer;' + #10 + 'begin' + #10 +
    '  // Keep the complete proposed quantity numeric.' + #10 + #10 +
    '  if AExecution.Cancelled or not AEvent.HasTextEdit then' + #10 +
    '  begin' + #10 + '    Exit;' + #10 + '  end;' + #10 +
    '  for LIndex := 1 to Length(AEvent.TextEdit.After) do' + #10 +
    '  begin' + #10 + #10 +
    '    if (AEvent.TextEdit.After[LIndex] < ''0'') or (AEvent.TextEdit.After[LIndex] > ''9'') then' + #10 +
    '    begin' + #10 + '      NyxEventResponse(AExecution).Consume;' + #10 +
    '      Exit;' + #10 + '    end;' + #10 + '  end;' + #10 + 'end;';
  Call('nyx_pascal', NyxObject([NyxField('mode', NyxData('apply')),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('implement-validator')),
    NyxField('changes', NyxArray([NyxObject([NyxField('handler', NyxData(LHandler)),
      NyxField('expected', NyxData(LOriginalBody)), NyxField('implementation', NyxData(LImplementation))])]))]));
  LSource := ExportSource;
  Call('nyx_history', NyxObject([NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('undo-validator')), NyxField('direction', NyxData('undo'))]));
  Check(ExportSource <> LSource, 'Paired review Undo restores its original TODO implementation');
  Call('nyx_history', NyxObject([NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('redo-validator')), NyxField('direction', NyxData('redo'))]));
  Check(ExportSource = LSource, 'Review Redo restores exact compiled source');
  GWatcher.WaitRevision(GRevision);
  GWatcher.Capture;
  GObserver.WaitText('[data-node="studio-agents-activity"]', 'nyx_pascal');
  Check((GObserver.Identity(CCode) = LCodeIdentity) and (GObserver.Value(CCode) = LDraft),
    'Callback/source/history observation retains the real user editor');
  GObserver.Capture;
  Preserved;
  Save('source/nyx.generated.view.pas', LSource);
  LOutput := Call('nyx_build', NyxObject([NyxField('mode', NyxData('outputs'))])).Field('outputID').AsText;
  LStatus := Build('browser', 'application', LOutput);
  Check(Fetch(LStatus.Field('compiledSource').AsText) = LSource,
    'Actual pas2js application compiler receives exact MCP-exported source');
  LStatus := Build('lcl', 'application', LOutput);
  Check(Fetch(LStatus.Field('compiledSource').AsText) = LSource,
    'Actual LCL application compiler receives exact MCP-exported source');
  Build('browser', 'view', LOutput);
  Build('lcl', 'view', LOutput);
  Preserved;
  FreeAndNil(GObserver);
  FreeAndNil(GWatcher);
  { Selective screenshot validation runs sequentially, outside concurrent host
    captures. The image and immutable packet carry the same review/revision. }
  LPreviewReply := GClient.Tool('nyx_preview', NyxWithReview(NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('view', NyxData('review-workshop')),
    NyxField('width', NyxData(390)), NyxField('height', NyxData(844)),
    NyxField('capture', NyxData(True))]), GReview));
  Check(not LPreviewReply.Field('isError').AsBoolean and
    (LPreviewReply.Field('content').Count = 3), 'Selective real MCP preview supplies its rendered PNG');
  Save('semantic-preview.png', DecodeStringBase64(LPreviewReply.Field('content').Item(2).Field('data').AsText));
  Preserved;
  { Operator reductions control live reviews immediately and independently of
    their friendly display name. Re-enabling changes permission only. }
  NyxTestEditorExchange(GBase, '/api/agents', GToken,
    NyxObject([NyxField('op', NyxData('configure')), NyxField('permission', NyxData('readOnly'))]));
  Check(Call('nyx_session', NyxObject([])).Field('permission').AsText = 'readOnly',
    'Real review inherits read-only permission immediately');
  Refuse(GClient, 'nyx_transaction', NyxWithReview(NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('readonly-refusal')),
    NyxField('operations', TNyxDataValue.ParseJSON('[{"op":"title","value":"Refuse"}]'))]), GReview));
  NyxTestEditorExchange(GBase, '/api/agents', GToken,
    NyxObject([NyxField('op', NyxData('configure')), NyxField('permission', NyxData('disabled'))]));
  Refuse(GClient, 'nyx_session', NyxWithReview(NyxObject([]), GReview));
  NyxTestEditorExchange(GBase, '/api/agents', GToken,
    NyxObject([NyxField('op', NyxData('configure')), NyxField('permission', NyxData('edit'))]));
  GWatcher := TReviewBrowser.Create(GBase + '/' + LPreview, GDirectory + 'retirement');
  GWatcher.WaitRevision(GRevision);
  LArguments := Lifecycle('discard', 'retire-review');
  LReceipt := GClient.Tool('nyx_reviews', LArguments).Field('structuredContent');
  Check(LReceipt.Field('disposed').AsBoolean, 'Exact revision disposal retires the owned review');
  Check(GClient.Tool('nyx_reviews', LArguments).Field('structuredContent').ToJSON = LReceipt.ToJSON,
    'Exact disposal retry retains its receipt');
  GWatcher.WaitRetired;
  Fetch(StringReplace(LPreview, 'agent-review.html?', 'api/agents/review?', []), 404);
  LRetired := GReview;
  Refuse(GClient, 'nyx_session', NyxWithReview(NyxObject([]), LRetired));
  GReview := NyxActiveWorkspace;
  LValue := Call('nyx_reviews', Lifecycle('create', 'disconnect-owned-review'));
  GReview := NyxReview(LValue.Field('review').AsText);
  GClient.Close;
  Check(Active.Field('reviews').Count = 0, 'Authenticated transport teardown retires its ephemeral work');
  Preserved;
  Save('user-after.json', Active.ToJSON);
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' real review workflow checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  GWatcher.Free;
  GObserver.Free;

  if GClient <> nil then
  begin
    GClient.Close;
  end;

  if GOther <> nil then
  begin
    GOther.Close;
  end;
  GClient.Free;
  GOther.Free;
end.
