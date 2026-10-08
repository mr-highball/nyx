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


program nyx_compiled_studio_lifetime;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.data, nyx.model, nyx.controls,
  nyx.codec, nyx.codegen, nyx.studio.projects, nyx.studio.outputs,
  nyx.test.mcp.client, nyx.test.browser.pipe;

const
  CFrame = '.nyx-compiled-preview';
  CMemo = '[data-node="notes"] textarea';

var
  GClient: TNyxMCPTestClient;
  GBrowser: TNyxBrowserPipe;
  GBase: TNyxText;
  GToken: TNyxText;
  GRevision: Integer;
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function ReadText(const APath: String; ALimit: Integer): TNyxText;
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try

    if (LFile.Size < 1) or (LFile.Size > ALimit) then
    begin
      raise Exception.Create('Qualification input exceeds its byte bound');
    end;
    SetLength(Result, LFile.Size);
    LFile.ReadBuffer(Result[1], Length(Result));
  finally
    LFile.Free;
  end;
end;

function Call(const ATool: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
var
  LReply: TNyxDataValue;
begin
  LReply := GClient.Tool(ATool, AArguments);

  if LReply.Field('isError').AsBoolean then
  begin
    raise Exception.Create('Semantic qualification operation refused: ' + ATool);
  end;
  Result := LReply.Field('structuredContent');
end;

procedure Pump(AMilliseconds: Integer);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    GBrowser.Attribute('data-nyx-ready');

    if GBrowser.RuntimeError <> '' then
    begin
      GBrowser.CaptureRuntimeError;
      raise Exception.Create('Studio runtime failed; inspect the private debugger receipt');
    end;
    Sleep(25);
  until GetTickCount64 - LStarted >= QWord(AMilliseconds);
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

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Ordinary Studio face did not become ready: ' + ASelector);
    end;
    Pump(50);
  until False;
end;

function WaitCompiled: Integer;
var
  LStarted: QWord;
  LValue: TNyxText;
begin
  LStarted := GetTickCount64;
  repeat

    if GBrowser.Exists(CFrame) then
    begin
      Result := GBrowser.FrameDocumentIdentity(CFrame);

      if (Result <> 0) and GBrowser.TryFieldValue(CMemo, LValue, CFrame) then
      begin
        Exit;
      end;
    end;

    if GetTickCount64 - LStarted > 60000 then
    begin
      GBrowser.Capture('failed-compiled-readiness');
      raise Exception.Create('Ordinary Studio did not mount its compiled memo');
    end;
    Pump(100);
  until False;
end;

procedure Retained(AIdentity: Integer; const AText, AReason: TNyxText);
var
  LValue: TNyxText;
begin
  Pump(400);
  Check(GBrowser.FrameDocumentIdentity(CFrame) = AIdentity,
    'Compiled document changed during ' + AReason);
  Check(GBrowser.TryFieldValue(CMemo, LValue, CFrame) and (LValue = AText),
    'Compiled input changed during ' + AReason);
end;

function Runs: TNyxDataValue;
begin
  Result := Call('nyx_resources', NyxObject([NyxField('mode', NyxData('runtimes')),
    NyxField('expectedRevision', NyxData(GRevision))])).Field('items');
end;

var
  LRoot: String;
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
  LReply: TNyxDataValue;
  LTools: TNyxDataValue;
  LOutputs: TNyxOutputConfiguration;
  LIdentity: Integer;
  LNextIdentity: Integer;
  LStarted: QWord;
  LValue: TNyxText;
  LRun: TNyxText;
  LItems: TNyxDataValue;
begin
  LDocument := nil;
  LOutputs := nil;
  GClient := nil;
  GBrowser := nil;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply isolated origin, owned runtime home and local toolchain JSON');
    end;
    GBase := ParamStr(1);
    LRoot := IncludeTrailingPathDelimiter(ParamStr(2));
    { The fixture refuses ordinary user workspaces before any claim or mutation.
      Only the isolated server writes this bounded exact-origin marker. }
    LReply := TNyxDataValue.ParseJSON(ReadText(LRoot +
      '.local/runtime-qualification.json', 1024));
    Check((LReply.Field('version').AsInteger = 1) and
      (LReply.Field('service').AsText = 'nyx-resource-runtime-qualification') and
      (LReply.Field('origin').AsText = GBase), 'Isolated host ownership marker');

    LDocument := TNyxDocument.Create;
    LDocument.AddPage(NewNyxColumn('home').Node);
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    LReply := NyxTestEditorExchange(GBase, '/api/agents/connect', '', NyxObject([
      NyxField('op', NyxData('claim')), NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GToken := LReply.Field('token').AsText;
    GRevision := LReply.Field('state').Field('session').Field('revision').AsInteger;
    LReply := NyxTestEditorExchange(GBase, '/api/agents', GToken, NyxObject([
      NyxField('op', NyxData('commit')), NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GRevision := LReply.Field('session').Field('revision').AsInteger;
    GClient := TNyxMCPTestClient.Create(LRoot + '.codex/config.toml',
      'Scooty ordinary compiled Studio qualification');
    { All design composition and accepted edits use one revision-aware semantic
      group. Browser input below exercises operator launch and physical lifetime,
      because MCP has no observing-editor adopt/launch operation yet. }
    LReply := Call('nyx_transaction', NyxObject([
      NyxField('operationId', NyxData('compiled-studio-controls')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operations', TNyxDataValue.ParseJSON(
        '[{"op":"create","kind":"memo","id":"notes","parent":"home","properties":{"text":"Workshop notes","value":"","placeholder":"Try a thought"}},' +
        '{"op":"create","kind":"label","id":"headline","parent":"home","properties":{"text":"Keep the workshop running"}}]'))]));
    GRevision := LReply.Field('revision').AsInteger;
    LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(3), 32768));
    LOutputs := TNyxOutputConfiguration.Create;
    LOutputs.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LOutputs.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LReply := NyxTestEditorExchange(GBase, '/api/agents', GToken, NyxObject([
      NyxField('op', NyxData('build')), NyxField('after', NyxData(0)),
      NyxField('build', NyxObject([NyxField('mode', NyxData('profile'))]))])).Field('buildReply');
    NyxTestEditorExchange(GBase, '/api/agents', GToken, NyxObject([
      NyxField('op', NyxData('build')), NyxField('after', NyxData(0)),
      NyxField('build', NyxObject([NyxField('mode', NyxData('profile')),
        NyxField('expectedOutputID', LReply.Field('outputID')),
        NyxField('profile', TNyxDataValue.ParseJSON(LOutputs.Encode))]))]));

    GBrowser := TNyxBrowserPipe.Create(GBase + '/', LRoot + 'browser', 1280, 960);
    WaitFace('[data-node="action-build-view"]');
    Pump(1000);

    if GBrowser.Exists('[data-node="action-agent-accept"]') then
    begin
      GBrowser.Click('[data-node="action-agent-accept"]');
      Pump(1000);
    end;
    GBrowser.Click('[data-node="action-outputs"]');
    WaitFace('[data-node="output-browser"]');
    GBrowser.Click('[data-node="output-browser"]');
    Pump(600);
    GBrowser.Click('[data-node="action-build-view"]');
    LIdentity := WaitCompiled;
    Check(LIdentity <> 0, 'Ordinary controller negotiates and mounts a compiled page');
    GBrowser.Click(CMemo, CFrame);
    GBrowser.TypeText('A thought worth keeping');
    Retained(LIdentity, 'A thought worth keeping', 'input and first observer refresh');
    LItems := Runs;
    Check((LItems.Count = 1) and LItems.Item(0).Field('active').AsBoolean,
      'Ordinary controller grants one active runtime reporter');
    LRun := LItems.Item(0).Field('run').AsText;
    Pump(6500);
    Retained(LIdentity, 'A thought worth keeping', 'ordinary polling and reporter heartbeat');
    Check(Runs.Item(0).Field('run').AsText = LRun, 'Polling retains the exact reporting run');

    GBrowser.Click('[data-node="action-code"]');
    Pump(500);
    WaitFace('[data-node="action-expand-source"]');
    GBrowser.Click('[data-node="action-expand-source"]');
    Retained(LIdentity, 'A thought worth keeping', 'source modal expansion');
    GBrowser.Click('[data-node="action-expand-source"]');
    Retained(LIdentity, 'A thought worth keeping', 'source modal close');
    GBrowser.Resize(390, 844);
    Pump(700);
    GBrowser.Capture('studio-narrow-compiled');
    Retained(LIdentity, 'A thought worth keeping', 'compact allocation');
    GBrowser.Click('[data-node="action-panel-project"]');
    Retained(LIdentity, 'A thought worth keeping', 'hidden compact Project panel');
    GBrowser.Capture('studio-narrow-project');
    GBrowser.Click('[data-node="action-panel-inspector"]');
    Retained(LIdentity, 'A thought worth keeping', 'hidden compact Inspector panel');
    GBrowser.Click('[data-node="action-panel-design"]');
    Retained(LIdentity, 'A thought worth keeping', 'compact Design return');
    GBrowser.Resize(1280, 960);
    Retained(LIdentity, 'A thought worth keeping', 'desktop return');
    GBrowser.Click(CMemo, CFrame);
    GBrowser.Key(nbkEnd, True);
    GBrowser.TypeText(' for tomorrow');
    Retained(LIdentity, 'A thought worth keeping for tomorrow', 'resumed physical editing');
    LItems := Runs;
    Check(LItems.Item(0).Field('active').AsBoolean and
      (LItems.Item(0).Field('run').AsText = LRun),
      'Returning to the visible preview retains active producer authority');
    GBrowser.Capture('studio-desktop-compiled');

    { A real accepted paired edit retires the old browsing context and grant.
      It must not retain an old program just because its active root is equal. }
    LReply := Call('nyx_transaction', NyxObject([
      NyxField('operationId', NyxData('compiled-studio-edit')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operations', TNyxDataValue.ParseJSON(
        '[{"op":"update","id":"headline","properties":{"text":"A new accepted design"}}]'))]));
    GRevision := LReply.Field('revision').AsInteger;
    LStarted := GetTickCount64;
    repeat
      Pump(100);

      if GetTickCount64 - LStarted > 15000 then
      begin
        raise Exception.Create('Accepted paired edit did not retire the old compiled preview');
      end;
    until not GBrowser.Exists(CFrame);
    Check(Runs.Count = 0, 'Paired revision clears old runtime authority and observations');
    GBrowser.Click('[data-node="action-build-app"]');
    LNextIdentity := WaitCompiled;
    Check(LNextIdentity <> LIdentity, 'A new successful application build owns a new document');
    Check(GBrowser.TryFieldValue(CMemo, LValue, CFrame) and (LValue = ''),
      'New execution starts from authored defaults');
    Check(GBrowser.RuntimeError = '', 'Ordinary browser Studio completed without runtime exceptions');
    WriteLn('Ordinary compiled Studio lifetime: ', GChecks, ' checks passed');
  finally
    GBrowser.Free;

    if GClient <> nil then
    begin
      GClient.Close;
    end;
    GClient.Free;
    LOutputs.Free;
    LDocument.Free;
  end;
end.
