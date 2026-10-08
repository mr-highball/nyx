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



program nyx_resource_runtime_http;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Forms, Classes, SysUtils, fphttpclient,
  nyx.text, nyx.data, nyx.types, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.binding, nyx.binding.types, nyx.resources,
  nyx.resource.sources, nyx.studio.projects,
  nyx.studio.outputs, nyx.studio.resourceedits, nyx.studio.preview,
  nyx.studio.preview.lcl, nyx.test.mcp.client, nyx.test.browser.pipe;

type
  { Borrowed completion receiver remains alive until the preview cancels/joins.
    This harness owns only a fresh isolated project and its compiled children. }
  TPreparation = class
  public
    Finished: Boolean;
    Succeeded: Boolean;
    procedure Receive(ASucceeded: Boolean; const AError: TNyxText);
  end;

var
  GClient: TNyxMCPTestClient;
  GBase: TNyxText;
  GToken: TNyxText;
  GRevision: Integer;
  GChecks: Integer;

procedure TPreparation.Receive(ASucceeded: Boolean; const AError: TNyxText);
begin
  Finished := True;
  Succeeded := ASucceeded;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Call(const ATool: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
var
  LReply: TNyxDataValue;
begin
  LReply := GClient.Tool(ATool, AArguments);

  if LReply.Field('isError').AsBoolean then
  begin
    raise Exception.Create('Semantic operation refused: ' + ATool + ' / ' +
      LReply.Field('content').Item(0).Field('text').AsText);
  end;
  Result := LReply.Field('structuredContent');
end;

function EditorBuild(const ARequest: TNyxDataValue): TNyxDataValue;
begin
  Result := NyxTestEditorExchange(GBase, '/api/agents', GToken,
    NyxObject([NyxField('op', NyxData('build')), NyxField('after', NyxData(0)),
    NyxField('build', ARequest)])).Field('buildReply');
end;

function Reports: TNyxDataValue;
begin
  Result := Call('nyx_resources', NyxObject([NyxField('mode', NyxData('runtimes')),
    NyxField('expectedRevision', NyxData(GRevision))])).Field('items');
end;

function FindRun(const ARun: TNyxText): TNyxDataValue;
var
  LReports: TNyxDataValue;
  LIndex: Integer;
begin
  Result := NyxNull;
  LReports := Reports;
  for LIndex := 0 to LReports.Count - 1 do
  begin

    if LReports.Item(LIndex).Field('run').AsText = ARun then
    begin
      Exit(LReports.Item(LIndex));
    end;
  end;
end;

function WaitRuntime(const ARun: TNyxText; ABrowser: TNyxBrowserPipe = nil): TNyxDataValue;
var
  LStarted: QWord;
  LSummary: TNyxDataValue;
  LItems: TNyxDataValue;
  LItem: TNyxDataValue;
  LIndex: Integer;
  LReply: TNyxDataValue;
begin
  LStarted := GetTickCount64;
  LItems := NyxArray([]);
  repeat
    Application.ProcessMessages;

    if ABrowser <> nil then
    begin
      ABrowser.Attribute('data-nyx-ready');

      if ABrowser.RuntimeError <> '' then
      begin
        raise Exception.Create('Browser runtime exception: ' + ABrowser.RuntimeError);
      end;

      if ABrowser.Attribute('data-nyx-resource-reporting') = 'unavailable' then
      begin
        raise Exception.Create('Browser rejected its supplied private reporter context');
      end;
    end;
    LSummary := FindRun(ARun);

    if LSummary.Kind <> ndNull then
    begin
      LReply := GClient.Tool('nyx_resources', NyxObject([
        NyxField('mode', NyxData('runtime')), NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('run', NyxData(ARun)), NyxField('expectedSequence', LSummary.Field('sequence')),
        NyxField('limit', NyxData(2))]));
      LItems := NyxArray([]);

      if LReply.Field('isError').AsBoolean then
      begin
        { A real producer may install a new frame between summary and detail.
          Reacquire only this documented sequence conflict; other failures fail. }
        if TNyxDataValue.ParseJSON(LReply.Field('content').Item(0).Field('text').AsText)
          .Field('message').AsText <> 'Runtime observation changed; inspect its current sequence' then
        begin
          raise Exception.Create('Runtime detail refused outside its sequence guard');
        end;
      end
      else
      begin
        LItems := LReply.Field('structuredContent').Field('page').Field('items');
      end;
      for LIndex := 0 to LItems.Count - 1 do
      begin
        LItem := LItems.Item(LIndex);

        if (LItem.Field('name').AsText = 'health') and
          (LItem.Field('phase').AsText = 'ready') and
          (LItem.Field('publishedOrigin').AsText = 'network') then
        begin
          Check(LItem.Field('publishedCacheWrite').AsText = 'memory',
            'Actual hosted admission reports its successful memory storage');
          Check((LItems.Count = 2) and LSummary.Field('active').AsBoolean,
            'Actual application reports complete membership at the accepted revision');
          Exit(LSummary);
        end;
      end;
    end;

    if GetTickCount64 - LStarted > 20000 then
    begin
      raise Exception.Create('Automatic resource producer did not publish a hosted ready frame: ' +
        LSummary.ToJSON + ' / ' + LItems.ToJSON);
    end;
    Sleep(50);
  until False;
end;

function Compile(const ATarget, AScope, AView, AOperation: TNyxText): TNyxDataValue;
var
  LOutput: TNyxDataValue;
  LRequest: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStarted: QWord;
  LJob: TNyxText;
  LFields: array of TNyxDataField;
  LPublic: TNyxDataValue;
  LIndex: Integer;
begin
  LOutput := Call('nyx_build', NyxObject([NyxField('mode', NyxData('outputs'))]));
  SetLength(LFields, 6);
  LFields[0] := NyxField('mode', NyxData('request'));
  LFields[1] := NyxField('operationId', NyxData(AOperation));
  LFields[2] := NyxField('expectedRevision', NyxData(GRevision));
  LFields[3] := NyxField('outputID', LOutput.Field('outputID'));
  LFields[4] := NyxField('target', NyxData(ATarget));
  LFields[5] := NyxField('scope', NyxData(AScope));

  if AView <> '' then
  begin
    SetLength(LFields, 7);
    LFields[6] := NyxField('view', NyxData(AView));
  end;
  LRequest := NyxObject(LFields);
  LReceipt := Call('nyx_build', LRequest);
  LJob := LReceipt.Field('job').AsText;
  LStarted := GetTickCount64;
  repeat
    Result := Call('nyx_build', NyxObject([NyxField('mode', NyxData('status')),
      NyxField('job', NyxData(LJob)), NyxField('limit', NyxData(1))]));

    if Result.Field('state').AsText = 'succeeded' then
    begin
      Break;
    end;

    if (Result.Field('state').AsText = 'failed') or
      (GetTickCount64 - LStarted > 90000) then
    begin
      raise Exception.Create('Actual semantic compiler did not produce a successful current artifact');
    end;
    Application.ProcessMessages;
    Sleep(100);
  until False;
  Check(Result.Field('currentSource').AsBoolean and Result.Field('currentOutput').AsBoolean,
    'Actual compiler result retains accepted source and output identity');
  Result := EditorBuild(NyxObject([NyxField('mode', NyxData('preview')),
    NyxField('job', NyxData(LJob)), NyxField('expectedRevision', NyxData(GRevision))]));
  Check((Result.Field('state').AsText = 'succeeded') and
    (Result.Field('runtime').Field('token').AsText <> ''), 'Private editor admits exact successful launch');

  LPublic := Call('nyx_build', NyxObject([NyxField('mode', NyxData('status')),
    NyxField('job', NyxData(LJob)), NyxField('limit', NyxData(1))]));
  for LIndex := 0 to LPublic.Count - 1 do
  begin

    if LPublic.Key(LIndex) = 'runtime' then
    begin
      raise Exception.Create('Producer authority leaked into public compiler status');
    end;
  end;
  Check(True, 'Ordinary public status has no private runtime capability');
end;

function RuntimeStatus(const AToken, AOrigin: TNyxText; ASequence: Integer): Integer;
var
  LHTTP: TFPHTTPClient;
  LBody: TMemoryStream;
  LReply: TMemoryStream;
  LText: TNyxText;
begin
  LHTTP := TFPHTTPClient.Create(nil);
  LBody := TMemoryStream.Create;
  LReply := TMemoryStream.Create;
  try
    LHTTP.IOTimeout := 5000;
    LHTTP.AllowRedirect := False;
    LHTTP.AddHeader('Content-Type', 'application/json');
    LHTTP.AddHeader('X-Nyx-Runtime', AToken);

    if AOrigin <> '' then
    begin
      LHTTP.AddHeader('Origin', AOrigin);
    end;
    LText := NyxObject([NyxField('version', NyxData(1)),
      NyxField('operation', NyxData('heartbeat')), NyxField('sequence', NyxData(ASequence))]).ToJSON;
    LBody.WriteBuffer(LText[1], Length(LText));
    LBody.Position := 0;
    LHTTP.RequestBody := LBody;
    LHTTP.HTTPMethod('POST', GBase + '/api/resource-runtime', LReply, [200, 400, 403]);
    Result := LHTTP.ResponseStatusCode;
  finally
    LHTTP.Free;
    LReply.Free;
    LBody.Free;
  end;
end;

var
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
  LReply: TNyxDataValue;
  LTools: TNyxDataValue;
  LOutputs: TNyxOutputConfiguration;
  LFile: TFileStream;
  LText: TNyxText;
  LNative: TNyxLCLCompiledPreview;
  LPreparation: TPreparation;
  LBrowser: TNyxBrowserPipe;
  LCompiled: TNyxDataValue;
  LNativeGrant: TNyxDataValue;
  LBrowserGrant: TNyxDataValue;
  LSummary: TNyxDataValue;
  LStarted: QWord;
  LSequence: Integer;
  LIndex: Integer;
  LFound: Boolean;
  LScope: TNyxText;
  LView: TNyxText;
  LTarget: TNyxText;
  LScopeIndex: Integer;
  LTargetIndex: Integer;
  LAllRetired: Boolean;
  LBrowserArtifact: TNyxText;
begin
  GClient := nil;
  LDocument := nil;
  LOutputs := nil;
  LNative := nil;
  LPreparation := nil;
  LBrowser := nil;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply freshly owned loopback editor base, runtime home and installed toolchain file');
    end;
    GBase := ParamStr(1);
    { Refuse before any editor claim/commit. Only the Pascal isolated server
      writes this origin-bound marker; an ordinary user Studio has none. }
    LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(2)) +
      '.local/runtime-qualification.json', fmOpenRead or fmShareDenyNone);
    try

      if (LFile.Size < 1) or (LFile.Size > 1024) then
      begin
        raise Exception.Create('Isolated runtime marker requires bounded data');
      end;
      SetLength(LText, LFile.Size);
      LFile.ReadBuffer(LText[1], Length(LText));
    finally
      LFile.Free;
    end;
    LReply := TNyxDataValue.ParseJSON(LText);

    if (LReply.Field('version').AsInteger <> 1) or
      (LReply.Field('service').AsText <> 'nyx-resource-runtime-qualification') or
      (LReply.Field('origin').AsText <> GBase) then
    begin
      raise Exception.Create('Qualification refuses a normal or different Studio');
    end;
    Application.Initialize;
    LDocument := TNyxDocument.Create;
    LDocument.AddPage(NewNyxColumn('home').Node);
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    LReply := NyxTestEditorExchange(GBase, '/api/agents/connect', '', NyxObject([
      NyxField('op', NyxData('claim')), NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GToken := LReply.Field('token').AsText;
    GRevision := LReply.Field('state').Field('session').Field('revision').AsInteger;
    Check(LReply.Field('state').Field('resourceRuntimeReporting').AsBoolean,
      'Actual observing protocol negotiates runtime reporting');
    { This executable is admitted only against its freshly owned isolated test
      server. An explicit paired commit resets a previous failed test there;
      claim alone correctly preserves any already accepted project. }
    LReply := NyxTestEditorExchange(GBase, '/api/agents', GToken, NyxObject([
      NyxField('op', NyxData('commit')), NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GRevision := LReply.Field('session').Field('revision').AsInteger;
    GClient := TNyxMCPTestClient.Create(IncludeTrailingPathDelimiter(ParamStr(2)) +
      '.codex/config.toml', 'Scooty resource runtime qualification');
    LReply := GClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools');
    LFound := False;
    for LIndex := 0 to LReply.Count - 1 do
    begin
      LFound := LFound or (LReply.Item(LIndex).Field('name').AsText = 'nyx_resources');
    end;
    Check(LFound, 'Authenticated current-source MCP discovers semantic resources');

    { Compose through semantic contracts; the editor route supplies only the
      isolated initial project and machine-local compiler configuration. }
    LReply := Call('nyx_transaction', NyxObject([
      NyxField('operationId', NyxData('runtime-controls')), NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operations', TNyxDataValue.ParseJSON(
        '[{"op":"create","kind":"label","id":"headline","parent":"home","properties":{"text":"Starting"}},' +
        '{"op":"create","kind":"card","id":"workshop","root":"component"},' +
        '{"op":"create","kind":"label","id":"component-title","parent":"workshop","properties":{"text":"Ready to make something"}}]'))]));
    GRevision := LReply.Field('revision').AsInteger;
    LReply := Call('nyx_resources', NyxObject([
      NyxField('mode', NyxData('apply')), NyxField('operationId', NyxData('runtime-resources')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('changes', NyxResourcePatch([
        NyxDefineResource(NyxResourceRef('copy'), NyxDefaultLocale, NyxTextResource('Ready to make something')),
        NyxDefineResource(NyxResourceRef('health'), NyxDefaultLocale,
          NyxHostedResource(nrkJSON, NyxResourceURL(GBase + '/api/health'))
            .Cache(NyxResourceCache.Memory.FreshFor(120).ServerPolicy(rcspOverride))
            .Fallback(NyxJSONResource('{"service":"Starting"}'))),
        NyxBindResource(NyxControl('headline'), bpText,
          NyxResourceValue(NyxResourceRef('health')).Field('service'))]).ToData)]));
    GRevision := LReply.Field('revision').AsInteger;
    Check(Reports.Count = 0, 'Declarations alone do not invent runtime observations');

    LFile := TFileStream.Create(ParamStr(3), fmOpenRead or fmShareDenyNone);
    try
      SetLength(LText, LFile.Size);
      LFile.ReadBuffer(LText[1], Length(LText));
    finally
      LFile.Free;
    end;
    LTools := TNyxDataValue.ParseJSON(LText);
    LOutputs := TNyxOutputConfiguration.Create;
    LOutputs.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LOutputs.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LOutputs.SetField('fpc', LTools.Field('LCL_FPC').AsText);
    LOutputs.SetField('lazarus', LTools.Field('LAZARUS').AsText);
    LOutputs.SetField('platform', 'i386-win32');
    LOutputs.SetField('widgetset', 'win32');
    LReply := EditorBuild(NyxObject([NyxField('mode', NyxData('profile'))]));
    EditorBuild(NyxObject([NyxField('mode', NyxData('profile')),
      NyxField('expectedOutputID', LReply.Field('outputID')),
      NyxField('profile', TNyxDataValue.ParseJSON(LOutputs.Encode))]));
    Check(RuntimeStatus('', '', 1) = 400, 'Actual runtime HTTP route refuses absent capability');
    Check(RuntimeStatus('', 'http://example.invalid', 1) = 403, 'Actual runtime route refuses foreign browser origin');

    LCompiled := Compile('lcl', 'application', '', 'runtime-native');
    LNativeGrant := LCompiled.Field('runtime');
    LPreparation := TPreparation.Create;
    LNative := TNyxLCLCompiledPreview.Create(GBase, ParamStr(2) + '/qualification/native');
    LNative.Prepare(AdmitNyxCompiledArtifact(LCompiled), LPreparation.Receive);
    LStarted := GetTickCount64;
    while not LPreparation.Finished do
    begin
      Application.ProcessMessages;

      if GetTickCount64 - LStarted > 20000 then
      begin
        raise Exception.Create('Actual native artifact preparation timed out');
      end;
      Sleep(20);
    end;
    Check(LPreparation.Succeeded, 'Actual native artifact passes byte manifest before launch');
    LNative.Launch(LNativeGrant);
    LSummary := WaitRuntime(LNativeGrant.Field('run').AsText);
    Check(LNative.Running and (LSummary.Field('scope').AsText = 'application'),
      'Separate ordinary native preview automatically enrolls its application run');
    WriteLn('PASS native automatic resource report'); Flush(Output);

    LCompiled := Compile('browser', 'application', '', 'runtime-browser');
    LBrowserGrant := LCompiled.Field('runtime');
    LBrowser := TNyxBrowserPipe.Create(GBase + '/' + LCompiled.Field('artifact').AsText +
      NyxStudioRuntimeFragment(LBrowserGrant), ParamStr(2) + '/qualification/browser', 390, 844);
    LSummary := WaitRuntime(LBrowserGrant.Field('run').AsText, LBrowser);
    Check((LBrowser.Attribute('data-nyx-ready') = 'true') and
      (Pos('nyx-studio-server', LBrowser.ElementHTML('[data-node="headline"]')) > 0),
      'Actual browser bound caption agrees with the hosted report');
    Check(LBrowser.RuntimeError = '', 'Ordinary browser producer executes without runtime exceptions');
    LBrowser.Capture('application');
    LSequence := LSummary.Field('sequence').AsInteger;
    LStarted := GetTickCount64;
    while GetTickCount64 - LStarted < 2200 do
    begin
      Application.ProcessMessages;
      Sleep(50);
    end;
    Check(FindRun(LBrowserGrant.Field('run').AsText).Field('sequence').AsInteger = LSequence,
      'Actual unchanged browser heartbeats do not invent resource changes');
    LReply := NyxTestEditorExchange(GBase, '/api/agents', GToken,
      NyxObject([NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
    Check(LReply.Field('resourceRuntimes').Count = 2,
      'Ordinary observing protocol exposes both automatically enrolled targets');
    WriteLn('PASS browser automatic resource report and observing protocol'); Flush(Output);

    for LScopeIndex := 0 to 1 do
    begin
      LScope := 'view';
      LView := 'home';

      if LScopeIndex = 1 then
      begin
        LScope := 'reusable';
        LView := 'workshop';
      end;
      for LTargetIndex := 0 to 1 do
      begin
        LTarget := 'browser';

        if LTargetIndex = 1 then
        begin
          LTarget := 'lcl';
        end;
        LCompiled := Compile(LTarget, LScope, LView,
          'runtime-' + LTarget + '-' + LScope);

        if LTargetIndex = 0 then
        begin
          FreeAndNil(LBrowser);
          LBrowserGrant := LCompiled.Field('runtime');
          LBrowserArtifact := LCompiled.Field('artifact').AsText;
          LBrowser := TNyxBrowserPipe.Create(GBase + '/' + LCompiled.Field('artifact').AsText +
            NyxStudioRuntimeFragment(LBrowserGrant),
            ParamStr(2) + '/qualification/' + LScope, 1100, 900);
          LSummary := WaitRuntime(LBrowserGrant.Field('run').AsText, LBrowser);
        end
        else
        begin
          LNativeGrant := LCompiled.Field('runtime');
          LPreparation.Finished := False;
          LPreparation.Succeeded := False;
          LNative.Prepare(AdmitNyxCompiledArtifact(LCompiled), LPreparation.Receive);
          LStarted := GetTickCount64;
          while not LPreparation.Finished do
          begin
            Application.ProcessMessages;

            if GetTickCount64 - LStarted > 20000 then
            begin
              raise Exception.Create('Scoped native artifact preparation timed out');
            end;
            Sleep(20);
          end;
          Check(LPreparation.Succeeded, 'Scoped native artifact passes its exact manifest');
          LNative.Launch(LNativeGrant);
          LSummary := WaitRuntime(LNativeGrant.Field('run').AsText);
        end;
        Check((LSummary.Field('scope').AsText = 'view') and
          (LSummary.Field('view').AsText = LView), 'Actual scoped producer identifies its exact root');
      end;
    end;
    WriteLn('PASS ordinary page and reusable producers on both targets'); Flush(Output);

    { Ordinary native preview Stop kills only its owned process. Lost producers
      need the real monotonic lease, rather than a fixture clock or fake retire. }
    FreeAndNil(LBrowser);
    LNative.Stop;
    LStarted := GetTickCount64;
    repeat
      LReply := Reports;
      LAllRetired := True;
      for LIndex := 0 to LReply.Count - 1 do
      begin
        LAllRetired := LAllRetired and not LReply.Item(LIndex).Field('active').AsBoolean;
      end;

      if not LAllRetired then
      begin

        if GetTickCount64 - LStarted > 65000 then
        begin
          raise Exception.Create('Lost real producer did not retire within its lease');
        end;
        Application.ProcessMessages;
        Sleep(250);
      end;
    until LAllRetired;
    Check(LReply.Count = 6, 'Six exact application/page/reusable observations remain as retired evidence');
    Check(RuntimeStatus(LNativeGrant.Field('token').AsText, '', 999) = 400,
      'Actual expired native launch capability refuses');
    WriteLn('PASS real producer loss and lease expiry'); Flush(Output);

    { Refused optional launch context must not break either ordinary wrapper.
      No new reporting authority is invented for these negative consumers. }
    LBrowser := TNyxBrowserPipe.Create(GBase + '/' + LBrowserArtifact +
      '#nyx-runtime=%7B%7D', ParamStr(2) + '/qualification/refused-context', 390, 844);
    Check((LBrowser.Attribute('data-nyx-ready') = 'true') and
      (LBrowser.Attribute('data-nyx-resource-reporting') = 'unavailable'),
      'Refused browser reporting context leaves its ordinary application ready');
    Check(LBrowser.RuntimeError = '', 'Optional browser diagnostic refusal has no uncaught exception');
    LNative.Launch(NyxObject([]));
    LStarted := GetTickCount64;
    while GetTickCount64 - LStarted < 500 do
    begin
      Application.ProcessMessages;
      Sleep(20);
    end;
    Check(LNative.Running, 'Refused native reporting context leaves its ordinary application running');
    LNative.Stop;
    Check(Reports.Count = 6, 'Refused optional context creates no additional observations');

    { A real paired mutation revokes all launch authority without replacing the
      project. Producer shutdown affects only this harness's owned processes. }
    LReply := Call('nyx_transaction', NyxObject([
      NyxField('operationId', NyxData('runtime-revision-change')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operations', TNyxDataValue.ParseJSON(
        '[{"op":"update","id":"headline","properties":{"text":"A new design"}}]'))]));
    GRevision := LReply.Field('revision').AsInteger;
    Check(Reports.Count = 0, 'Actual paired edit revokes reports at the old revision');
    Check(RuntimeStatus(LNativeGrant.Field('token').AsText, '', 999) = 400,
      'Actual revoked child capability cannot report into a newer design');
    WriteLn('PASS ', GChecks, ' authenticated HTTP and automatically launched producer checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  LBrowser.Free;
  LNative.Free;
  LPreparation.Free;

  if GClient <> nil then
  begin
    GClient.Close;
  end;
  GClient.Free;
  LOutputs.Free;
  LDocument.Free;
end.
