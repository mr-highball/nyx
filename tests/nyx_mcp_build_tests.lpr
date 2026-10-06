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

program nyx_mcp_build_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.studio.builds, fphttpclient, md5, base64,
  nyx.text, nyx.data, nyx.model, nyx.codec, nyx.codegen, nyx.source,
  nyx.studio.session, nyx.studio.projects, nyx.studio.buildjobs, nyx.studio.agents,
  nyx.test.compiler, nyx.test.mcp.client, nyx.test.browser.host;

type
  { Browser reads Pascal observer markers and captures real compiled artifacts.
    All authoring/build requests use semantic MCP. No script injection or browser
    designer automation is used. The invalid-helper substrate fixture is the
    sole explicit private editor commit until semantic body editing is exposed. }
  TBuildObserver = class(TNyxBrowserHost)
  public
    procedure WaitPhase(const APhase: TNyxText);
    procedure Capture;
    procedure ReadyArtifact;
    function CheckCount: TNyxText;
    procedure Phone;
  end;

var
  GClient: TNyxMCPTestClient;
  GObserver: TBuildObserver;
  GBase: TNyxText;
  GToken: TNyxText;
  GOutputID: TNyxText;
  GRevision: Integer;
  GCount: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GCount);
end;

function Call(const ATool: TNyxText; const AArgs: TNyxDataValue): TNyxDataValue;
var
  LValue: TNyxDataValue;
begin
  LValue := GClient.Tool(ATool, AArgs);

  if LValue.Field('isError').AsBoolean then
  begin
    raise Exception.Create('Semantic tool refused: ' + LValue.ToJSON);
  end;
  Result := LValue.Field('structuredContent');
end;

function Request(const AID, ATarget, AScope: TNyxText;
  const AView: TNyxText = ''): TNyxDataValue;
var
  LFields: array of TNyxDataField;
begin
  SetLength(LFields, 6);
  LFields[0] := NyxField('mode', NyxData('request'));
  LFields[1] := NyxField('operationId', NyxData(AID));
  LFields[2] := NyxField('expectedRevision', NyxData(GRevision));
  LFields[3] := NyxField('target', NyxData(ATarget));
  LFields[4] := NyxField('scope', NyxData(AScope));
  LFields[5] := NyxField('outputID', NyxData(GOutputID));

  if AView <> '' then
  begin
    SetLength(LFields, 7);
    LFields[6] := NyxField('view', NyxData(AView));
  end;
  Result := NyxObject(LFields);
end;

function Status(const AJob: TNyxText; AWait: Boolean = True): TNyxDataValue;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Result := Call('nyx_build', NyxObject([NyxField('mode', NyxData('status')),
      NyxField('job', NyxData(AJob)), NyxField('limit', NyxData(20))]));

    if not AWait or NyxBuildJobTerminal(ParseNyxBuildJobState(Result.Field('state').AsText)) then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 90000 then
    begin
      raise Exception.Create('Compiler job status timeout');
    end;
    Sleep(100);
  until False;
end;

procedure TBuildObserver.WaitPhase(const APhase: TNyxText);
var
  LStarted: QWord;
  LPhase: TNyxText;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;
    LPhase := Attribute('data-build-phase');

    if LPhase = 'failed' then
    begin
      raise Exception.Create(Attribute('data-build-error'));
    end;

    if GetTickCount64 - LStarted > 45000 then
    begin
      raise Exception.Create('Observer phase timeout: ' + APhase + ' / ' + LPhase);
    end;
    Sleep(20);
  until LPhase = APhase;
  Check(True, 'Observing phase ' + APhase);
end;

procedure TBuildObserver.Capture;
begin
  Save('preview.png', DecodeStringBase64(Command('Page.captureScreenshot', NyxObject([
    NyxField('format', NyxData('png')), NyxField('captureBeyondViewport', NyxData(False))]))
    .Field('data').AsText));
end;

function TBuildObserver.CheckCount: TNyxText;
begin
  Result := Attribute('data-nyx-build-observer-checks');
end;

procedure TBuildObserver.Phone;
begin
  Command('Emulation.setDeviceMetricsOverride', NyxObject([
    NyxField('width', NyxData(390)), NyxField('height', NyxData(844)),
    NyxField('deviceScaleFactor', NyxData(1)), NyxField('mobile', NyxData(True))]));
end;

procedure TBuildObserver.ReadyArtifact;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;

    if Attribute('data-nyx-ready') = 'true' then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Actual compiled browser application did not mount');
    end;
    Sleep(20);
  until False;
  Check(Node('[data-node="build-title"]') > 0, 'Actual semantic build artifact contains the authored label');
  Capture;
end;

function Fetch(const ARelative: TNyxText): TNyxText;
var
  LHTTP: TFPHTTPClient;
  LStream: TMemoryStream;
begin
  LHTTP := TFPHTTPClient.Create(nil);
  LStream := TMemoryStream.Create;
  try
    LHTTP.IOTimeout := 10000;
    LHTTP.Get(GBase + '/' + ARelative, LStream);
    SetLength(Result, LStream.Size);
    SetCodePage(RawByteString(Result), CP_UTF8, False);

    if LStream.Size > 0 then
    begin
      LStream.Position := 0;
      LStream.ReadBuffer(Result[1], LStream.Size);
    end;
  finally
    LHTTP.Free;
    LStream.Free;
  end;
end;

procedure Manifest(const AStatus: TNyxDataValue);
var
  LIndex: Integer;
  LItem: TNyxDataValue;
  LText: TNyxText;
begin
  for LIndex := 0 to AStatus.Field('manifest').Count - 1 do
  begin
    LItem := AStatus.Field('manifest').Item(LIndex);
    LText := Fetch(LItem.Field('path').AsText);
    Check((LItem.Field('bytes').AsInteger = Length(LText)) and
      (LItem.Field('md5').AsText = NyxBuildFingerprint(LText)),
      'Served artifact bytes match the job manifest');
  end;
  Check(AStatus.Field('manifest').Count > 0, 'Successful job has an artifact manifest');
end;

function ExportSource: TNyxText;
var
  LLines: TNyxStrings;
  LValue: TNyxDataValue;
  LLine: Integer;
  LIndex: Integer;
begin
  LLines := TNyxStrings.Create;
  try
    LLine := 1;
    repeat
      LValue := Call('nyx_source', NyxObject([
        NyxField('line', NyxData(LLine)), NyxField('count', NyxData(80))]));
      Check(LValue.Field('revision').AsInteger = GRevision, 'Bounded source export retains one revision');
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

procedure Refuse(const AArgs: TNyxDataValue; const AReason: TNyxText);
var
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
begin
  LBefore := Call('nyx_session', NyxObject([]));
  Check(GClient.Tool('nyx_build', AArgs).Field('isError').AsBoolean, AReason);
  LAfter := Call('nyx_session', NyxObject([]));
  Check((LAfter.Field('revision').AsInteger = LBefore.Field('revision').AsInteger) and
    (LAfter.Field('selection').AsText = LBefore.Field('selection').AsText) and
    (LAfter.Field('canUndo').AsBoolean = LBefore.Field('canUndo').AsBoolean),
    'Build refusal preserves content, selection and history');
end;

var
  LStudio: TNyxStudioSession;
  LValue: TNyxDataValue;
  LArgs: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LRetry: TNyxDataValue;
  LBefore: TNyxDataValue;
  LSource: TNyxText;
  LJob: TNyxText;
  LBrowserArtifact: TNyxText;
  LTarget: TNyxText;
  LScope: TNyxText;
  LView: TNyxText;
  LTargetIndex: Integer;
  LScopeIndex: Integer;
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
  LItem: TNyxDataValue;
  LIndex: Integer;
  LFound: Boolean;
  LArtifact: TBuildObserver;
  LExpected: ENyxSource;
begin
  GClient := nil;
  GObserver := nil;
  LStudio := nil;
  LDocument := nil;
  LArtifact := nil;
  LExpected := nil;
  try

    if (ParamCount <> 3) and (ParamCount <> 4) then
    begin
      raise Exception.Create('Supply isolated loopback editor base, MCP config and artifact directory');
    end;
    GBase := ParamStr(1);
    LStudio := TNyxStudioSession.Create;
    LValue := NyxTestEditorExchange(GBase, '/api/agents/connect', '', NyxObject([
      NyxField('op', NyxData('claim')), NyxField('project', NyxData(EncodeNyxProject(LStudio.ProjectSnapshot))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GToken := LValue.Field('token').AsText;
    GClient := TNyxMCPTestClient.Create(ParamStr(2), 'Scooty semantic build review');
    GRevision := Call('nyx_session', NyxObject([])).Field('revision').AsInteger;
    GObserver := TBuildObserver.Create(GBase + '/agent-build-observer.html', ParamStr(3) + '/observer');

    if ParamStr(4) = 'phone' then
    begin
      GObserver.Phone;
    end;
    GObserver.WaitPhase('ready');
    LValue := Call('nyx_transaction', NyxObject([
      NyxField('operationId', NyxData('build-review-compose')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operations', TNyxDataValue.ParseJSON(
        '[{"op":"create","kind":"page","id":"build-review","root":"page","properties":{"padding":24}},' +
        '{"op":"create","kind":"label","id":"build-title","parent":"build-review","properties":{"text":"Compiler workshop 🌙"}},' +
        '{"op":"create","kind":"card","id":"build-definition","root":"component","properties":{"padding":20}},' +
        '{"op":"create","kind":"label","id":"build-definition-title","parent":"build-definition","properties":{"text":"Reusable workshop"}}]'))]));
    GRevision := LValue.Field('revision').AsInteger;
    LSource := ExportSource;
    LBefore := Call('nyx_session', NyxObject([]));
    LValue := Call('nyx_build', NyxObject([NyxField('mode', NyxData('outputs'))]));
    GOutputID := LValue.Field('outputID').AsText;
    Check(LValue.Field('outputs').Count = 2, 'Readiness exposes both outputs without machine paths');
    Check(not NyxAgentHas(LValue, 'pas2js'), 'Readiness does not dump private compiler configuration');
    Refuse(Request('bad-target', 'shell', 'application'), 'Unpublished compiler target refuses');
    Refuse(Request('bad-view', 'browser', 'view', 'build-title'), 'Descendant view refuses');
    Refuse(Request('bad-scope', 'browser', 'reusable', 'build-review'), 'Reusable scope refuses page');
    LArgs := Request('compiler-injection', 'browser', 'application');
    Refuse(NyxObject([NyxField('mode', NyxData('request')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('inject')), NyxField('outputID', NyxData(GOutputID)),
      NyxField('target', NyxData('browser')), NyxField('scope', NyxData('application')),
      NyxField('command', NyxData('arbitrary executable'))]), 'Compiler command injection refuses');
    for LTargetIndex := 0 to 1 do
    begin
      LTarget := 'browser';

      if LTargetIndex = 1 then
      begin
        LTarget := 'lcl';
      end;
      for LScopeIndex := 0 to 2 do
      begin
        case LScopeIndex of
          0: begin LScope := 'view'; LView := 'build-review'; end;
          1: begin LScope := 'reusable'; LView := 'build-definition'; end;
          2: begin LScope := 'application'; LView := ''; end;
        end;
        LArgs := Request('qualified-' + LTarget + '-' + LScope, LTarget, LScope, LView);
        LReceipt := Call('nyx_build', LArgs);
        Check(LReceipt.Field('state').AsText = 'queued', 'Build returns an immediate immutable receipt');
        LJob := LReceipt.Field('job').AsText;

        if (LTargetIndex = 0) and (LScopeIndex = 0) then
        begin
          { An independent editor change proceeds while the job owns its earlier
            pair. Undo restores that exact pair with a later monotonic revision. }
          LValue := Call('nyx_transaction', NyxObject([
            NyxField('expectedRevision', NyxData(GRevision)),
            NyxField('operationId', NyxData('edit-during-compile')),
            NyxField('operations', TNyxDataValue.ParseJSON(
              '[{"op":"title","value":"Still editing during compilation"}]'))]));
          GRevision := LValue.Field('revision').AsInteger;
          Check(not Status(LJob, False).Field('currentSource').AsBoolean,
            'Editor can publish while the worker compiles its earlier immutable pair');
          LValue := Call('nyx_history', NyxObject([
            NyxField('expectedRevision', NyxData(GRevision)),
            NyxField('operationId', NyxData('undo-edit-during-compile')),
            NyxField('direction', NyxData('undo'))]));
          GRevision := LValue.Field('revision').AsInteger;
          GObserver.WaitPhase('working');
        end;
        LRetry := Call('nyx_build', LArgs);
        Check(LRetry.ToJSON = LReceipt.ToJSON, 'Exact retry returns the original job receipt');
        LStatus := Status(LJob);
        Check((LStatus.Field('state').AsText = 'succeeded') and
          LStatus.Field('currentSource').AsBoolean and LStatus.Field('currentOutput').AsBoolean,
          'Actual compiler succeeds against current source/output / ' + LTarget + '/' + LScope);
        Check((LStatus.Field('revision').AsInteger = LArgs.Field('expectedRevision').AsInteger) and
          (LStatus.Field('sourceFingerprint').AsText = NyxBuildFingerprint(LSource)),
          'Job binds the exact revision and bounded-exported accepted source');
        Manifest(LStatus);
        WriteLn('Qualified ', LTarget, '/', LScope, ' / revision ',
          LStatus.Field('revision').AsInteger, ' / artifact ', LStatus.Field('artifact').AsText);

        if LScope = 'application' then
        begin
          Check(Fetch(LStatus.Field('compiledSource').AsText) = LSource,
            'Application compiler consumes the exact accepted source bytes');
        end;

        if (LTargetIndex = 0) and (LScopeIndex = 0) then
        begin
          LBrowserArtifact := LStatus.Field('artifact').AsText;
        end;
      end;
    end;
    LValue := Call('nyx_session', NyxObject([]));
    Check((LValue.Field('revision').AsInteger = GRevision) and
      (LValue.Field('selection').AsText = LBefore.Field('selection').AsText) and
      (LValue.Field('canUndo').AsBoolean = LBefore.Field('canUndo').AsBoolean),
      'Six builds preserve selection and ordinary document history');
    LArtifact := TBuildObserver.Create(GBase + '/' + LBrowserArtifact, ParamStr(3) + '/compiled-browser');
    LArtifact.ReadyArtifact;
    FreeAndNil(LArtifact);

    { Intentional bad helper crosses the existing accepted companion boundary.
      Source/body authoring is a recorded MCP gap; no UI automation conceals it. }
    LDocument := CreateNyxCompilerFixture(LSource);
    LExpected := ENyxSource.CreateAt('expected', LSource, Pos('MissingApplicationFunction;', LSource));
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
    LValue := NyxTestEditorExchange(GBase, '/api/agents', GToken, NyxObject([
      NyxField('op', NyxData('commit')), NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GRevision := LValue.Field('session').Field('revision').AsInteger;
    LArgs := Request('qualified-helper-error', 'browser', 'view', 'home');
    LReceipt := Call('nyx_build', LArgs);
    LJob := LReceipt.Field('job').AsText;
    LStatus := Status(LJob);
    Check((LStatus.Field('state').AsText = 'failed') and
      (LStatus.Field('artifact').AsText = '') and (LStatus.Field('manifest').Count = 0),
      'Failed compiler exposes no successful artifact');
    LFound := False;
    for LIndex := 0 to LStatus.Field('diagnostics').Field('items').Count - 1 do
    begin
      LItem := LStatus.Field('diagnostics').Field('items').Item(LIndex);

      if Pos('MissingApplicationFunction', LItem.Field('message').AsText) > 0 then
      begin
        Check(LItem.Field('navigable').AsBoolean and
          (LItem.Field('line').AsInteger = LExpected.Line) and
          (LItem.Field('column').AsInteger = 11), 'Actual compiler maps the Unicode helper site');
        LFound := True;
      end;
    end;
    Check(LFound, 'Bounded job diagnostics expose the helper error');
    LValue := Call('nyx_build', NyxObject([
      NyxField('mode', NyxData('status')), NyxField('job', NyxData(LJob)),
      NyxField('severity', NyxData('error')), NyxField('limit', NyxData(1))]));
    Check((LValue.Field('diagnostics').Field('total').AsInteger = 1) and
      (LValue.Field('diagnostics').Field('items').Item(0).Field('severity').AsText = 'error'),
      'Focused closed-severity query returns only the actionable diagnostic');
    GObserver.WaitPhase('diagnostics');
    LValue := Call('nyx_transaction', NyxObject([
      NyxField('operationId', NyxData('change-after-build')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operations', TNyxDataValue.ParseJSON(
        '[{"op":"title","value":"A newer design"}]'))]));
    GRevision := LValue.Field('revision').AsInteger;
    LStatus := Status(LJob);
    Check(not LStatus.Field('currentSource').AsBoolean, 'Later semantic edit makes old job diagnostics stale');
    for LIndex := 0 to LStatus.Field('diagnostics').Field('items').Count - 1 do
    begin
      Check(not LStatus.Field('diagnostics').Field('items').Item(LIndex).Field('navigable').AsBoolean,
        'Stale job cannot navigate current source');
    end;
    Check(Call('nyx_build', LArgs).ToJSON = LReceipt.ToJSON, 'Retry after a new revision never rebuilds the old request');
    Refuse(Request('qualified-helper-error', 'browser', 'application'), 'Changed retry arguments refuse');
    Refuse(NyxObject([NyxField('mode', NyxData('status')), NyxField('job', NyxData(LJob)),
      NyxField('limit', NyxData(21))]), 'Diagnostic response window is bounded');
    GObserver.WaitPhase('passed');
    GObserver.Capture;
    Check(GObserver.CheckCount <> '', 'Actual Nyx observer qualifies diagnostics/activity');
    WriteLn('PASS ', GCount, ' real MCP/compiler/observer build checks');
    GClient.Close;
    FreeAndNil(GObserver);
    FreeAndNil(GClient);
    FreeAndNil(LExpected);
    FreeAndNil(LDocument);
    FreeAndNil(LStudio);
  except
    on LException: Exception do
    begin
      LArtifact.Free;
      GObserver.Free;
      GClient.Free;
      LExpected.Free;
      LDocument.Free;
      LStudio.Free;
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
