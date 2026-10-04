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
program nyx_mcp_handler_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, base64, fphttpclient, nyx.text, nyx.data,
  nyx.test.mcp.client, nyx.test.browser.host;

type
  { Authoring and compilation use semantic MCP exclusively. The CDP owner reads
    Pascal observer markers and validates actual host input/accessibility values;
    it never injects scripts or edits the designer. The generated source is
    exported unchanged for independent browser/LCL control consumers. }
  THandlerBrowser = class(TNyxBrowserHost)
  public
    procedure Phase(const APhase: TNyxText);
    procedure Capture;
    procedure Phone;
    procedure Artifact;
    function Value(const ASelector: TNyxText): TNyxText;
  end;

var
  GClient: TNyxMCPTestClient;
  GObserver: THandlerBrowser;
  GArtifact: THandlerBrowser;
  GRevision: Integer;
  GCount: Integer;
  GBase: TNyxText;
  GOutputID: TNyxText;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GCount);
end;

function Call(const ATool: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
var
  LValue: TNyxDataValue;
  LIndex: Integer;
  LRevisionKey: TNyxText;
begin
  LValue := GClient.Tool(ATool, AArguments);

  if LValue.Field('isError').AsBoolean then
  begin
    raise Exception.Create('Semantic request refused: ' + LValue.ToJSON);
  end;
  Result := LValue.Field('structuredContent');

  { Field deliberately refuses missing keys. Jobs carry both their captured
    revision and the current editor revision; prefer the latter when present. }
  LRevisionKey := '';
  for LIndex := 0 to Result.Count - 1 do
  begin

    if Result.Key(LIndex) = 'currentRevision' then
    begin
      LRevisionKey := 'currentRevision';
      Break;
    end;

    if Result.Key(LIndex) = 'revision' then
    begin
      LRevisionKey := 'revision';
    end;
  end;

  if LRevisionKey <> '' then
  begin
    GRevision := Result.Field(LRevisionKey).AsInteger;
  end;
end;

procedure THandlerBrowser.Phase(const APhase: TNyxText);
var
  LStarted: QWord;
  LPhase: TNyxText;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;
    LPhase := Attribute('data-handler-phase');

    if LPhase = 'failed' then
    begin
      raise Exception.Create(Attribute('data-handler-error'));
    end;

    if GetTickCount64 - LStarted > 45000 then
    begin
      raise Exception.Create('Handler observer wait exceeded: ' + APhase + '/' + LPhase);
    end;
    Sleep(20);
  until LPhase = APhase;
  Check(True, 'Observed ' + APhase);
end;

procedure THandlerBrowser.Capture;
begin
  Save('preview.png', DecodeStringBase64(Command('Page.captureScreenshot', NyxObject([
    NyxField('format', NyxData('png')), NyxField('captureBeyondViewport', NyxData(False))]))
    .Field('data').AsText));
end;

procedure THandlerBrowser.Phone;
begin
  Command('Emulation.setDeviceMetricsOverride', NyxObject([
    NyxField('width', NyxData(390)), NyxField('height', NyxData(844)),
    NyxField('deviceScaleFactor', NyxData(1)), NyxField('mobile', NyxData(True))]));
end;

function THandlerBrowser.Value(const ASelector: TNyxText): TNyxText;
var
  LNodes: TNyxDataValue;
begin
  LNodes := Command('Accessibility.getPartialAXTree', NyxObject([
    NyxField('nodeId', NyxData(Node(ASelector))), NyxField('fetchRelatives', NyxData(False))])).Field('nodes');
  Check(LNodes.Count = 1, 'Inspect only the actual input accessibility node');
  Result := LNodes.Item(0).Field('value').Field('value').AsText;
end;

procedure THandlerBrowser.Artifact;
const
  CNumberSelector: TNyxText = 'input[data-node="number-input"],[data-node="number-input"] input';
var
  LStarted: QWord;
  LNode: Integer;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Compiled semantic artifact did not mount');
    end;
    Sleep(20);
  until Attribute('data-nyx-ready') = 'true';
  LNode := Node(CNumberSelector);
  Check(LNode > 0, 'Actual MCP artifact contains the authored input');
  Command('DOM.focus', NyxObject([NyxField('nodeId', NyxData(LNode))]));
  Command('Input.insertText', NyxObject([NyxField('text', NyxData('12'))]));
  Check(Value(CNumberSelector) = '12', 'Host typing passes the compiled Pascal digit validator');
  Command('Input.insertText', NyxObject([NyxField('text', NyxData('x'))]));
  Check(Value(CNumberSelector) = '12', 'Host invalid typing is rejected by the authored callback');
  Capture;
end;

function Inspect(const AHandler: TNyxText): TNyxText;
var
  LOffset: Integer;
  LValue: TNyxDataValue;
  LParts: TNyxStrings;
begin
  LParts := TNyxStrings.Create;
  try
    LOffset := 0;
    repeat
      LValue := Call('nyx_pascal', NyxObject([
        NyxField('mode', NyxData('inspect')), NyxField('handler', NyxData(AHandler)),
        NyxField('offset', NyxData(LOffset)), NyxField('count', NyxData(256))]));
      LParts.Add(LValue.Field('text').AsText);
      LOffset := LValue.Field('nextOffset').AsInteger;
    until LOffset = LValue.Field('total').AsInteger;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function Edit(const AHandler, AExpected, ACode: TNyxText): TNyxDataValue;
begin
  Result := NyxObject([NyxField('handler', NyxData(AHandler)),
    NyxField('expected', NyxData(AExpected)), NyxField('implementation', NyxData(ACode))]);
end;

function Apply(const AID: TNyxText; const AChanges: TNyxDataValue): TNyxDataValue;
begin
  Result := Call('nyx_pascal', NyxObject([
    NyxField('mode', NyxData('apply')), NyxField('operationId', NyxData(AID)),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('changes', AChanges)]));
end;

function Build(const AID, ATarget: TNyxText): TNyxDataValue;
var
  LJob: TNyxText;
  LStarted: QWord;
begin
  LJob := Call('nyx_build', NyxObject([
    NyxField('mode', NyxData('request')), NyxField('operationId', NyxData(AID)),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('outputID', NyxData(GOutputID)),
    NyxField('target', NyxData(ATarget)), NyxField('scope', NyxData('view')),
    NyxField('view', NyxData('handler-workshop'))])).Field('job').AsText;
  LStarted := GetTickCount64;
  repeat
    Result := Call('nyx_build', NyxObject([
      NyxField('mode', NyxData('status')), NyxField('job', NyxData(LJob)),
      NyxField('severity', NyxData('error')), NyxField('limit', NyxData(5))]));

    if GetTickCount64 - LStarted > 90000 then
    begin
      raise Exception.Create('Handler compiler job did not complete');
    end;
    Sleep(100);
  until Result.Field('state').AsText <> 'running';
end;

function Fetch(const APath: TNyxText): TNyxText;
var
  LHTTP: TFPHTTPClient;
  LStream: TMemoryStream;
begin
  LHTTP := TFPHTTPClient.Create(nil);
  LStream := TMemoryStream.Create;
  try
    LHTTP.IOTimeout := 10000;
    LHTTP.Get(GBase + '/' + APath, LStream);
    SetLength(Result, LStream.Size);
    SetCodePage(RawByteString(Result), CP_UTF8, False);

    if LStream.Size > 0 then
    begin
      LStream.Position := 0;
      LStream.ReadBuffer(Result[1], LStream.Size);
    end;
  finally
    LStream.Free;
    LHTTP.Free;
  end;
end;

procedure Export(const AText: TNyxText);
var
  LStream: TFileStream;
begin
  ForceDirectories(ParamStr(3) + '/source');
  LStream := TFileStream.Create(ParamStr(3) + '/source/nyx.generated.view.pas', fmCreate);
  try

    if AText <> '' then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;

var
  LValue: TNyxDataValue;
  LHandlers: TNyxDataValue;
  LStatus: TNyxDataValue;
  LBefore: TNyxDataValue;
  LFirst: TNyxText;
  LSecond: TNyxText;
  LFirstOld: TNyxText;
  LSecondOld: TNyxText;
  LFirstCode: TNyxText;
  LSecondCode: TNyxText;
  LAcceptedSource: TNyxText;
begin
  GClient := nil;
  GObserver := nil;
  GArtifact := nil;
  try
    GBase := ParamStr(1);
    GClient := TNyxMCPTestClient.Create(ParamStr(2));
    LValue := GClient.RPC('tools/list', NyxObject([])).Field('result');
    Check(LValue.Field('tools').Count = 14, 'Actual MCP initializes all fourteen focused tools');
    GObserver := THandlerBrowser.Create(GBase + '/handler-observer.html', ParamStr(3) + '/observer');

    if (ParamCount = 4) and (ParamStr(4) = 'phone') then
    begin
      GObserver.Phone;
    end;
    GObserver.Phase('ready');
    LBefore := Call('nyx_session', NyxObject([]));
    Call('nyx_transaction', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('handler-compose')),
      NyxField('operations', TNyxDataValue.ParseJSON(
        '[{"op":"create","kind":"page","id":"handler-workshop","root":"page","properties":{"layout":"column","gap":16,"padding":24}},' +
        '{"op":"create","kind":"heading","id":"handler-title","parent":"handler-workshop","properties":{"text":"Thoughtful input"}},' +
        '{"op":"create","kind":"label","id":"handler-help","parent":"handler-workshop","properties":{"text":"Enter digits and a short note. Pascal callbacks keep the input meaningful."}},' +
        '{"op":"create","kind":"input","id":"number-input","parent":"handler-workshop","properties":{"text":"Quantity","placeholder":"Digits only","value":""}},' +
        '{"op":"create","kind":"memo","id":"short-note","parent":"handler-workshop","properties":{"text":"A short note","placeholder":"Up to forty Unicode characters","value":""}}]'))]));
    Check(Call('nyx_session', NyxObject([])).Field('selection').AsText = LBefore.Field('selection').AsText,
      'Semantic demo composition retains the operator selection');
    LHandlers := Call('nyx_callbacks', NyxObject([
      NyxField('mode', NyxData('apply')), NyxField('operationId', NyxData('handler-add')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('changes', TNyxDataValue.ParseJSON(
        '[{"op":"add","id":"number-input","event":{"trigger":"before-text-input"}},' +
        '{"op":"add","id":"short-note","event":{"trigger":"before-text-input"}}]'))])).Field('callbacks');
    LFirst := LHandlers.Item(0).Field('handler').AsText;
    LSecond := LHandlers.Item(1).Field('handler').AsText;
    GObserver.Phase('added');
    LFirstOld := Inspect(LFirst);
    LSecondOld := Inspect(LSecond);
    LFirstCode := #10 + 'var' + #10 + '  LIndex: Integer;' + #10 +
      'function IsDigit(ACharacter: Char): Boolean;' + #10 +
      'begin' + #10 + '  Result := (ACharacter >= ''0'') and (ACharacter <= ''9'');' + #10 + 'end;' + #10 +
      'begin' + #10 + #10 + '  if AExecution.Cancelled or not AEvent.HasTextEdit then' + #10 +
      '  begin' + #10 + '    Exit;' + #10 + '  end;' + #10 +
      '  // Validate the complete proposed quantity.' + #10 +
      '  for LIndex := 1 to Length(AEvent.TextEdit.After) do' + #10 +
      '  begin' + #10 + #10 + '    if not IsDigit(AEvent.TextEdit.After[LIndex]) then' + #10 +
      '    begin' + #10 + '      NyxEventResponse(AExecution).Consume;' + #10 +
      '      Exit;' + #10 + '    end;' + #10 + '  end;' + #10 + 'end;';
    LSecondCode := #10 + 'const' + #10 + '  CMaximumNoteCharacters = 40;' + #10 +
      'begin' + #10 + #10 + '  if AExecution.Cancelled or not AEvent.HasTextEdit then' + #10 +
      '  begin' + #10 + '    Exit;' + #10 + '  end;' + #10 +
      '  // Count Unicode scalars, so a moon 🌙 is one character.' + #10 + #10 +
      '  if NyxTextScalarCount(AEvent.TextEdit.After) > CMaximumNoteCharacters then' + #10 +
      '  begin' + #10 + '    NyxEventResponse(AExecution).Consume;' + #10 + '  end;' + #10 + 'end;';
    LValue := Apply('handler-implement', NyxArray([
      Edit(LFirst, LFirstOld, LFirstCode), Edit(LSecond, LSecondOld, LSecondCode)]));
    Check(LValue.Field('handlers').Count = 2, 'One semantic operation authors both implementation bodies');
    GObserver.Phase('undone');
    LValue := Call('nyx_session', NyxObject([]));
    Check(Inspect(LFirst) = LFirstOld, 'Ordinary editor Undo restores the exact first implementation');
    Check(Inspect(LSecond) = LSecondOld, 'The same Undo restores the second implementation');
    Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('handler-redo')),
      NyxField('direction', NyxData('redo'))]));
    GObserver.Phase('passed');
    Check((Inspect(LFirst) = LFirstCode) and (Inspect(LSecond) = LSecondCode),
      'Semantic Redo restores exact handcrafted implementations');
    GObserver.Capture;
    GOutputID := Call('nyx_build', NyxObject([NyxField('mode', NyxData('outputs'))])).Field('outputID').AsText;
    LStatus := Build('handler-browser', 'browser');
    Check((LStatus.Field('state').AsText = 'succeeded') and LStatus.Field('currentSource').AsBoolean,
      'Actual pas2js compiles the authored callback application');
    LAcceptedSource := Fetch(LStatus.Field('compiledSource').AsText);
    Check((Pos(LFirstCode, LAcceptedSource) > 0) and (Pos(LSecondCode, LAcceptedSource) > 0),
      'Delegated compilation preserves both method implementations unchanged');
    Export(LAcceptedSource);
    GArtifact := THandlerBrowser.Create(GBase + '/' + LStatus.Field('artifact').AsText,
      ParamStr(3) + '/artifact');
    GArtifact.Artifact;
    FreeAndNil(GArtifact);
    LStatus := Build('handler-native', 'lcl');
    Check((LStatus.Field('state').AsText = 'succeeded') and
      (Fetch(LStatus.Field('compiledSource').AsText) = LAcceptedSource),
      'Actual FPC/LCL compiler consumes the exact same authored companion');
    Apply('handler-compiler-error', NyxArray([Edit(LFirst, LFirstCode,
      #10 + 'begin' + #10 + '  { 🌙 } MissingQuantityCheck;' + #10 + 'end;')]));
    LStatus := Build('handler-error', 'browser');
    Check((LStatus.Field('state').AsText = 'failed') and
      (LStatus.Field('diagnostics').Field('items').Count = 1) and
      (Pos('MissingQuantityCheck', LStatus.Field('diagnostics').Field('items').Item(0).Field('message').AsText) > 0) and
      LStatus.Field('diagnostics').Field('items').Item(0).Field('navigable').AsBoolean,
      'Source admission retains an ordinary compiler error with current-source navigation');
    Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('handler-undo-error')),
      NyxField('direction', NyxData('undo'))]));
    Check(Inspect(LFirst) = LFirstCode, 'Undo recovers the exact working callback after failed compilation');
    LStatus := Call('nyx_build', NyxObject([
      NyxField('mode', NyxData('status')), NyxField('job', LStatus.Field('job')),
      NyxField('severity', NyxData('error')), NyxField('limit', NyxData(1))]));
    Check(not LStatus.Field('currentSource').AsBoolean and
      not LStatus.Field('diagnostics').Field('items').Item(0).Field('navigable').AsBoolean,
      'Earlier failed compiler source cannot navigate the recovered method');
    WriteLn('PASS ', GCount, ' real MCP handler/observer/compiler/host-input checks');
    GClient.Close;
    FreeAndNil(GObserver);
    FreeAndNil(GClient);
  except
    on LException: Exception do
    begin
      GArtifact.Free;
      GObserver.Free;
      GClient.Free;
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
