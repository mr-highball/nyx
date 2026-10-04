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
program nyx_mcp_root_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, base64, fphttpclient, nyx.text, nyx.data, nyx.source,
  nyx.test.mcp.client, nyx.test.browser.host;

type
  { Semantic MCP owns all document mutations/builds. The CDP owner only waits
    for Pascal assertions from the ordinary observing Studio and captures it. }
  TRootBrowser = class(TNyxBrowserHost)
  public
    procedure Phase(const APhase: TNyxText);
    procedure Phone;
    procedure Capture;
  end;

var
  GClient: TNyxMCPTestClient;
  GObserver: TRootBrowser;
  GRevision: Integer;
  GChecks: Integer;
  GBase: TNyxText;
  GOutputID: TNyxText;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Call(const ATool: TNyxText; const AArgs: TNyxDataValue): TNyxDataValue;
var
  LValue: TNyxDataValue;
  LIndex: Integer;
begin
  LValue := GClient.Tool(ATool, AArgs);

  if LValue.Field('isError').AsBoolean then
  begin
    raise Exception.Create('MCP refused: ' + LValue.ToJSON);
  end;
  Result := LValue.Field('structuredContent');
  for LIndex := 0 to Result.Count - 1 do
  begin

    if Result.Key(LIndex) = 'revision' then
    begin
      GRevision := Result.Field('revision').AsInteger;
    end;
  end;
end;

procedure TRootBrowser.Phase(const APhase: TNyxText);
var
  LStarted: QWord;
  LPhase: TNyxText;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;
    LPhase := Attribute('data-root-phase');

    if LPhase = 'failed' then
    begin
      raise Exception.Create(Attribute('data-root-error'));
    end;

    if GetTickCount64 - LStarted > 45000 then
    begin
      raise Exception.Create('Root observer did not reach ' + APhase + ' / ' + LPhase);
    end;
    Sleep(20);
  until LPhase = APhase;
end;

procedure TRootBrowser.Phone;
begin
  Command('Emulation.setDeviceMetricsOverride', NyxObject([
    NyxField('width', NyxData(390)), NyxField('height', NyxData(844)),
    NyxField('deviceScaleFactor', NyxData(1)), NyxField('mobile', NyxData(True))]));
end;

procedure TRootBrowser.Capture;
begin
  Save('preview.png', DecodeStringBase64(Command('Page.captureScreenshot', NyxObject([
    NyxField('format', NyxData('png')), NyxField('captureBeyondViewport', NyxData(False))]))
    .Field('data').AsText));
end;

function Source: TNyxText;
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
      LValue := Call('nyx_source', NyxObject([NyxField('line', NyxData(LLine)),
        NyxField('count', NyxData(80))]));
      Check(GRevision = LRevision, 'Bounded source windows retain one revision');
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

function Roots(const AMode: TNyxText; const ARoots: TNyxDataValue;
  const AReview: TNyxText = ''): TNyxDataValue;
var
  LFields: array of TNyxDataField;
begin
  SetLength(LFields, 3);
  LFields[0] := NyxField('mode', NyxData(AMode));
  LFields[1] := NyxField('expectedRevision', NyxData(GRevision));
  LFields[2] := NyxField('roots', ARoots);

  if AReview <> '' then
  begin
    SetLength(LFields, 5);
    LFields[3] := NyxField('operationId', NyxData('cleanup-review-roots'));
    LFields[4] := NyxField('reviewID', NyxData(AReview));
  end;
  Result := NyxObject(LFields);
end;

function Build(const ATarget: TNyxText): TNyxDataValue;
var
  LJob: TNyxText;
  LStarted: QWord;
begin
  LJob := Call('nyx_build', NyxObject([NyxField('mode', NyxData('request')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('cleaned-' + ATarget)),
    NyxField('outputID', NyxData(GOutputID)), NyxField('target', NyxData(ATarget)),
    NyxField('scope', NyxData('application'))])).Field('job').AsText;
  LStarted := GetTickCount64;
  repeat
    Result := Call('nyx_build', NyxObject([NyxField('mode', NyxData('status')),
      NyxField('job', NyxData(LJob)), NyxField('severity', NyxData('error'))]));

    if GetTickCount64 - LStarted > 90000 then
    begin
      raise Exception.Create('Cleanup compiler job did not finish');
    end;
    Sleep(100);
  until Result.Field('state').AsText <> 'running';
  Check(Result.Field('state').AsText = 'succeeded', 'Actual compiler accepts cleanup / ' + ATarget);
end;

procedure Export(const ASource: TNyxText);
var
  LFile: TFileStream;
begin
  ForceDirectories(ParamStr(3) + '/source');
  LFile := TFileStream.Create(ParamStr(3) + '/source/nyx.generated.view.pas', fmCreate);
  try
    LFile.WriteBuffer(ASource[1], Length(ASource));
  finally
    LFile.Free;
  end;
end;

var
  LGroup: TNyxDataValue;
  LDefinition: TNyxDataValue;
  LReview: TNyxDataValue;
  LApply: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LBefore: TNyxText;
  LAuthored: TNyxText;
  LCleaned: TNyxText;
  LPrefix: TNyxText;
  LBody: TNyxText;
  LSuffix: TNyxText;
  LNewPrefix: TNyxText;
  LNewSuffix: TNyxText;
begin
  try
    GBase := ParamStr(1);
    GClient := TNyxMCPTestClient.Create(ParamStr(2));
    Check(GClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools').Count = 15,
      'Actual Codex-compatible MCP advertises fifteen tools');
    GObserver := TRootBrowser.Create(GBase + '/root-observer.html', ParamStr(3) + '/observer');

    if (ParamCount = 4) and (ParamStr(4) = 'phone') then
    begin
      GObserver.Phone;
    end;
    GObserver.Phase('ready');
    Call('nyx_session', NyxObject([]));
    LBefore := Source;
    Call('nyx_transaction', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('compose-review-roots')),
      NyxField('operations', TNyxDataValue.ParseJSON(
        '[{"op":"create","kind":"column","id":"review-definition","root":"component","properties":{"padding":20}},' +
        '{"op":"create","kind":"label","id":"review-greeting","parent":"review-definition","properties":{"text":"A reusable moon 🌙"}},' +
        '{"op":"create","kind":"page","id":"review-workshop","root":"page","properties":{"layout":"column"}},' +
        '{"op":"create","kind":"component","id":"review-use","parent":"review-workshop","properties":{"component":"review-definition"}},' +
        '{"op":"create","kind":"memo","id":"review-note","parent":"review-workshop","properties":{"text":"Review notes"}}]'))]));
    Call('nyx_callbacks', NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('review-callback')),
      NyxField('changes', TNyxDataValue.ParseJSON(
        '[{"op":"add","id":"review-note","event":{"trigger":"after-text-input"}}]'))]));
    Call('nyx_select', NyxObject([NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('activate-review')), NyxField('id', NyxData('review-definition')),
      NyxField('activate', NyxData(True))]));
    GObserver.Phase('authored');
    GObserver.Capture;
    LAuthored := Source;
    LDefinition := TNyxDataValue.ParseJSON('[{"root":"component","id":"review-definition"}]');
    LReview := Call('nyx_roots', Roots('review', LDefinition));
    Check(not LReview.Field('removal').Field('ready').AsBoolean and
      (LReview.Field('removal').Field('retainedReferences').AsInteger = 1), 'Review reports one retained authored reusable reference');
    Check(GClient.Tool('nyx_roots', Roots('apply', LDefinition, LReview.Field('reviewID').AsText))
      .Field('isError').AsBoolean, 'Real MCP refuses dangling reusable removal');
    Check(Source = LAuthored, 'Refusal retains every accepted source line');
    LGroup := TNyxDataValue.ParseJSON('[{"root":"component","id":"review-definition"},{"root":"page","id":"review-workshop"}]');
    LReview := Call('nyx_roots', Roots('review', LGroup));
    Check(LReview.Field('removal').Field('ready').AsBoolean and
      (LReview.Field('removal').Field('registrations').AsInteger = 1), 'Complete root group warns about its callback registration');
    LApply := Roots('apply', LGroup, LReview.Field('reviewID').AsText);
    LReceipt := Call('nyx_roots', LApply);
    Check((LReceipt.Field('pages').AsInteger = 1) and (LReceipt.Field('components').AsInteger = 1),
      'Only owned review roots are removed');
    Check(Call('nyx_roots', LApply).ToJSON = LReceipt.ToJSON, 'Consumed review exact retry returns original receipt');
    GObserver.Phase('undone');
    Call('nyx_session', NyxObject([]));
    Check(Source = LAuthored, 'Ordinary observing Studio Undo restores exact authored source');
    Call('nyx_history', NyxObject([NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('cleanup-redo')), NyxField('direction', NyxData('redo'))]));
    GObserver.Phase('passed');
    LCleaned := Source;
    SplitNyxSourceFrame(LAuthored, LPrefix, LBody, LSuffix);
    SplitNyxSourceFrame(LCleaned, LNewPrefix, LBody, LNewSuffix);
    Check((LPrefix = LNewPrefix) and (LSuffix = LNewSuffix), 'Surrounding application Pascal survives demo cleanup byte for byte');
    Check((Pos('''review-workshop''', LCleaned) = 0) and (Pos('// TODO: implement ', LCleaned) > 0),
      'Managed roots retire while local callback implementation remains');
    GOutputID := Call('nyx_build', NyxObject([NyxField('mode', NyxData('outputs'))])).Field('outputID').AsText;
    Build('browser');
    Build('lcl');
    Export(LCleaned);
    GClient.Close;
    FreeAndNil(GObserver);
    FreeAndNil(GClient);
    WriteLn('PASS ', GChecks, ' real MCP root/observer/compiler checks');
  except
    on LException: Exception do
    begin
      GObserver.Free;
      GClient.Free;
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
