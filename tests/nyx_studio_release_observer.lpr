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

program nyx_studio_release_observer;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.data, nyx.dates, nyx.types, nyx.contract,
  nyx.studio.edits, nyx.test.mcp.client, nyx.test.browser.pipe;

var
  GClient: TNyxMCPTestClient;
  GBrowser: TNyxBrowserPipe;
  GWorkspace: TNyxText;
  GDirectory: String;
  GRevision: Integer;
  GChecks: Integer;
  GWidth: Integer;
  GSequence: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ The observer never claims/replaces the primary project. Its explicitly owned
  ordinary workspace is supplied by the maintained semantic client. }
function Call(const AName: TNyxText; const AFields: array of TNyxDataField;
  ARefused: Boolean = False): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LPacket: TNyxDataValue;
begin
  SetLength(LFields, Length(AFields) + 1);
  for LIndex := 0 to High(AFields) do
  begin
    LFields[LIndex] := AFields[LIndex];
  end;
  LFields[High(LFields)] := NyxField('workspace', NyxData(GWorkspace));
  LPacket := GClient.Tool(AName, NyxObject(LFields));
  Check(LPacket.Field('isError').AsBoolean = ARefused,
    AName + TNyxText(' admitted/refused as expected'));
  Result := LPacket.Field('structuredContent');
end;

procedure RefreshRevision;
begin
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
end;

function OperationID: TNyxText;
begin
  Inc(GSequence);
  Result := TNyxText('observing-') + IntToStr(GWidth) + TNyxText('-') +
    IntToStr(GRevision) + TNyxText('-') + IntToStr(GSequence);
end;

procedure Transaction(const AOperations: array of TNyxDataValue; ARefused: Boolean = False);
begin
  Call('nyx_transaction', [
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(OperationID)),
    NyxField('operations', NyxArray(AOperations))], ARefused);
  RefreshRevision;
end;

function Source: TNyxText;
var
  LReply: TNyxDataValue;
  LLine: Integer;
  LTotal: Integer;
  LIndex: Integer;
begin
  Result := '';
  LLine := 1;
  repeat
    LReply := Call('nyx_source', [NyxField('line', NyxData(LLine)),
      NyxField('count', NyxData(80))]);
    Check(LReply.Field('revision').AsInteger = GRevision, 'source window keeps exact revision');
    for LIndex := 0 to LReply.Field('lines').Count - 1 do
    begin

      if (LLine > 1) or (LIndex > 0) then
      begin
        Result := Result + #10;
      end;
      Result := Result + LReply.Field('lines').Item(LIndex).AsText;
    end;
    LTotal := LReply.Field('totalLines').AsInteger;
    Inc(LLine, 80);
  until LLine > LTotal;
  { This fixture owns a canonical LF-terminated generated companion. Bounded
    line queries omit terminators; arbitrary user-file byte export remains a
    separate semantic source operation, not inferred by this observer. }
  Result := Result + #10;
end;

procedure WaitField(const ASelector, AExpected: TNyxText);
var
  LStarted: QWord;
  LValue: TNyxText;
begin
  LStarted := GetTickCount64;
  repeat

    if GBrowser.RuntimeError <> '' then
    begin
      raise Exception.Create('Actual observing browser raised; inspect runtime-error.json');
    end;

    if GBrowser.TryFieldValue(ASelector, LValue) and (LValue = AExpected) then
    begin
      Inc(GChecks);
      Exit;
    end;

    if GetTickCount64 - LStarted > 20000 then
    begin
      GBrowser.Capture('failure');
      raise Exception.Create(TNyxText('Observed field did not synchronize / ') + ASelector);
    end;
    Sleep(100);
  until False;
end;

procedure Build(const ATarget: TNyxText);
var
  LOutputs: TNyxDataValue;
  LOutputPacket: TNyxDataValue;
  LOutput: TNyxDataValue;
  LReply: TNyxDataValue;
  LJob: TNyxText;
  LIndex: Integer;
  LStarted: QWord;
  LStream: TFileStream;
  LText: TNyxText;
begin
  LOutputPacket := Call('nyx_build', [NyxField('mode', NyxData('outputs'))]);
  LOutputs := LOutputPacket.Field('outputs');
  LOutput := Default(TNyxDataValue);
  for LIndex := 0 to LOutputs.Count - 1 do
  begin

    if LOutputs.Item(LIndex).Field('target').AsText = ATarget then
    begin
      LOutput := LOutputs.Item(LIndex);
    end;
  end;
  Check(LOutput.Defined and LOutput.Field('ready').AsBoolean, 'chosen compiler output is ready');
  LReply := Call('nyx_build', [
    NyxField('mode', NyxData('request')), NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(OperationID)), NyxField('outputID', LOutputPacket.Field('outputID')),
    NyxField('target', NyxData(ATarget)), NyxField('scope', NyxData('application'))]);
  LJob := LReply.Field('job').AsText;
  LStarted := GetTickCount64;
  repeat
    Sleep(100);
    LReply := Call('nyx_build', [NyxField('mode', NyxData('status')),
      NyxField('job', NyxData(LJob)), NyxField('limit', NyxData(1)),
      NyxField('severity', NyxData('error'))]);

    if (LReply.Field('state').AsText <> 'queued') and
      (LReply.Field('state').AsText <> 'running') then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 90000 then
    begin
      raise Exception.Create('Real semantic compiler job exceeded observer budget');
    end;
  until False;
  LText := LReply.ToJSON;
  LStream := TFileStream.Create(GDirectory + String(ATarget) + '-build.json', fmCreate);
  try
    LStream.WriteBuffer(LText[1], Length(LText));
  finally
    LStream.Free;
  end;
  Check(LReply.Field('state').AsText = 'succeeded', 'real application compiler job succeeded');
  WriteLn('Observed real compiler / ', ATarget, ' / ', LJob);
end;

var
  LBefore: TNyxText;
  LAfter: TNyxText;
  LTools: TNyxDataValue;
  LStarted: QWord;
  LReply: TNyxDataValue;
begin
  GClient := nil;
  GBrowser := nil;
  try

    if ParamCount <> 4 then
    begin
      raise Exception.Create('Supply enrolled repository, owned workspace, fresh evidence directory, CSS width');
    end;
    GWorkspace := ParamStr(2);
    GDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));
    GWidth := StrToInt(ParamStr(4));
    ForceDirectories(GDirectory);
    GClient := TNyxMCPTestClient.Create(IncludeTrailingPathDelimiter(ParamStr(1)) +
      '.codex' + PathDelim + 'config.toml', 'Scooty observing release');
    LTools := GClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools');
    Check(LTools.Count = 20, 'actual authenticated twenty-tool discovery');
    RefreshRevision;
    LReply := Call('nyx_node', [NyxField('id', NyxData('first-arrival')),
      NyxField('limit', NyxData(1)), NyxField('valueDomain', NyxData(True)),
      NyxField('domainScope', NyxData('local')), NyxField('domainLimit', NyxData(1))]);

    if LReply.Field('valueDomain').Field('defined').AsBoolean then
    begin
      Transaction([NyxInheritValueDomain(NyxControl('first-arrival')).ToData,
        NyxInheritValueDomain(NyxControl('second-arrival')).ToData]);
    end;
    Call('nyx_select', [NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(OperationID)), NyxField('id', NyxData('home')),
      NyxField('activate', NyxData(True))]);
    RefreshRevision;
    Call('nyx_select', [NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(OperationID)), NyxField('id', NyxData('first-arrival'))]);
    RefreshRevision;
    LBefore := Source;
    GBrowser := TNyxBrowserPipe.Create('http://127.0.0.1:8088/?workspace=' +
      String(GWorkspace), GDirectory, GWidth);
    LStarted := GetTickCount64;
    repeat

      if GBrowser.RuntimeError <> '' then
      begin
        raise Exception.Create('Actual ordinary Studio startup exception');
      end;

      if GBrowser.Attribute('data-nyx-studio-ready') = 'true' then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 20000 then
      begin
        raise Exception.Create('Ordinary Studio did not render on real clocks');
      end;
      Sleep(100);
    until False;
    if GWidth < 640 then
    begin
      { Compact Studio mounts one panel at a time. Open the ordinary Project
        host control before observing its title field; absence is not a pass. }
      GBrowser.Click('[data-node=action-panel-project]');
    end;
    WaitField('[data-node=project-title] input', 'Plan a little getaway');

    if GWidth < 640 then
    begin
      GBrowser.Click('[data-node=action-panel-inspector]');
    end;
    WaitField('[data-node=inspector-date-domain-minimum] input', '');
    GBrowser.Capture('inherited');
    Transaction([
      NyxSetValueDomain(NyxControl('first-arrival'), NyxDateDomain
        .Range(NyxDate(2026, 10, 1), NyxDate(2026, 10, 31))).ToData,
      NyxSetValueDomain(NyxControl('second-arrival'), NyxDateDomain
        .Range(NyxDate(2026, 11, 1), NyxDate(2026, 11, 30))).ToData]);
    WaitField('[data-node=inspector-date-domain-minimum] input', '2026-10-01');
    WaitField('[data-node=inspector-date-domain-maximum] input', '2026-10-31');
    LAfter := Source;
    Check(Pos('NyxDateDomain.Range(NyxDate(2026, 10, 1)', LAfter) > 0,
      'accepted adjacent source uses typed fluent date constraints');
    LReply := Call('nyx_node', [NyxField('id', NyxData('first-arrival')),
      NyxField('limit', NyxData(1)), NyxField('valueDomain', NyxData(True)),
      NyxField('domainScope', NyxData('local')), NyxField('domainLimit', NyxData(1))]);
    Check(LReply.Field('valueDomain').Field('minimum').AsText = '2026-10-01',
      'bounded MCP policy matches observing Inspector');
    GBrowser.Capture('constraints');
    Transaction([NyxSetValueDomain(NyxControl('first-arrival'), NyxDateDomain
      .Range(NyxDate(2026, 10, 7), NyxDate(2026, 10, 31))).ToData], True);
    Check(Source = LAfter, 'refused default-incompatible policy preserves exact source');
    Call('nyx_history', [NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(OperationID)), NyxField('direction', NyxData('undo'))]);
    RefreshRevision;
    Check(Source = LBefore, 'one paired Undo restores exact accepted source');
    WaitField('[data-node=inspector-date-domain-minimum] input', '');
    Call('nyx_history', [NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(OperationID)), NyxField('direction', NyxData('redo'))]);
    RefreshRevision;
    WaitField('[data-node=inspector-date-domain-minimum] input', '2026-10-01');
    Check(Source = LAfter, 'paired Redo restores exact typed source');
    GBrowser.Click('[data-node=action-code]');
    WaitField('[data-node=studio-code]', LAfter);
    GBrowser.Capture('source');
    GBrowser.Click('[data-node=action-expand-source]');
    WaitField('[data-node=studio-code]', LAfter);
    GBrowser.Capture('expanded-source');
    GBrowser.Click('[data-node=action-expand-source]');
    WaitField('[data-node=studio-code]', LAfter);

    if GWidth >= 640 then
    begin
      Build('browser');
      Build('lcl');
    end;
    FreeAndNil(GBrowser);
    GClient.Close;
    FreeAndNil(GClient);
    WriteLn('PASS actual observing Studio / ', GChecks, ' / CSS width ', GWidth);
  except
    on LException: Exception do
    begin

      if GBrowser <> nil then
      begin
        try
          GBrowser.Capture('failure');
        except
          on LCaptureError: Exception do
          begin
            WriteLn('Failure capture unavailable / ', LCaptureError.Message);
          end;
        end;
      end;
      GBrowser.Free;

      if GClient <> nil then
      begin
        try
          { Failed assertion must retire this observer's authenticated transport
            too. Preserve the original diagnostic if retirement itself fails. }
          GClient.Close;
        except
          on LCloseError: Exception do
          begin
            WriteLn('Transport retirement refused / ', LCloseError.Message);
          end;
        end;
      end;
      GClient.Free;
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
