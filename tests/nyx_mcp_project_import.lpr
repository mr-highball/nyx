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
program nyx_mcp_project_import;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.studio.projects,
  nyx.studio.session, nyx.studio.builds, nyx.studio.buildjobs,
  nyx.test.mcp.client, nyx.test.browser.pipe;

var
  GClient: TNyxMCPTestClient;
  GOther: TNyxMCPTestClient;
  GBrowser: TNyxBrowserPipe;
  GRevision: Integer;
  GChecks: Integer;
  GEvidence: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Authenticated project import: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Save(const AName, AText: TNyxText);
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(GEvidence + AName, fmCreate);
  try

    if AText <> '' then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
end;

function Call(const ATool: TNyxText; const AArgs: TNyxDataValue): TNyxDataValue;
var
  LReply: TNyxDataValue;
begin
  LReply := GClient.Tool(ATool, AArgs);

  if LReply.Field('isError').AsBoolean then
  begin
    Save('refusal-' + ATool + '.json', LReply.ToJSON);
    raise Exception.Create('Protocol operation refused: ' + ATool);
  end;
  Result := LReply.Field('structuredContent');
end;

function Args(const AMode, AOperation: TNyxText;
  const AExtra: array of TNyxDataField): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LBase: Integer;
  LIndex: Integer;
begin
  LBase := 2;

  if AOperation <> '' then
  begin
    Inc(LBase);
  end;
  SetLength(LFields, LBase + Length(AExtra));
  LFields[0] := NyxField('mode', NyxData(AMode));
  LFields[1] := NyxField('expectedRevision', NyxData(GRevision));

  if AOperation <> '' then
  begin
    LFields[2] := NyxField('operationId', NyxData(AOperation));
  end;
  for LIndex := 0 to High(AExtra) do
  begin
    LFields[LBase + LIndex] := AExtra[LIndex];
  end;
  Result := NyxObject(LFields);
end;

function ExportPacket: TNyxText;
var
  LParts: TNyxStrings;
  LReply: TNyxDataValue;
  LOffset: Integer;
begin
  LParts := TNyxStrings.Create;
  try
    LOffset := 0;
    repeat
      LReply := Call('nyx_project', Args('export', '', [NyxField('part', NyxData('project')),
        NyxField('offset', NyxData(LOffset)), NyxField('count', NyxData(4096))]));
      LParts.Add(LReply.Field('text').AsText);
      LOffset := LReply.Field('nextOffset').AsInteger;
    until LOffset = LReply.Field('total').AsInteger;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

procedure WaitSource(const ASource: TNyxText);
var
  LStarted: QWord;
  LActual: TNyxText;
begin
  LStarted := GetTickCount64;
  repeat

    if GBrowser.RuntimeError <> '' then
    begin
      GBrowser.CaptureRuntimeError;
      raise Exception.Create('Ordinary observing Studio failed at runtime');
    end;

    if GBrowser.TryFieldValue('[data-node="studio-code"]', LActual) and
      (LActual = ASource) then
    begin
      Check(True, 'Ordinary observing source is exact');
      Exit;
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      GBrowser.Capture('source-refusal');
      raise Exception.Create('Observing source did not reach the imported pair');
    end;
    Sleep(50);
  until False;
end;

procedure Build(ATarget: TNyxBuildTarget; const AOutput, ASource: TNyxText);
var
  LTarget: TNyxText;
  LJob: TNyxText;
  LStatus: TNyxDataValue;
  LStarted: QWord;
begin
  LTarget := NyxBuildTargetName(ATarget);
  LJob := Call('nyx_build', NyxObject([NyxField('mode', NyxData('request')),
    NyxField('operationId', NyxData('import-build-' + LTarget)),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('outputID', NyxData(AOutput)),
    NyxField('target', NyxData(LTarget)), NyxField('scope', NyxData('application'))])).Field('job').AsText;
  LStarted := GetTickCount64;
  repeat
    LStatus := Call('nyx_build', NyxObject([NyxField('mode', NyxData('status')),
      NyxField('job', NyxData(LJob)), NyxField('severity', NyxData('error')),
      NyxField('limit', NyxData(3))]));

    if GetTickCount64 - LStarted > 120000 then
    begin
      raise Exception.Create('Imported application compiler deadline exceeded');
    end;
    Sleep(100);
  until NyxBuildJobTerminal(ParseNyxBuildJobState(LStatus.Field('state').AsText));
  Save('build-' + LTarget + '.json', LStatus.ToJSON);
  Check((LStatus.Field('state').AsText = 'succeeded') and
    LStatus.Field('currentSource').AsBoolean and
    (LStatus.Field('sourceFingerprint').AsText = NyxBuildFingerprint(ASource)),
    'Compiler job uses the exact imported accepted source');
  WriteLn('Compiled imported application: ', LTarget);
  Flush(Output);
end;

procedure Run(const AConfiguration, AOrigin: TNyxText);
var
  LAuthor: TNyxStudioSession;
  LPair: TNyxProjectPair;
  LBefore: TNyxText;
  LPacket: TNyxText;
  LImport: TNyxText;
  LTicket: TNyxText;
  LOutput: TNyxText;
  LArgs: TNyxDataValue;
  LReply: TNyxDataValue;
  LIndex: Integer;
  LStart: Integer;
  LScalars: Integer;
  LScalar: Integer;
  LOffset: Integer;
  LSerial: Integer;
  LStarted: QWord;
begin
  GClient := TNyxMCPTestClient.Create(AConfiguration, 'Scooty project import qualification');
  GOther := TNyxMCPTestClient.Create(AConfiguration, 'Scooty project import qualification');
  LReply := GClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools');
  Check(LReply.Count = 23,
    'Fresh authenticated discovery advertises the new file contract');
  Check((LReply.Item(22).Field('name').AsText = 'nyx_project') and
    (LReply.Item(22).Field('inputSchema').Field('type').AsText = 'object') and
    (LReply.Item(22).Field('inputSchema').Field('oneOf').Count = 7),
    'Discovery retains an object schema and all seven closed file alternatives');
  LArgs := LReply.Item(22).Field('inputSchema').Field('oneOf');
  Check((LArgs.Item(0).Field('properties').Field('mode').Field('const').AsText = 'export') and
    (LArgs.Item(1).Field('properties').Field('mode').Field('const').AsText = 'begin-import') and
    (LArgs.Item(2).Field('properties').Field('mode').Field('const').AsText = 'append-import') and
    (LArgs.Item(3).Field('properties').Field('mode').Field('const').AsText = 'inspect-import') and
    (LArgs.Item(4).Field('properties').Field('mode').Field('const').AsText = 'review-import') and
    (LArgs.Item(5).Field('properties').Field('mode').Field('const').AsText = 'apply') and
    (LArgs.Item(6).Field('properties').Field('mode').Field('const').AsText = 'cancel-import'),
    'Schema alternatives retain independent mode declarations');
  GBrowser := TNyxBrowserPipe.Create(AOrigin + '/', GEvidence + 'observer', 1280, 960);
  LStarted := GetTickCount64;
  repeat

    if GBrowser.Exists('[data-node="action-code"]') then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      raise Exception.Create('Ordinary isolated Studio did not become ready');
    end;
    Sleep(50);
  until False;
  GBrowser.Click('[data-node="action-code"]');
  GRevision := Call('nyx_session', NyxObject([])).Field('revision').AsInteger;
  LBefore := ExportPacket;
  WaitSource(DecodeNyxProject(LBefore).Source);
  LAuthor := TNyxStudioSession.Create;
  try
    LAuthor.AddPage;
    LAuthor.Document.Title := 'A fresh start';
    LPair := LAuthor.ProjectSnapshot;
  finally
    LAuthor.Free;
  end;
  LPacket := EncodeNyxProject(LPair);
  Save('input.nyx', LPacket);
  LArgs := Args('begin-import', 'reserve-file', [NyxField('bytes', NyxData(NyxUTF8ByteCount(LPacket)))]);
  LReply := Call('nyx_project', LArgs);
  Check(Call('nyx_project', LArgs).ToJSON = LReply.ToJSON, 'HTTP reserve retry is exact');
  LImport := LReply.Field('projectImport').Field('import').AsText;
  Check(GOther.Tool('nyx_project', Args('inspect-import', '', [NyxField('import', NyxData(LImport))]))
    .Field('isError').AsBoolean, 'Same display actor with another transport cannot inspect input');
  LIndex := 1;
  LOffset := 0;
  LSerial := 0;
  while LIndex <= Length(LPacket) do
  begin
    LStart := LIndex;
    LScalars := 0;
    while (LIndex <= Length(LPacket)) and (LScalars < 4096) do
    begin

      if not NyxNextScalar(LPacket, LIndex, LScalar) then
      begin
        raise Exception.Create('Malformed authored qualification packet');
      end;
      Inc(LScalars);
    end;
    Inc(LSerial);
    LArgs := Args('append-import', 'chunk-' + IntToStr(LSerial), [NyxField('import', NyxData(LImport)),
      NyxField('offset', NyxData(LOffset)), NyxField('text', NyxData(Copy(LPacket, LStart, LIndex - LStart)))]);
    LReply := Call('nyx_project', LArgs);
    Check(Call('nyx_project', LArgs).ToJSON = LReply.ToJSON, 'HTTP chunk retry is exact');
    LOffset := LReply.Field('projectImport').Field('nextOffset').AsInteger;
  end;
  Check(ExportPacket = LBefore, 'HTTP staging preserves the exact active project');
  LArgs := Args('review-import', 'review-file', [NyxField('import', NyxData(LImport)),
    NyxField('resolution', NyxData('match'))]);
  LReply := Call('nyx_project', LArgs);
  Check(Call('nyx_project', LArgs).ToJSON = LReply.ToJSON, 'HTTP review retry retains ticket');
  LTicket := LReply.Field('projectImport').Field('reviewID').AsText;
  Check(LReply.Field('projectImport').Field('candidate').Field('pages').AsInteger = 2,
    'Small semantic review describes the candidate');
  LArgs := Args('apply', 'apply-file', [NyxField('import', NyxData(LImport)),
    NyxField('reviewID', NyxData(LTicket))]);
  LReply := Call('nyx_project', LArgs);
  GRevision := LReply.Field('revision').AsInteger;
  Check(Call('nyx_project', LArgs).ToJSON = LReply.ToJSON, 'HTTP apply retry retains one paired step');
  Check(ExportPacket = LPacket, 'Authenticated admitted pair equals the authored input');
  WaitSource(LPair.Source);
  GBrowser.Capture('imported-desktop');
  GBrowser.Resize(390, 844);
  GBrowser.Click('[data-node="action-expand-source"]');
  WaitSource(LPair.Source);
  GBrowser.Capture('imported-compact-source');
  GBrowser.Click('[data-node="action-expand-source"]');
  GBrowser.Resize(1280, 960);
  LOutput := Call('nyx_build', NyxObject([NyxField('mode', NyxData('outputs'))])).Field('outputID').AsText;
  Build(btBrowser, LOutput, LPair.Source);
  Build(btNativeLCL, LOutput, LPair.Source);
  LReply := Call('nyx_history', NyxObject([NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('undo-import')), NyxField('direction', NyxData('undo'))]));
  GRevision := LReply.Field('revision').AsInteger;
  Check(ExportPacket = LBefore, 'One authenticated Undo restores the previous complete pair');
  WaitSource(DecodeNyxProject(LBefore).Source);
  GBrowser.Capture('undo-desktop');
  LReply := Call('nyx_history', NyxObject([NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('redo-import')), NyxField('direction', NyxData('redo'))]));
  GRevision := LReply.Field('revision').AsInteger;
  Check(ExportPacket = LPacket, 'One authenticated Redo restores the imported complete pair');
  WaitSource(LPair.Source);
  GBrowser.Capture('redo-desktop');
  { Pressure the actual private slot budget, then retire its transport. A new
    connection's access refusal alone would not prove that memory was released. }
  for LIndex := 1 to 8 do
  begin
    LReply := GOther.Tool('nyx_project', Args('begin-import',
      'disconnect-reservation-' + IntToStr(LIndex), [NyxField('bytes', NyxData(1))]));
    Check(not LReply.Field('isError').AsBoolean, 'Live second owner reserves its bounded slot');
  end;
  Check(GClient.Tool('nyx_project', Args('begin-import', 'full-slots',
    [NyxField('bytes', NyxData(1))])).Field('isError').AsBoolean,
    'All eight private slots refuse another reservation');
  GOther.Close;
  LReply := Call('nyx_project', Args('begin-import', 'after-disconnect',
    [NyxField('bytes', NyxData(1))]));
  LImport := LReply.Field('projectImport').Field('import').AsText;
  Call('nyx_project', Args('cancel-import', 'cancel-after-disconnect',
    [NyxField('import', NyxData(LImport))]));
  Check(ExportPacket = LPacket, 'Actual disconnect releases all slots without changing the paired project');
  Save('final-session.json', Call('nyx_session', NyxObject([])).ToJSON);
end;

var
  LMarker: TNyxText;
  LFile: TFileStream;
  LRuntime: TNyxText;
begin
  try
    try

      if ParamCount <> 3 then
      begin
        raise Exception.Create('Supply isolated MCP config, loopback origin and NEW evidence directory');
      end;
      LRuntime := IncludeTrailingPathDelimiter(ExtractFileDir(ExtractFileDir(ExpandFileName(ParamStr(1)))));
      LFile := TFileStream.Create(LRuntime + '.local' + PathDelim + 'runtime-qualification.json', fmOpenRead);
      try
        Check((LFile.Size > 0) and (LFile.Size < 1024), 'Explicit isolated runtime marker is bounded');
        SetLength(LMarker, LFile.Size);
        LFile.ReadBuffer(LMarker[1], Length(LMarker));
      finally
        LFile.Free;
      end;
      Check((TNyxDataValue.ParseJSON(LMarker).Field('service').AsText =
        'nyx-resource-runtime-qualification') and
        (TNyxDataValue.ParseJSON(LMarker).Field('origin').AsText = TNyxText(ParamStr(2))) and
        (Pos('http://127.0.0.1:', ParamStr(2)) = 1), 'This destructive journey admits only its isolated origin');
      GEvidence := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));
      Check(not DirectoryExists(GEvidence), 'Evidence directory is new');
      ForceDirectories(GEvidence);
      Run(ParamStr(1), ParamStr(2));
      WriteLn('PASS ', GChecks, ' authenticated import/observer checks');
    except
      on LException: Exception do
      begin
        WriteLn(StdErr, LException.ClassName, ': ', LException.Message);
        ExitCode := 1;
      end;
    end;
  finally
    GBrowser.Free;

    if GOther <> nil then
    begin
      GOther.Close;
    end;
    GOther.Free;

    if GClient <> nil then
    begin
      GClient.Close;
    end;
    GClient.Free;
  end;
end.
