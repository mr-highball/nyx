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
program nyx_mcp_import_retirement;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.studio.mcpconfig, nyx.test.mcp.client;

var
  GWorkspace: TNyxText;
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

function Operation: TNyxText;
var
  LID: TGUID;
begin

  if CreateGUID(LID) <> 0 then
  begin
    raise Exception.Create('Could not create a qualification operation identity');
  end;
  Result := TNyxText(GUIDToString(LID));
end;

{ Context is always explicit. Staging mutations receive fresh identities while
  read-only inspection/export must omit operationId under the actual schema. }
function Args(const AMode, AOperation: TNyxText;
  const AExtra: array of TNyxDataField): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LBase: Integer;
  LIndex: Integer;
begin
  LBase := 3;

  if AOperation <> '' then
  begin
    Inc(LBase);
  end;
  SetLength(LFields, LBase + Length(AExtra));
  LFields[0] := NyxField('mode', NyxData(AMode));
  LFields[1] := NyxField('workspace', NyxData(GWorkspace));
  LFields[2] := NyxField('expectedRevision', NyxData(GRevision));

  if AOperation <> '' then
  begin
    LFields[3] := NyxField('operationId', NyxData(AOperation));
  end;
  for LIndex := 0 to High(AExtra) do
  begin
    LFields[LBase + LIndex] := AExtra[LIndex];
  end;
  Result := NyxObject(LFields);
end;

function Call(AClient: TNyxMCPTestClient; const ATool: TNyxText;
  const AArguments: TNyxDataValue): TNyxDataValue;
var
  LReply: TNyxDataValue;
begin
  LReply := AClient.Tool(ATool, AArguments);

  if LReply.Field('isError').AsBoolean then
  begin
    raise Exception.Create('Protocol operation refused: ' + LReply.ToJSON);
  end;
  Result := LReply.Field('structuredContent');
end;

{ Exact before/after evidence is assembled from bounded scalar windows. It is
  never printed as a document dump and a stalled cursor cannot spin forever. }
function ExportPair(AClient: TNyxMCPTestClient): TNyxText;
var
  LParts: TNyxStrings;
  LReply: TNyxDataValue;
  LOffset: Integer;
  LNext: Integer;
begin
  LParts := TNyxStrings.Create;
  try
    LOffset := 0;
    repeat
      LReply := Call(AClient, 'nyx_project', Args('export', '', [
        NyxField('part', NyxData('project')), NyxField('offset', NyxData(LOffset)),
        NyxField('count', NyxData(4096))]));
      LParts.Add(LReply.Field('text').AsText);
      LNext := LReply.Field('nextOffset').AsInteger;

      if (LNext < LReply.Field('total').AsInteger) and (LNext <= LOffset) then
      begin
        raise Exception.Create('Project export made no progress');
      end;
      LOffset := LNext;
    until LOffset = LReply.Field('total').AsInteger;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

procedure Run;
var
  LConfiguration: TNyxText;
  LPacket: TNyxText;
  LBefore: TNyxText;
  LImport: TNyxText;
  LCandidate: TNyxDataValue;
  LJobsBefore: TNyxDataValue;
  LReply: TNyxDataValue;
  LClient: TNyxMCPTestClient;
  LOther: TNyxMCPTestClient;
  LIndex: Integer;
  LStart: Integer;
  LScalar: Integer;
  LChunkCount: Integer;
  LOffset: Integer;
begin
  Check(ParamCount = 4,
    'Supply enrollment root, explicit workspace, revision and literal project file');
  GWorkspace := TNyxText(ParamStr(2));
  GRevision := StrToInt(ParamStr(3));
  Check((GWorkspace <> '') and (GRevision > 0), 'An explicit project context is required');
  LConfiguration := IncludeTrailingPathDelimiter(ParamStr(1)) + '.codex' + PathDelim + 'config.toml';
  LPacket := NyxMCPReadBytes(TNyxText(ParamStr(4)));
  Check((Length(LPacket) > 0) and (Length(LPacket) <= 4194304), 'Qualification input fits the wire budget');
  LClient := nil;
  LOther := nil;
  try
    LClient := TNyxMCPTestClient.Create(LConfiguration, 'Scooty import lifecycle checks');
    LOther := TNyxMCPTestClient.Create(LConfiguration, 'Scooty import lifecycle checks');
    LBefore := ExportPair(LOther);
    LJobsBefore := Call(LOther, 'nyx_build', NyxObject([
      NyxField('mode', NyxData('jobs')), NyxField('workspace', NyxData(GWorkspace)),
      NyxField('filter', NyxData('all')), NyxField('limit', NyxData(16))]));
    { Occupy the actual eight-slot budget. A new connection's access refusal
      alone would not prove that disconnect released private reservations. }
    for LIndex := 1 to 8 do
    begin
      LReply := Call(LClient, 'nyx_project', Args('begin-import', Operation,
        [NyxField('bytes', NyxData(1))]));
      Check(LReply.Field('projectImport').Field('receivedBytes').AsInteger = 0,
        'A live owner reserves its private upload slot');
    end;
    LReply := LOther.Tool('nyx_project', Args('begin-import', Operation,
      [NyxField('bytes', NyxData(1))]));
    Check(LReply.Field('isError').AsBoolean, 'Eight occupied slots refuse a ninth reservation');
    LClient.Close;
    FreeAndNil(LClient);
    LReply := Call(LOther, 'nyx_project', Args('begin-import', Operation,
      [NyxField('bytes', NyxData(Length(LPacket)))]));
    LImport := LReply.Field('projectImport').Field('import').AsText;
    Check(LImport <> '', 'Disconnect releases the occupied slot budget');
    LIndex := 1;
    LOffset := 0;
    while LIndex <= Length(LPacket) do
    begin
      LStart := LIndex;
      LChunkCount := 0;
      while (LIndex <= Length(LPacket)) and (LChunkCount < 1024) do
      begin

        if not NyxNextScalar(LPacket, LIndex, LScalar) then
        begin
          raise Exception.Create('Qualification input is not UTF-8');
        end;
        Inc(LChunkCount);
      end;
      LReply := Call(LOther, 'nyx_project', Args('append-import', Operation, [
        NyxField('import', NyxData(LImport)), NyxField('offset', NyxData(LOffset)),
        NyxField('text', NyxData(Copy(LPacket, LStart, LIndex - LStart)))]));
      LOffset := LReply.Field('projectImport').Field('nextOffset').AsInteger;
    end;
    LReply := Call(LOther, 'nyx_project', Args('review-import', Operation, [
      NyxField('import', NyxData(LImport)), NyxField('resolution', NyxData('match'))]));
    Check((LReply.Field('revision').AsInteger = GRevision) and
      (LReply.Field('projectImport').Field('reviewID').AsText <> ''),
      'Omitted admission returns a literal review ticket without publication');
    LCandidate := LReply.Field('projectImport').Field('candidate');
    LReply := Call(LOther, 'nyx_project', Args('review-import', Operation, [
      NyxField('import', NyxData(LImport)), NyxField('resolution', NyxData('match')),
      NyxField('admission', NyxData('literal'))]));
    Check(LReply.Field('projectImport').Field('candidate').ToJSON = LCandidate.ToJSON,
      'Explicit and default literal admission have identical resolved meaning');
    LReply := LOther.Tool('nyx_project', Args('review-import', Operation, [
      NyxField('import', NyxData(LImport)), NyxField('resolution', NyxData('match')),
      NyxField('admission', NyxData('unsupported'))]));
    Check(LReply.Field('isError').AsBoolean, 'Unsupported admission refuses before execution');
    LOther.Close;
    FreeAndNil(LOther);
    LClient := TNyxMCPTestClient.Create(LConfiguration, 'Scooty import lifecycle checks');
    LReply := LClient.Tool('nyx_project', Args('inspect-import', '',
      [NyxField('import', NyxData(LImport))]));
    Check(LReply.Field('isError').AsBoolean, 'The disconnected ready ticket is retired');
    LReply := Call(LClient, 'nyx_build', NyxObject([
      NyxField('mode', NyxData('jobs')), NyxField('workspace', NyxData(GWorkspace)),
      NyxField('filter', NyxData('all')), NyxField('limit', NyxData(16))]));
    Check((LReply.Field('total').AsInteger = LJobsBefore.Field('total').AsInteger) and
      (LReply.Field('items').ToJSON = LJobsBefore.Field('items').ToJSON),
      'Literal/default/refused review preserves the complete retained compiler-job set');
    Check(ExportPair(LClient) = LBefore, 'Every staging/refusal/disconnect preserves the exact accepted pair');
  finally

    if LOther <> nil then
    begin
      LOther.Close;
    end;
    LOther.Free;

    if LClient <> nil then
    begin
      LClient.Close;
    end;
    LClient.Free;
  end;
  WriteLn('PASS ', GChecks, ' scoped MCP import/default/retirement checks / no publication');
end;

begin
  try
    Run;
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
end.
