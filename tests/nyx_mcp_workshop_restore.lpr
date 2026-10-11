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

program nyx_mcp_workshop_restore;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.studio.projects, nyx.studio.mcpconfig,
  nyx.studio.workspaces,
  nyx.test.mcp.client;

{ Restore only an explicitly owned qualification workshop through public MCP
  project import. The caller first audits all protected contexts separately.
  This consumer requires an exact current source/revision without a draft, then
  reviews and adopts the saved complete pair as one ordinary Undo command.
  It never resets revisions/history, touches recovery files or stops a server.
  One transport owns the upload and review ticket until terminal publication.
  Source admission is not application compilation or execution evidence. }

var
  GClient: TNyxMCPTestClient;
  GWorkspace: TNyxText;
  GRevision: Integer;
  GOperation: TNyxText;

procedure Require(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
end;

{ Closed property strings belong to this explicit protocol boundary. Every call
  carries the named workspace; there is no implicit primary-project fallback. }
function Call(const ATool: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
var
  LReply: TNyxDataValue;
begin
  LReply := GClient.Tool(ATool, NyxWithWorkspace(AArguments, NyxWorkspace(GWorkspace)));
  Require(not LReply.Field('isError').AsBoolean, 'Semantic workshop operation refused: ' + ATool);
  Result := LReply.Field('structuredContent');
end;

function Args(const AMode, AOperation: TNyxText;
  const AExtra: array of TNyxDataField): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LBase: Integer;
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
    LFields[2] := NyxField('operationId', NyxData(GOperation + AOperation));
  end;
  for LIndex := 0 to High(AExtra) do
  begin
    LFields[LBase + LIndex] := AExtra[LIndex];
  end;
  Result := NyxObject(LFields);
end;

{ Bounded scalar windows retain exact UTF-8 source/project bytes. Reject a
  non-progressing response rather than looping forever on malformed transport. }
function ExportPart(const APart: TNyxText): TNyxText;
var
  LReply: TNyxDataValue;
  LOffset: Integer;
  LNext: Integer;
begin
  Result := '';
  LOffset := 0;
  repeat
    LReply := Call('nyx_project', Args('export', '', [NyxField('part', NyxData(APart)),
      NyxField('offset', NyxData(LOffset)), NyxField('count', NyxData(4096))]));
    Result := Result + LReply.Field('text').AsText;
    LNext := LReply.Field('nextOffset').AsInteger;
    Require((LNext > LOffset) or (LNext = LReply.Field('total').AsInteger),
      'Export did not advance');
    Require(Length(Result) <= 4 * 1024 * 1024, 'Export exceeds the project byte bound');
    LOffset := LNext;
  until LOffset = LReply.Field('total').AsInteger;
end;

procedure Run;
var
  LBaseline: TNyxDataValue;
  LExpected: TNyxDataValue;
  LReply: TNyxDataValue;
  LPair: TNyxProjectPair;
  LPacket: TNyxText;
  LImport: TNyxText;
  LTicket: TNyxText;
  LID: TGUID;
  LMatches: Integer;
  LIndex: Integer;
  LStart: Integer;
  LCount: Integer;
  LScalar: Integer;
  LOffset: Integer;
  LSerial: Integer;
begin
  Require(ParamCount = 5,
    'Supply configuration, nine-context baseline, owned workspace, exact revision and current source');
  GWorkspace := TNyxText(ParamStr(3));
  Require(GWorkspace <> '', 'Restoration must never address the primary project');
  GRevision := StrToInt(ParamStr(4));
  Require(GRevision > 0, 'An exact nonzero revision is required');
  LBaseline := TNyxDataValue.ParseJSON(NyxMCPReadBytes(TNyxText(ParamStr(2))));
  Require((LBaseline.Kind = ndArray) and (LBaseline.Count = 9),
    'The saved complete nine-context baseline is required');
  LMatches := 0;
  for LIndex := 0 to LBaseline.Count - 1 do
  begin

    if LBaseline.Item(LIndex).Field('workspace').AsText = GWorkspace then
    begin
      Require((LIndex > 0) and
        (LBaseline.Item(LIndex).Field('label').AsText = 'Manual presentations'),
        'Only the explicit owned Manual presentations workshop may be restored');
      LExpected := LBaseline.Item(LIndex);
      Inc(LMatches);
    end;
  end;
  Require(LMatches = 1, 'The owned baseline must have exactly one matching identity');
  LPacket := LExpected.Field('project').AsText;
  LPair := DecodeNyxProject(LPacket);
  Require(not LPair.Pending, 'This qualification cleanup does not resolve saved drafts');
  Require(CreateGUID(LID) = 0, 'Could not create unique operation identities');
  GOperation := 'workshop-' + TNyxText(GUIDToString(LID)) + '-';
  GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty undoable workshop restoration');
  LReply := Call('nyx_session', NyxObject([]));
  Require((LReply.Field('revision').AsInteger = GRevision) and
    not LReply.Field('pendingDraft').AsBoolean, 'Current workshop revision/draft changed');
  Require(ExportPart('source') = NyxMCPReadBytes(TNyxText(ParamStr(5))),
    'Current workshop is not the exact expected qualification source');
  LImport := Call('nyx_project', Args('begin-import', 'begin', [
    NyxField('bytes', NyxData(Length(LPacket)))])).Field('projectImport').Field('import').AsText;
  LIndex := 1;
  LOffset := 0;
  LSerial := 0;
  while LIndex <= Length(LPacket) do
  begin
    LStart := LIndex;
    LCount := 0;
    while (LIndex <= Length(LPacket)) and (LCount < 4096) do
    begin
      Require(NyxNextScalar(LPacket, LIndex, LScalar), 'Saved project contains malformed UTF-8');
      Inc(LCount);
    end;
    Inc(LSerial);
    LReply := Call('nyx_project', Args('append-import', 'chunk-' + IntToStr(LSerial), [
      NyxField('import', NyxData(LImport)), NyxField('offset', NyxData(LOffset)),
      NyxField('text', NyxData(Copy(LPacket, LStart, LIndex - LStart)))]));
    LOffset := LReply.Field('projectImport').Field('nextOffset').AsInteger;
  end;
  Require(LReply.Field('projectImport').Field('complete').AsBoolean,
    'Complete saved project did not arrive');
  LReply := Call('nyx_project', Args('inspect-import', '', [
    NyxField('import', NyxData(LImport))]));
  Require(LReply.Field('complete').AsBoolean and
    (LReply.Field('receivedBytes').AsInteger = Length(LPacket)),
    'Inspected input is not the complete exact saved byte count');
  LReply := Call('nyx_project', Args('review-import', 'review', [
    NyxField('import', NyxData(LImport)), NyxField('resolution', NyxData('match'))]));
  Require(not LReply.Field('projectImport').Field('candidate').Field('pendingDraft').AsBoolean,
    'Reviewed restoration introduced a pending draft');
  LTicket := LReply.Field('projectImport').Field('reviewID').AsText;
  Require(LTicket <> '', 'Exact import review ticket is required');
  LReply := Call('nyx_project', Args('apply', 'apply', [NyxField('import', NyxData(LImport)),
    NyxField('reviewID', NyxData(LTicket))]));
  GRevision := LReply.Field('revision').AsInteger;
  Require(ExportPart('project') = LPacket, 'Restored complete design/Pascal pair differs');
  LReply := Call('nyx_session', NyxObject([]));
  Require((LReply.Field('selection').AsText = LExpected.Field('selection').AsText) and
    (LReply.Field('view').AsText = LExpected.Field('view').AsText) and
    not LReply.Field('pendingDraft').AsBoolean, 'Restored navigation/draft differs');
  WriteLn('PASS exact owned saved pair/navigation restored through one undoable MCP import; revision ',
    GRevision, '. Qualification history is retained.');
end;

begin
  GClient := nil;
  try
    try
      Run;
    finally

      if GClient <> nil then
      begin
        { Refusal and success both retire upload authority; exceptions must not
          leave a live actor-owned import or transport on the observing server. }
        try
          GClient.Close;
        finally
          GClient.Free;
        end;
      end;
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
