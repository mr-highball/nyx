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

program nyx_agent_column_move_http;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.test.mcp.client;

{ Explicitly invoked against the existing public Service monitor fixture.
  Arguments are private Codex configuration, exact workspace and expected
  revision. This consumer creates no workspace/listener and never prints grants.
  One retained connection qualifies receipts; a separate connection must not
  inherit that authority. Successful completion restores the exact starting pair
  through ordinary Undo while revisions/history advance normally. }
var
  GClient: TNyxMCPTestClient;
  GOther: TNyxMCPTestClient;
  GWorkspace: TNyxText;
  GRevision: Integer;
  GChecks: Integer;
  GBefore: TNyxText;
  GMoved: TNyxText;
  GRequest: TNyxDataValue;
  GReply: TNyxDataValue;
  GRetry: TNyxDataValue;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Named move HTTP: ' + AReason);
  end;
  Inc(GChecks);
end;

function Accepted(const ATool: TNyxText;
  const AArguments: TNyxDataValue): TNyxDataValue;
var
  LResult: TNyxDataValue;
begin
  LResult := GClient.Tool(ATool, AArguments);
  Check(not LResult.Field('isError').AsBoolean, 'ordinary request accepted: ' + ATool);
  Result := LResult.Field('structuredContent');
end;

{ Export only the addressed accepted pair, with revision-qualified bounded
  windows. No runtime/editor APIs or direct durable-store access substitute for
  the public semantic boundary. Reassembled text stays owned by this consumer. }
function Pair: TNyxText;
var
  LWindow: TNyxDataValue;
  LOffset: Integer;
begin
  Result := '';
  LOffset := 0;
  repeat
    LWindow := Accepted('nyx_project', NyxObject([
      NyxField('workspace', NyxData(GWorkspace)), NyxField('mode', NyxData('export')),
      NyxField('part', NyxData('project')), NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('offset', NyxData(LOffset)), NyxField('count', NyxData(4096))]));
    Check((LWindow.Field('revision').AsInteger = GRevision) and
      (LWindow.Field('offset').AsInteger = LOffset) and
      (LWindow.Field('nextOffset').AsInteger > LOffset) and
      (LWindow.Field('total').AsInteger <= 8 * 1024 * 1024), 'exact advancing export');
    Result := Result + LWindow.Field('text').AsText;
    LOffset := LWindow.Field('nextOffset').AsInteger;
  until LOffset = LWindow.Field('total').AsInteger;
end;

procedure History(const ADirection, AID: TNyxText);
var
  LResult: TNyxDataValue;
begin
  LResult := Accepted('nyx_history', NyxObject([
    NyxField('workspace', NyxData(GWorkspace)), NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(AID)), NyxField('direction', NyxData(ADirection))]));
  Check(LResult.Field('revision').AsInteger = GRevision + 1, 'one ordinary history revision');
  GRevision := LResult.Field('revision').AsInteger;
end;

begin
  GClient := nil;
  GOther := nil;
  try
    Check(ParamCount = 3, 'configuration, exact workspace and expected revision required');
    GWorkspace := TNyxText(ParamStr(2));
    GRevision := StrToInt(ParamStr(3));
    GClient := TNyxMCPTestClient.Create(TNyxText(ParamStr(1)), 'Scooty named column regression');
    try
      GReply := Accepted('nyx_session', NyxObject([NyxField('workspace', NyxData(GWorkspace))]));
      Check((GReply.Field('revision').AsInteger = GRevision) and
        (GReply.Field('title').AsText = 'Service monitor') and
        not GReply.Field('pendingDraft').AsBoolean, 'exact disposable fixture with no Pascal draft');
      GBefore := Pair;
      GRequest := NyxObject([NyxField('workspace', NyxData(GWorkspace)),
        NyxField('mode', NyxData('apply')), NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('operationId', NyxData('named-column-http-move')),
        NyxField('changes', NyxArray([NyxObject([
          NyxField('op', NyxData('move-column')), NyxField('owner', NyxData('service-table')),
          NyxField('key', NyxData('services')), NyxField('projection', NyxData('table')),
          NyxField('field', NyxData('notes')), NyxField('kind', NyxData('text')),
          NyxField('direction', NyxData('earlier'))])]))]);
      GReply := Accepted('nyx_collections', GRequest);
      Check(GReply.Field('revision').AsInteger = GRevision + 1, 'one named move publication');
      GRevision := GReply.Field('revision').AsInteger;
      GMoved := Pair;
      Check(GMoved <> GBefore, 'named movement changes the actual accepted pair');
      GRetry := Accepted('nyx_collections', GRequest);
      Check((GRetry.ToJSON = GReply.ToJSON) and (Pair = GMoved),
        'same authenticated connection returns the exact original receipt without moving again');
      GOther := TNyxMCPTestClient.Create(TNyxText(ParamStr(1)), 'Scooty named column regression');
      try
        GRetry := GOther.Tool('nyx_collections', GRequest);
        Check(GRetry.Field('isError').AsBoolean and (Pair = GMoved),
          'same display name on another connection cannot inherit mutation receipts');
      finally
        GOther.Close;
        FreeAndNil(GOther);
      end;
      History('undo', 'named-column-http-undo');
      Check(Pair = GBefore, 'one Undo restores exact source, design and draft bytes');
      History('redo', 'named-column-http-redo');
      Check(Pair = GMoved, 'one Redo restores the exact moved pair');
      History('undo', 'named-column-http-restore');
      Check(Pair = GBefore, 'final ordinary Undo preserves the starting accepted pair');
      WriteLn('PASS ', GChecks, ' named column HTTP checks; final revision ', GRevision);
    finally
      GClient.Close;
      FreeAndNil(GClient);
    end;
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
