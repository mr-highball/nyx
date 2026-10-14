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
program nyx_logic_http_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.codegen,
  nyx.flow.types, nyx.flow, nyx.flow.declarations, nyx.flow.editing, nyx.logic,
  nyx.studio.agents, nyx.studio.projects, nyx.studio.mcp,
  nyx.studio.directories, nyx.studio.outputs, nyx.test.mcp.client,
  nyx.test.logic.fixture, nyx.test.logic.document;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Transaction(const AID: TNyxText; ARevision: Integer;
  const AChanges: array of TNyxDataValue): TNyxDataValue;
begin
  Result := NyxObject([NyxField('expectedRevision', NyxData(ARevision)),
    NyxField('operationId', NyxData(AID)), NyxField('operations', NyxArray(AChanges))]);
end;

var
  LEngine: TNyxStudioMCP;
  LClient: TNyxMCPTestClient;
  LProfile: TNyxOutputConfiguration;
  LSeed: TNyxAgentSession;
  LDoc: TNyxDocument;
  LObserved: TNyxDocument;
  LPair: TNyxProjectPair;
  LReply: TNyxDataValue;
  LArguments: TNyxDataValue;
  LToken: TNyxText;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LRuntime: TNyxText;
  LResult: INyxFlowResult;
  LRevision: Integer;
  LPort: Integer;
  LIndex: Integer;
  LFound: Boolean;
begin
  LEngine := nil;
  LClient := nil;
  LProfile := nil;
  LSeed := nil;
  LDoc := nil;
  LObserved := nil;
  try
    try

      if ParamCount <> 2 then
      begin
        raise Exception.Create('Supply a fresh owned runtime and unused MCP localhost port');
      end;
      LRuntime := TNyxText(ExpandFileName(ParamStr(1)));
      LPort := StrToInt(ParamStr(2));

      if DirectoryExists(LRuntime) then
      begin
        raise Exception.Create('HTTP Logic qualification never reuses a project runtime');
      end;
      LDoc := LogicDocument;
      LDoc.Flows.Define(NyxFlowRef('Profile'), LogicFixture);
      LPair := NyxProjectPair(TNyxCodec.Encode(LDoc), TNyxCodegen.Generate(LDoc));
      LBefore := EncodeNyxProject(LPair);
      LProfile := TNyxOutputConfiguration.Create;
      LEngine := TNyxStudioMCP.Create(TNyxStudioDirectories.ForRepository(LRuntime),
        LPort - 1, LPort, LProfile.Encode);
      LSeed := TNyxAgentSession.Create(LPair);
      LReply := LEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
        NyxField('project', LSeed.Exchange(NyxObject([
          NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))])).Field('project')),
        NyxField('selection', NyxData('source-memo')), NyxField('view', NyxData('home'))]));
      LToken := LReply.Field('token').AsText;
      LEngine.Start;
      { Observe this same owned listener's readiness. A failed connection retries
        transport initialization, never restarts an engine or another process. }
      for LIndex := 0 to 19 do
      begin
        try
          LClient := TNyxMCPTestClient.Create(LRuntime + '/.codex/config.toml', 'Scooty Logic qualification');
          Break;
        except
          on E: Exception do
          begin

            if LIndex = 19 then
            begin
              raise;
            end;
            Sleep(50);
          end;
        end;
      end;
      LReply := LClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools');
      LFound := False;
      for LIndex := 0 to LReply.Count - 1 do
      begin
        LFound := LFound or (LReply.Item(LIndex).Field('name').AsText = 'nyx_logic');
      end;
      Check(LFound and (LReply.Count = 24), 'authenticated HTTP discovery advertises focused Logic and existing tools');
      LReply := LClient.Tool('nyx_session', NyxObject([])).Field('structuredContent');
      LRevision := LReply.Field('revision').AsInteger;
      LReply := LClient.Tool('nyx_logic', NyxObject([
        NyxField('scope', NyxData('catalog')), NyxField('query', NyxData('format json')),
        NyxField('limit', NyxData(1))])).Field('structuredContent');
      Check((LReply.Field('items').Count = 1) and
        (LReply.Field('items').Item(0).Field('kind').AsText = 'json-format'),
        'actual HTTP catalog returns the requested bounded operation context');
      LArguments := Transaction('wire-format', LRevision, [
        TNyxDataValue.ParseJSON('{"op":"flow-step-add","flow":"Profile","step":"extra","kind":"json-format","configuration":{"layout":1,"indent":4}}'),
        NyxConnectFlow(NyxFlowRef('Profile'),
          NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpJSON)),
          NyxFlowEndpoint(NyxFlowStep('extra'), NyxLogicPort(nlpJSON))).ToData]);
      LClient.Tool('nyx_transaction', LArguments);
      LReply := LEngine.EditorExchange(LToken, NyxObject([
        NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
      LAfter := LReply.Field('project').AsText;
      LPair := DecodeNyxProject(LAfter);
      LObserved := TNyxCodec.Decode(LPair.Design);
      Check((LObserved.Flows.Definition(NyxFlowRef('Profile')).StepCount = 6) and
        (LObserved.Flows.Definition(NyxFlowRef('Profile')).WireCount = 5),
        'ordinary operator observation sees the exact HTTP-authored graph');
      LResult := LObserved.Flows.Definition(NyxFlowRef('Profile')).Build.Evaluate([
        NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpText)), NyxFlowText(LogicExample)),
        NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('summary'), NyxLogicPort(nlpLeft)), NyxFlowText('Profile'))]);
      Check(Pos('English demo', LResult.Value(NyxFlowEndpoint(NyxFlowStep('extra'),
        NyxLogicPort(nlpText))).AsText) > 0, 'HTTP-authored operations produce functional output');
      LClient.Tool('nyx_transaction', LArguments);
      Check(LEngine.EditorExchange(LToken, NyxObject([
        NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]))
        .Field('project').AsText = LAfter, 'authenticated exact retry retains one grouped pair');
      LReply := LClient.Tool('nyx_session', NyxObject([])).Field('structuredContent');
      Check(LReply.Field('revision').AsInteger = LRevision + 1, 'one grouped HTTP edit adds exactly one revision');
      LRevision := LReply.Field('revision').AsInteger;
      LClient.Tool('nyx_history', NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
        NyxField('operationId', NyxData('http-undo')), NyxField('direction', NyxData('undo'))]));
      Check(LEngine.EditorExchange(LToken, NyxObject([
        NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]))
        .Field('project').AsText = LBefore, 'ordinary observer sees exact paired HTTP Undo');
      LReply := LClient.Tool('nyx_session', NyxObject([])).Field('structuredContent');
      LClient.Tool('nyx_history', NyxObject([NyxField('expectedRevision', LReply.Field('revision')),
        NyxField('operationId', NyxData('http-redo')), NyxField('direction', NyxData('redo'))]));
      Check(LEngine.EditorExchange(LToken, NyxObject([
        NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]))
        .Field('project').AsText = LAfter, 'ordinary observer sees exact paired HTTP Redo');
      LEngine.EditorExchange(LToken, NyxObject([
        NyxField('op', NyxData('configure')), NyxField('permission', NyxData('readOnly'))]));
      LReply := LClient.Tool('nyx_session', NyxObject([])).Field('structuredContent');
      LArguments := Transaction('forbidden-delete', LReply.Field('revision').AsInteger,
        [NyxRemoveFlow(NyxFlowRef('Profile')).ToData]);
      LReply := LClient.Tool('nyx_transaction', LArguments);
      Check(LReply.Field('isError').AsBoolean, 'operator read-only policy refuses HTTP Logic mutation');
      Check(LEngine.EditorExchange(LToken, NyxObject([
        NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]))
        .Field('project').AsText = LAfter, 'permission refusal retains the complete pair');
      LReply := LClient.Tool('nyx_logic', NyxObject([NyxField('scope', NyxData('steps')),
        NyxField('flow', NyxData('Profile')), NyxField('limit', NyxData(2))])).Field('structuredContent');
      Check((LReply.Field('items').Count = 2) and (LReply.Field('total').AsInteger = 6),
        'read-only agents retain bounded Logic inspection');
      LClient.Close;
    finally
      LClient.Free;
      LEngine.Free;
      LProfile.Free;
      LSeed.Free;
      LObserved.Free;
      LDoc.Free;
    end;
    WriteLn('PASS ', GChecks, ' authenticated HTTP Logic checks; owned listener retired');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
