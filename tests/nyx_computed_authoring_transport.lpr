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

program nyx_computed_authoring_transport;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, fphttpserver, nyx.text, nyx.data, nyx.types, nyx.model, nyx.codec,
  nyx.codegen, nyx.binding.types, nyx.state, nyx.expressions, nyx.studio.stateedits,
  nyx.studio.mcp, nyx.studio.directories, nyx.studio.outputs,
  nyx.studio.projects, nyx.studio.agents, nyx.generated.view;

type
  { Borrow the production handler without starting its socket thread. This
    verifies current authentication, schema, dispatch and durable paired edits;
    socket framing and an installed release remain separate evidence. }
  TPreviewProtocolHost = class(TNyxStudioMCP)
  public
    procedure DeliverRequest(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
  end;

  TPreviewProtocolReply = record
    Status: Integer;
    Session: TNyxText;
    Packet: TNyxDataValue;
  end;

var
  LEngine: TPreviewProtocolHost;
  LProfile: TNyxOutputConfiguration;
  LSeed: TNyxAgentSession;
  LDocument: TNyxDocument;
  LToken: TNyxText;
  LAuthorization: TNyxText;
  LPath: TNyxText;
  LClient: TNyxText;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LRevision: Integer;
  LChecks: Integer;
  LClaim: TNyxDataValue;
  LArguments: TNyxDataValue;
  LReply: TPreviewProtocolReply;
  LExpression: INyxTextExpression;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Computed transport: ' + AReason);
  end;
  Inc(LChecks);
end;

procedure TPreviewProtocolHost.DeliverRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  Request(nil, ARequest, AResponse);
end;

function OwnedAuthorization(const APath: TNyxText): TNyxText;
var
  LFile: TFileStream;
  LText: TNyxText;
  LLines: TNyxStrings;
  LIndex: Integer;
  LLine: TNyxText;
begin
  Result := '';
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  LLines := nil;
  try
    SetLength(LText, LFile.Size);

    if LFile.Size > 0 then
    begin
      LFile.ReadBuffer(LText[1], LFile.Size);
    end;
    LLines := TNyxStrings.Create;
    LLines.Text := LText;
    for LIndex := 0 to LLines.Count - 1 do
    begin
      LLine := Trim(LLines[LIndex]);

      if Pos('http_headers = { Authorization = "', LLine) = 1 then
      begin
        Result := Copy(LLine, 35, Length(LLine) - 37);
        Break;
      end;
    end;
    Check(Pos('Bearer ', Result) = 1, 'owned host reads only its issued bearer');
  finally
    LLines.Free;
    LFile.Free;
  end;
end;

{ Construct the same FPC request/response objects borrowed by the listener.
  Production Reply writes UTF-8 bytes to its owned response stream. This host
  copies them before freeing both objects; socket framing is outside this test. }
function Dispatch(const AMethod, APath, AAuthorization, ASession: TNyxText;
  const APacket: TNyxDataValue; const AOrigin: TNyxText = '';
  const AProtocol: TNyxText = '2025-11-25';
  const AHost: TNyxText = '127.0.0.1:8649'): TPreviewProtocolReply;
var
  LRequest: TFPHTTPConnectionRequest;
  LResponse: TFPHTTPConnectionResponse;
  LText: TNyxText;
begin
  Result.Packet := NyxNull;
  LRequest := TFPHTTPConnectionRequest.Create;
  LResponse := nil;
  try
    LRequest.Method := AMethod;
    LRequest.PathInfo := APath;
    LRequest.ContentType := 'application/json';
    LRequest.Content := APacket.ToJSON;
    LRequest.SetFieldByName('Host', AHost);
    LRequest.SetFieldByName('Authorization', AAuthorization);
    LRequest.SetFieldByName('Mcp-Session-Id', ASession);
    LRequest.SetFieldByName('MCP-Protocol-Version', AProtocol);
    LRequest.SetFieldByName('Origin', AOrigin);
    LResponse := TFPHTTPConnectionResponse.Create(LRequest);
    LEngine.DeliverRequest(LRequest, LResponse);
    Result.Status := LResponse.Code;
    Result.Session := LResponse.CustomHeaders.Values['Mcp-Session-Id'];
    Check((LResponse.CustomHeaders.Values['Cache-Control'] = 'no-store') and
      (LResponse.CustomHeaders.Values['X-Content-Type-Options'] = 'nosniff'),
      'all protocol responses retain private-cache and content-type guards');

    if LResponse.ContentStream <> nil then
    begin
      Check(LResponse.ContentStream.Size <= 4 * 1024 * 1024,
        'protocol discovery/response remains within its transport budget');
      SetLength(LText, LResponse.ContentStream.Size);
      LResponse.ContentStream.Position := 0;

      if LText <> '' then
      begin
        LResponse.ContentStream.ReadBuffer(LText[1], Length(LText));
      end;
      Result.Packet := TNyxDataValue.ParseJSON(LText);
    end;
  finally
    LResponse.Free;
    LRequest.Free;
  end;
end;

function RPC(const AMethod: TNyxText; const AParameters: TNyxDataValue): TNyxDataValue;
begin
  Result := NyxObject([NyxField('jsonrpc', NyxData('2.0')),
    NyxField('id', NyxData(1)), NyxField('method', NyxData(AMethod)),
    NyxField('params', AParameters)]);
end;

function Observe: TNyxText;
begin
  Result := LEngine.EditorExchange(LToken, NyxObject([
    NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))])).Field('project').AsText;
end;

function Call(const ATool: TNyxText; const AArguments: TNyxDataValue): TPreviewProtocolReply;
begin
  Result := Dispatch('POST', LPath, LAuthorization, LClient,
    RPC('tools/call', NyxObject([NyxField('name', NyxData(ATool)),
      NyxField('arguments', AArguments)])));
end;

function Content(const AReply: TPreviewProtocolReply): TNyxDataValue;
begin
  Check((AReply.Status = 200) and
    not AReply.Packet.Field('result').Field('isError').AsBoolean,
    'authenticated tool dispatch succeeds');
  Result := AReply.Packet.Field('result').Field('structuredContent');
end;

function Revision: Integer;
begin
  Result := Content(Call('nyx_session', NyxObject([]))).Field('revision').AsInteger;
end;

procedure Verify;
var
  LPair: TNyxProjectPair;
  LTools: TNyxDataValue;
  LTool: TNyxDataValue;
  LIndex: Integer;
  LFound: Boolean;
begin
  Check(ParamCount = 1, 'supply a fresh owned suspended runtime');
  Check(not DirectoryExists(ParamStr(1)), 'existing runtime cannot be overwritten');
  LDocument := BuildNyxDocument;
  LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
  FreeAndNil(LDocument);
  LSeed := TNyxAgentSession.Create(LPair);
  LProfile := TNyxOutputConfiguration.Create;
  LEngine := TPreviewProtocolHost.Create(TNyxStudioDirectories.ForRepository(ParamStr(1)),
    8648, 8649, LProfile.Encode);
  LClaim := LEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
    NyxField('project', LSeed.Exchange(NyxObject([
      NyxField('op', NyxData('observe'))])).Field('project')),
    NyxField('selection', NyxData('order-total')), NyxField('view', NyxData('home'))]));
  LToken := LClaim.Field('token').AsText;
  LBefore := Observe;
  LAuthorization := OwnedAuthorization(IncludeTrailingPathDelimiter(ParamStr(1)) +
    '.codex' + PathDelim + 'config.toml');
  LPath := Copy(LEngine.Endpoint, Pos('/mcp/', LEngine.Endpoint), MaxInt);
  LArguments := RPC('initialize', NyxObject([
    NyxField('protocolVersion', NyxData('2025-11-25')),
    NyxField('capabilities', NyxObject([])),
    NyxField('clientInfo', NyxObject([NyxField('name', NyxData('Expression workshop')),
      NyxField('version', NyxData('1'))]))]));
  Check(Dispatch('POST', LPath, '', '', LArguments).Status = 401,
    'missing bearer cannot initialize');
  LReply := Dispatch('POST', LPath, LAuthorization, '', LArguments);
  LClient := LReply.Session;
  Check((LReply.Status = 200) and (LClient <> ''), 'owned transport initializes');
  Check(Dispatch('POST', LPath, LAuthorization, LClient, NyxObject([
    NyxField('jsonrpc', NyxData('2.0')),
    NyxField('method', NyxData('notifications/initialized'))])).Status = 202,
    'initialized notification admitted');
  LReply := Dispatch('POST', LPath, LAuthorization, LClient, RPC('tools/list', NyxObject([])));
  LTools := LReply.Packet.Field('result').Field('tools');
  LFound := False;
  for LIndex := 0 to LTools.Count - 1 do
  begin
    LTool := LTools.Item(LIndex);

    if LTool.Field('name').AsText = 'nyx_state' then
    begin
      LFound := True;
      Check(LTool.Field('inputSchema').Field('$defs').Field('NyxExpression')
        .Field('oneOf').Count = 9, 'authenticated discovery includes recursive expression shapes');
      Check(LTool.Field('inputSchema').Field('oneOf').Count = 6,
        'authenticated discovery includes bounded tree and source modes');
    end;
  end;
  Check(LFound and (LTools.Count = 24), 'computed authoring retained beside Logic discovery');
  LRevision := Revision;
  LExpression := NyxTextExpression(TNyxText('Total 🌙: ')).Append(
    NyxIntegerExpression(NyxIntegerState('quantity')).Times(NyxIntegerExpression(4)).AsText);
  LArguments := NyxObject([NyxField('mode', NyxData('apply')),
    NyxField('expectedRevision', NyxData(LRevision)),
    NyxField('operationId', NyxData('transport-computed')),
    NyxField('changes', NyxStateBindingPatch([NyxBindControl(
      NyxBindingOwner('order-total'), bpText, LExpression)]).ToData)]);
  Content(Call('nyx_state', LArguments));
  LAfter := Observe;
  Check((LAfter <> LBefore) and (Revision = LRevision + 1),
    'one authenticated typed mutation publishes one revision');
  Content(Call('nyx_state', LArguments));
  Check((Observe = LAfter) and (Revision = LRevision + 1),
    'exact mutation retry does not add another command');
  LArguments := NyxObject([NyxField('mode', NyxData('expression')),
    NyxField('expectedRevision', NyxData(Revision)), NyxField('owner', NyxData('order-total')),
    NyxField('target', NyxData('text')), NyxField('path', NyxArray([NyxData(0)])),
    NyxField('limit', NyxData(1))]);
  Check(Content(Call('nyx_state', LArguments)).Field('literal').Field('preview').AsText =
    TNyxText('Total 🌙: '), 'UTF-8 literal survives bounded authenticated node query');
  LArguments := NyxObject([NyxField('mode', NyxData('expression-source')),
    NyxField('expectedRevision', NyxData(Revision)), NyxField('owner', NyxData('order-total')),
    NyxField('target', NyxData('text')), NyxField('count', NyxData(20))]);
  Check(Content(Call('nyx_state', LArguments)).Field('source').AsText =
    Copy(TNyxCodegen.Expression(LExpression), 1, 20),
    'authenticated source returns only the requested scalar window');
  LArguments := NyxObject([NyxField('mode', NyxData('expression-source')),
    NyxField('expectedRevision', NyxData(LRevision)), NyxField('owner', NyxData('order-total')),
    NyxField('target', NyxData('text')), NyxField('count', NyxData(20))]);
  LReply := Call('nyx_state', LArguments);
  Check(LReply.Packet.Field('result').Field('isError').AsBoolean and (Observe = LAfter),
    'stale source query preserves exact pair');
  Content(Call('nyx_history', NyxObject([NyxField('direction', NyxData('undo')),
    NyxField('expectedRevision', NyxData(Revision)),
    NyxField('operationId', NyxData('transport-undo'))])));
  Check(Observe = LBefore, 'ordinary authenticated Undo restores exact design and Pascal');
  Content(Call('nyx_history', NyxObject([NyxField('direction', NyxData('redo')),
    NyxField('expectedRevision', NyxData(Revision)),
    NyxField('operationId', NyxData('transport-redo'))])));
  Check(Observe = LAfter, 'ordinary authenticated Redo restores complete expression');
  Check(Dispatch('DELETE', LPath, LAuthorization, LClient, NyxObject([])).Status = 200,
    'owned client disconnects without a listener');
end;

begin
  try
    Verify;
    WriteLn('PASS ', LChecks, ' computed authenticated handler checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  LExpression := nil;
  LDocument.Free;
  LEngine.Free;
  LProfile.Free;
  LSeed.Free;
end.
