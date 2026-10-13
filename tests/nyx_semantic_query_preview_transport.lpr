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



program nyx_semantic_query_preview_transport;
{$mode delphi}{$H+}{$codepage utf8}
uses
  Classes, SysUtils, fphttpserver, nyx.text, nyx.data, nyx.collections, nyx.collections.query,
  nyx.studio.mcp, nyx.studio.directories, nyx.studio.outputs,
  nyx.studio.projects, nyx.studio.agents, nyx.test.query.preview;

type
  { Serial host consumer of the production HTTP handler. The engine stays
    suspended: no socket is bound, and all requests retain actual admission. }
  TPreviewProtocolHost = class(TNyxStudioMCP)
  public
    { Borrow both objects for one serial call; neither may be retained. }
    procedure DeliverRequest(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
  end;

  { Copied response observations; no stream/request ownership escapes. }
  TPreviewProtocolReply = record
    Status: Integer;
    Session: TNyxText;
    Packet: TNyxDataValue;
  end;
var
  LEngine: TPreviewProtocolHost;
  LProfile: TNyxOutputConfiguration;
  LSeed: TNyxAgentSession;
  LPair: TNyxProjectPair;
  LClaim: TNyxDataValue;
  LToken: TNyxText;
  LBefore: TNyxText;
  LRevision: Integer;
  LTools: TNyxDataValue;
  LModes: TNyxDataValue;
  LProperties: TNyxDataValue;
  LReply: TNyxDataValue;
  LQuery: TNyxCollectionQuery;
  LIndex: Integer;
  LFound: Boolean;
  LChecks: Integer;
  LSections: Integer;
  LRejected: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Query preview transport: ' + AReason);
  end;
  Inc(LChecks);
end;

procedure TPreviewProtocolHost.DeliverRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  Request(nil, ARequest, AResponse);
end;

{ Read only this fresh owned runtime's generated credential. Never use the
  observing repository configuration or emit the capability in diagnostics. }
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

function Tool(const AArguments: TNyxDataValue): TNyxDataValue;
begin
  Result := LEngine.InvokeTool('nyx_collections', 'owned-preview-connection', 'Scooty', AArguments);
  Check(Observe = LBefore, 'ordinary durable wrapper preserves accepted pair and draft');
end;

procedure VerifyProtocol;
var
  LAuthorization: TNyxText;
  LPath: TNyxText;
  LClient: TNyxText;
  LOtherClient: TNyxText;
  LReview: TNyxText;
  LExpiredClient: TNyxText;
  LInitialize: TNyxDataValue;
  LCall: TNyxDataValue;
  LReply: TPreviewProtocolReply;
  LArguments: TNyxDataValue;
  LSnapshot: TNyxDataValue;
  LState: TNyxDataValue;
  LSection: Integer;
begin
  LAuthorization := OwnedAuthorization(IncludeTrailingPathDelimiter(ParamStr(1)) +
    '.codex' + PathDelim + 'config.toml');
  LPath := Copy(LEngine.Endpoint, Pos('/mcp/', LEngine.Endpoint), MaxInt);
  LInitialize := RPC('initialize', NyxObject([
    NyxField('protocolVersion', NyxData('2025-11-25')),
    NyxField('capabilities', NyxObject([])),
    NyxField('clientInfo', NyxObject([NyxField('name', NyxData('Owned query preview host')),
      NyxField('version', NyxData('1'))]))]));
  Check(Dispatch('POST', LPath, '', '', LInitialize).Status = 401,
    'missing bearer cannot initialize');
  Check(Dispatch('POST', LPath, LAuthorization, '', LInitialize,
    'https://untrusted.example').Status = 403, 'foreign origin cannot initialize');
  Check(Dispatch('POST', LPath, LAuthorization, '', LInitialize, '',
    '2025-11-25', 'untrusted.example:8649').Status = 403,
    'foreign host cannot initialize');
  Check(Dispatch('POST', '/mcp/expired', LAuthorization, '', LInitialize).Status = 404,
    'expired endpoint cannot initialize');
  Check(Dispatch('POST', LPath, LAuthorization, '', LInitialize, '',
    'unsupported').Status = 400, 'unsupported protocol refuses before dispatch');
  LReply := Dispatch('POST', LPath, LAuthorization, '', LInitialize);
  LClient := LReply.Session;
  Check((LReply.Status = 200) and (LClient <> '') and
    (LReply.Packet.Field('result').Field('protocolVersion').AsText = '2025-11-25'),
    'real handler issues private initialized-client authority');
  LReply := Dispatch('POST', LPath, LAuthorization, LClient, RPC('tools/list', NyxObject([])));
  Check(LReply.Packet.Field('error').Field('code').AsInteger = -32000,
    'tool discovery requires initialized notification');
  LReply := Dispatch('POST', LPath, LAuthorization, LClient,
    NyxObject([NyxField('jsonrpc', NyxData('2.0')),
      NyxField('method', NyxData('notifications/initialized'))]));
  Check(LReply.Status = 202, 'initialized notification is admitted');
  LReply := Dispatch('POST', LPath, LAuthorization, LClient, RPC('tools/list', NyxObject([])));
  Check(LReply.Packet.Field('result').Field('tools').ToJSON =
    NyxStudioMCPTools.Field('tools').ToJSON, 'authenticated discovery uses exact current schemas');
  LReply := Dispatch('POST', LPath, LAuthorization, LClient,
    RPC('tools/call', NyxObject([NyxField('name', NyxData('nyx_session')),
      NyxField('arguments', NyxObject([]))])));
  LState := LReply.Packet.Field('result').Field('structuredContent');
  Check(LState.Field('pendingDraft').AsBoolean and
    (LState.Field('revision').AsInteger = LRevision),
    'authenticated query context keeps the unfinished source revision');
  for LSection := 0 to 2 do
  begin
    case LSection of
      0:
      begin
        LArguments := NyxTestQueryPreviewArguments(LRevision, 'summary', LQuery);
      end;
      1:
      begin
        LArguments := NyxTestQueryPreviewArguments(LRevision, 'rows', LQuery)
          .WithField('limit', NyxData(1)).WithField('fields', NyxArray([NyxData('priority')]));
      end;
      2:
      begin
        LArguments := NyxTestQueryPreviewArguments(LRevision, 'pascal', LQuery)
          .WithField('count', NyxData(16));
      end;
    end;
    LCall := RPC('tools/call', NyxObject([NyxField('name', NyxData('nyx_collections')),
      NyxField('arguments', LArguments)]));
    LReply := Dispatch('POST', LPath, LAuthorization, LClient, LCall);
    Check((LReply.Status = 200) and
      not LReply.Packet.Field('result').Field('isError').AsBoolean,
      'each preview section runs through authenticated JSON-RPC tool dispatch');
    Check(Length(LReply.Packet.ToJSON) <= 49152,
      'each wrapped preview remains within the bounded context budget');
    case LSection of
      0:
      begin
        Check(LReply.Packet.Field('result').Field('structuredContent')
          .Field('visibleCount').AsInteger = 3, 'protocol returns exact unapplied count');
      end;
      1:
      begin
        Check(LReply.Packet.Field('result').Field('structuredContent').Field('rows')
          .Item(0).Field('values').Item(0).Field('value').Field('value').AsInteger = 27,
          'protocol returns the bounded requested field');
      end;
      2:
      begin
        Check(LReply.Packet.Field('result').Field('structuredContent').Field('pascal')
          .Field('text').AsText = 'NyxCollectionQue', 'protocol preserves Pascal scalar window');
      end;
    end;
    Check(Observe = LBefore, 'protocol preview retains the exact accepted/draft pair');
  end;
  LArguments := NyxTestQueryPreviewArguments(LRevision - 1, 'summary', LQuery);
  LCall := RPC('tools/call', NyxObject([NyxField('name', NyxData('nyx_collections')),
    NyxField('arguments', LArguments)]));
  LReply := Dispatch('POST', LPath, LAuthorization, LClient, LCall);
  Check((LReply.Status = 200) and LReply.Packet.Field('result').Field('isError').AsBoolean,
    'stale revision is a semantic refusal within a valid transport');
  LArguments := NyxTestQueryPreviewArguments(LRevision, 'summary',
    NyxCollectionQuery.Where(NyxWhere(NyxTextField('caption'))
      .Contains(TNyxText('🌙') + #0 + TNyxText('é'))));
  LCall := RPC('tools/call', NyxObject([NyxField('name', NyxData('nyx_collections')),
    NyxField('arguments', LArguments)]));
  LReply := Dispatch('POST', LPath, LAuthorization, LClient, LCall);
  Check(not LReply.Packet.Field('result').Field('isError').AsBoolean and
    (LReply.Packet.Field('result').Field('structuredContent').Field('visibleCount').AsInteger = 1),
    'UTF-8 supplementary/NUL query survives request bytes and semantic dispatch');
  LArguments := NyxTestQueryPreviewArguments(LRevision, 'rows', LQuery)
    .WithField('limit', NyxData(21));
  LCall := RPC('tools/call', NyxObject([NyxField('name', NyxData('nyx_collections')),
    NyxField('arguments', LArguments)]));
  LReply := Dispatch('POST', LPath, LAuthorization, LClient, LCall);
  Check(LReply.Packet.Field('result').Field('isError').AsBoolean,
    'protocol cannot exceed the published row budget');
  LEngine.EditorExchange(LToken, NyxObject([NyxField('op', NyxData('configure')),
    NyxField('permission', NyxData('readOnly'))]));
  LCall := RPC('tools/call', NyxObject([NyxField('name', NyxData('nyx_collections')),
    NyxField('arguments', NyxTestQueryPreviewArguments(LRevision, 'summary', LQuery))]));
  LReply := Dispatch('POST', LPath, LAuthorization, LClient, LCall);
  Check(not LReply.Packet.Field('result').Field('isError').AsBoolean,
    'operator read-only allows pure preview');
  LEngine.EditorExchange(LToken, NyxObject([NyxField('op', NyxData('configure')),
    NyxField('permission', NyxData('disabled'))]));
  LReply := Dispatch('POST', LPath, LAuthorization, LClient, LCall);
  Check(LReply.Packet.Field('result').Field('isError').AsBoolean,
    'operator disablement refuses preview');
  LEngine.EditorExchange(LToken, NyxObject([NyxField('op', NyxData('configure')),
    NyxField('permission', NyxData('edit'))]));
  LSnapshot := LEngine.EditorExchange(LToken, NyxObject([
    NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
  Check((Observe = LBefore) and
    (LSnapshot.Field('session').Field('canUndo').AsBoolean = LState.Field('canUndo').AsBoolean) and
    (LSnapshot.Field('session').Field('canRedo').AsBoolean = LState.Field('canRedo').AsBoolean),
    'protocol refusals and permissions preserve exact pair and history availability');
  { Identical visible actor names cannot substitute for transport ownership.
    The review copies accepted work; primary pending source stays untouched. }
  LReply := Dispatch('POST', LPath, LAuthorization, '', LInitialize);
  LOtherClient := LReply.Session;
  Check((LOtherClient <> '') and (LOtherClient <> LClient),
    'same actor name gets an independent transport authority');
  LReply := Dispatch('POST', LPath, LAuthorization, LOtherClient,
    NyxObject([NyxField('jsonrpc', NyxData('2.0')),
      NyxField('method', NyxData('notifications/initialized'))]));
  Check(LReply.Status = 202, 'second independent transport completes initialization');
  LCall := RPC('tools/call', NyxObject([NyxField('name', NyxData('nyx_reviews')),
    NyxField('arguments', NyxObject([NyxField('mode', NyxData('create')),
      NyxField('base', NyxData('accepted')), NyxField('label', NyxData('Owned preview')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('protocol-preview-review'))]))]));
  LReply := Dispatch('POST', LPath, LAuthorization, LClient, LCall);
  Check(not LReply.Packet.Field('result').Field('isError').AsBoolean,
    'authenticated semantic creation admits only an independently owned review');
  LReview := LReply.Packet.Field('result').Field('structuredContent').Field('review').AsText;
  LArguments := NyxTestQueryPreviewArguments(1, 'summary', LQuery)
    .WithField('review', NyxData(LReview));
  LCall := RPC('tools/call', NyxObject([NyxField('name', NyxData('nyx_collections')),
    NyxField('arguments', LArguments)]));
  LReply := Dispatch('POST', LPath, LAuthorization, LClient, LCall);
  Check(not LReply.Packet.Field('result').Field('isError').AsBoolean and
    (LReply.Packet.Field('result').Field('structuredContent').Field('visibleCount').AsInteger = 3),
    'owning transport previews its accepted review at its independent revision');
  LReply := Dispatch('POST', LPath, LAuthorization, LOtherClient, LCall);
  Check(LReply.Packet.Field('result').Field('isError').AsBoolean,
    'same actor name cannot inspect another transport private review');
  Check(Observe = LBefore, 'private-review preview leaves the full primary pair exact');
  Check(Dispatch('GET', LPath, LAuthorization, LClient, NyxObject([])).Status = 405,
    'optional stream remains explicitly unsupported');
  LExpiredClient := LClient;
  Check(Dispatch('DELETE', LPath, LAuthorization, LClient, NyxObject([])).Status = 200,
    'owning transport disconnect retires its authority');
  Check(Dispatch('POST', LPath, LAuthorization, LExpiredClient, LCall).Status = 404,
    'retired transport cannot reuse preview authority');
  LReply := Dispatch('POST', LPath, LAuthorization, LOtherClient, LCall);
  Check(LReply.Packet.Field('result').Field('isError').AsBoolean,
    'disconnect releases rather than transfers the private review');
  Check(Dispatch('DELETE', LPath, LAuthorization, LOtherClient, NyxObject([])).Status = 200,
    'second transport retires independently');
  Check(Observe = LBefore, 'complete protocol retirement leaves the primary pair exact');
end;

begin
  LEngine := nil;
  LProfile := nil;
  LSeed := nil;
  try
    LTools := NyxStudioMCPTools.Field('tools');
    LFound := False;
    for LIndex := 0 to LTools.Count - 1 do
    begin

      if LTools.Item(LIndex).Field('name').AsText = 'nyx_collections' then
      begin
        LModes := LTools.Item(LIndex).Field('inputSchema').Field('oneOf');
        LFound := True;
        Break;
      end;
    end;
    Check(LFound and (LModes.Count = 13), 'three strict preview sections extend existing ten modes');
    LSections := 0;
    for LIndex := 0 to LModes.Count - 1 do
    begin
      LProperties := LModes.Item(LIndex).Field('properties');

      if LProperties.Field('mode').Field('const').AsText = 'preview' then
      begin
        Inc(LSections);
        Check(not LModes.Item(LIndex).Field('additionalProperties').AsBoolean and
          NyxAgentHas(LProperties, 'workspace') and NyxAgentHas(LProperties, 'review'),
          'each closed preview variant keeps outer project routing');
        Check(LModes.Item(LIndex).Field('not').Field('required').Count = 2,
          'project and review are mutually exclusive');
        Check(LProperties.Field('expectedRevision').Field('minimum').AsInteger = 1,
          'every preview section requires a revision');
        Check(LProperties.Field('query').Field('anyOf').Item(1).Field('$ref').AsText =
          '#/$defs/nyxQuery', 'strict typed query definition is shared with mutation');

        if LProperties.Field('section').Field('const').AsText = 'rows' then
        begin
          Check((LProperties.Field('limit').Field('maximum').AsInteger = 20) and
            (LProperties.Field('fields').Field('maxItems').AsInteger = 16), 'row/field budgets');
        end;

        if LProperties.Field('section').Field('const').AsText = 'pascal' then
        begin
          Check(LProperties.Field('count').Field('maximum').AsInteger = 4096,
            'Pascal scalar window budget');
        end;
      end;
    end;
    Check(LSections = 3, 'all three preview sections are actually advertised');
    Check(ParamCount = 1, 'supply a fresh owned suspended runtime');
    Check(not DirectoryExists(ParamStr(1)), 'existing runtime cannot be overwritten');
    LPair := CreateNyxQueryPreviewSeed;
    LPair.Pending := True;
    LPair.DraftBase := LPair.Source;
    LPair.Draft := LPair.Source + TNyxText(#10 + '// Unfinished 🌙');
    LSeed := TNyxAgentSession.Create(LPair);
    LProfile := TNyxOutputConfiguration.Create;
    { Constructor is suspended: no listener/process or user enrollment is started.
      The real tool wrapper owns routing, activity, locking and durable recovery. }
    LEngine := TPreviewProtocolHost.Create(TNyxStudioDirectories.ForRepository(ParamStr(1)),
      8648, 8649, LProfile.Encode);
    LClaim := LEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', LSeed.Exchange(NyxObject([NyxField('op', NyxData('observe')),
        NyxField('after', NyxData(0))])).Field('project')),
      NyxField('selection', NyxData('tasks-table')), NyxField('view', NyxData('home'))]));
    LToken := LClaim.Field('token').AsText;
    LBefore := Observe;
    LRevision := LEngine.InvokeTool('nyx_session', 'owned-preview-connection', 'Scooty',
      NyxObject([])).Field('revision').AsInteger;
    LQuery := NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('priority')).AtLeast(27));
    LReply := Tool(NyxTestQueryPreviewArguments(LRevision, 'summary', LQuery));
    Check(LReply.Field('visibleCount').AsInteger = 3, 'real wrapper returns unapplied counts');
    LReply := Tool(NyxTestQueryPreviewArguments(LRevision, 'rows', LQuery)
      .WithField('limit', NyxData(1)).WithField('fields', NyxArray([NyxData('priority')])));
    Check((LReply.Field('rows').Count = 1) and
      (LReply.Field('rows').Item(0).Field('values').Item(0).Field('value')
        .Field('value').AsInteger = 27), 'wrapper returns only requested field page');
    LReply := Tool(NyxTestQueryPreviewArguments(LRevision, 'pascal', LQuery)
      .WithField('count', NyxData(16)));
    Check((LReply.Field('pascal').Field('text').AsText = 'NyxCollectionQue') and
      (LReply.Field('pascal').Field('nextOffset').AsInteger = 16), 'wrapper retains text window');
    LRejected := False;
    try
      Tool(NyxTestQueryPreviewArguments(LRevision - 1, 'summary', LQuery));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (Observe = LBefore), 'wrapper stale refusal retains complete pair');
    VerifyProtocol;
    FreeAndNil(LEngine);
    LEngine := TPreviewProtocolHost.Create(TNyxStudioDirectories.ForRepository(ParamStr(1)),
      8648, 8649, LProfile.Encode);
    LClaim := LEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(LBefore)), NyxField('selection', NyxData('tasks-table')),
      NyxField('view', NyxData('home'))]));
    LToken := LClaim.Field('token').AsText;
    Check(Observe = LBefore, 'owned durable recovery retains pending pair after previews');
    WriteLn('PASS ', LChecks, ' query preview discovery/server-dispatch/durable checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  LEngine.Free;
  LProfile.Free;
  LSeed.Free;
end.
