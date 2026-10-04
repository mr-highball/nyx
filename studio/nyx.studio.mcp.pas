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

unit nyx.studio.mcp;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, SyncObjs, fphttpserver, httpdefs, Process, base64,
  nyx.text, nyx.data, nyx.studio.agents, nyx.studio.projects, nyx.studio.buildjobs;

type
  TNyxMCPHTTP = class(TFPHTTPServer)
  public
    property Address;
  end;

  { Separate localhost listener shares one guarded authoritative Studio session
    with the LAN/editor server. HTTP transport is JSON-RPC Streamable HTTP with
    JSON responses and explicit 405 for optional GET streams. Transport sessions
    identify initialized clients; the design session is independently scoped by
    its unguessable endpoint and bearer capability. No SDK/runtime dependency. }
  TNyxStudioMCP = class(TThread)
  private
    FHTTP: TNyxMCPHTTP;
    FGuard: TCriticalSection;
    FCore: TNyxAgentSession;
    FBuilds: TNyxBuildJobs;
    FID: TNyxText;
    FToken: TNyxText;
    FEditorToken: TNyxText;
    FRepository: TNyxText;
    FPort: Integer;
    FStudioPort: Integer;
    FClients: array of TNyxDataValue;
    FPreviews: array of TNyxDataValue;
    FFailure: TNyxText;
    FConfigurationIssue: TNyxText;
    procedure Request(ASender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure WriteCodexConfiguration;
    function Tools: TNyxDataValue;
    function Preview(const AArguments: TNyxDataValue;
      const AActor: TNyxText): TNyxDataValue;
    function CapturePreview(const APreview: TNyxDataValue): TNyxDataValue;
    function ClientIndex(const AID: TNyxText): Integer;
    function BuildTool(const AArguments: TNyxDataValue;
      const AActor: TNyxText): TNyxDataValue;
    procedure PollBuilds;
  protected
    procedure Execute; override;
  public
    constructor Create(const ARepository: TNyxText; AStudioPort, AMCPPort: Integer;
      const AOutputProfile: TNyxText);
    destructor Destroy; override;
    { Only trusted same-origin editor requests receive this independent token.
      MCP bearer credentials cannot call the operator exchange or raise access. }
    function ConnectEditor(const ARequest: TNyxDataValue): TNyxDataValue;
    function EditorExchange(const AToken: TNyxText;
      const ARequest: TNyxDataValue): TNyxDataValue;
    function PreviewData(const AToken: TNyxText): TNyxText;
    function Endpoint: TNyxText;
    { Trusted operator route changes future job profiles. Running jobs retain
      their captured configuration and output identity. MCP cannot set paths. }
    procedure ConfigureOutputs(const AProfile: TNyxText);
    procedure Stop;
    property Failure: TNyxText read FFailure;
  end;

implementation

uses
  nyx.studio.mcpconfig, nyx.types, nyx.studio.builds, nyx.studio.compiler;

function NewCapability: TNyxText;
var
  LID: TGUID;
begin
  CreateGUID(LID);
  Result := Copy(GUIDToString(LID), 2, 36);
end;

function ReadBytes(const APath: TNyxText): TNyxText;
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, LFile.Size);
    SetCodePage(RawByteString(Result), CP_UTF8, False);

    if LFile.Size > 0 then
    begin
      LFile.ReadBuffer(Result[1], LFile.Size);
    end;
  finally
    LFile.Free;
  end;
end;

procedure WriteBytes(const APath, AText: TNyxText);
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmCreate);
  try

    if AText <> '' then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
end;

function Body(ARequest: TFPHTTPConnectionRequest): TNyxText;
var
  LBytes: RawByteString;
begin
  LBytes := ARequest.Content;
  SetCodePage(LBytes, CP_UTF8, False);
  Result := LBytes;
end;

procedure Reply(AResponse: TFPHTTPConnectionResponse; const AValue: TNyxDataValue);
var
  LText: TNyxText;
  LStream: TMemoryStream;
begin
  LText := AValue.ToJSON;
  LStream := TMemoryStream.Create;
  try

    if LText <> '' then
    begin
      LStream.WriteBuffer(LText[1], Length(LText));
    end;
    LStream.Position := 0;
    AResponse.ContentType := 'application/json; charset=utf-8';
    AResponse.ContentStream := LStream;
    AResponse.FreeContentStream := True;
  except
    LStream.Free;
    raise;
  end;
end;

function RPCResult(const AID, AResult: TNyxDataValue): TNyxDataValue;
begin
  Result := NyxObject([NyxField('jsonrpc', NyxData('2.0')),
    NyxField('id', AID), NyxField('result', AResult)]);
end;

function RPCError(const AID: TNyxDataValue; ACode: Integer;
  const AMessage: TNyxText): TNyxDataValue;
begin
  Result := NyxObject([NyxField('jsonrpc', NyxData('2.0')),
    NyxField('id', AID), NyxField('error', NyxObject([
      NyxField('code', NyxData(ACode)), NyxField('message', NyxData(AMessage))]))]);
end;

function ToolResult(const AValue: TNyxDataValue; AError: Boolean = False): TNyxDataValue;
begin
  Result := NyxObject([NyxField('content', NyxArray([NyxObject([
    NyxField('type', NyxData('text')), NyxField('text', NyxData(AValue.ToJSON))])])),
    NyxField('structuredContent', AValue), NyxField('isError', NyxData(AError))]);
end;

constructor TNyxStudioMCP.Create(const ARepository: TNyxText;
  AStudioPort, AMCPPort: Integer; const AOutputProfile: TNyxText);
begin
  inherited Create(True);
  FreeOnTerminate := False;

  if (AMCPPort < 1024) or (AMCPPort > 65535) or (AMCPPort = AStudioPort) then
  begin
    raise ENyxProjectConflict.Create('MCP requires a distinct localhost port in 1024..65535');
  end;
  FPort := AMCPPort;
  FStudioPort := AStudioPort;
  FRepository := IncludeTrailingPathDelimiter(ExpandFileName(ARepository));
  FID := NewCapability;
  FToken := NewCapability + NewCapability;
  FEditorToken := NewCapability + NewCapability;
  FGuard := SyncObjs.TCriticalSection.Create;
  FCore := TNyxAgentSession.Create;
  FBuilds := TNyxBuildJobs.Create(FRepository, AOutputProfile);
  FHTTP := TNyxMCPHTTP.Create(nil);
  FHTTP.Address := '127.0.0.1';
  FHTTP.Port := FPort;
  FHTTP.Threaded := False;
  FHTTP.OnRequest := Request;
  try
    WriteCodexConfiguration;
  except
    on LException: Exception do
    begin
      { An unrelated/malformed Codex file must not prevent Studio authoring or
        the MCP listener from launching. Surface the exact retained-file issue. }
      FConfigurationIssue := LException.Message;
    end;
  end;
end;

destructor TNyxStudioMCP.Destroy;
begin
  Stop;
  FHTTP.Free;
  FBuilds.Free;
  FCore.Free;
  FGuard.Free;
  inherited Destroy;
end;

function TNyxStudioMCP.Endpoint: TNyxText;
begin
  Result := 'http://127.0.0.1:' + IntToStr(FPort) + '/mcp/' + FID;
end;

procedure TNyxStudioMCP.Execute;
begin
  if Terminated then
  begin
    Exit;
  end;
  try
    FHTTP.Active := True;
  except
    on LException: Exception do FFailure := LException.Message;
  end;
end;

procedure TNyxStudioMCP.Stop;
begin

  if Suspended then
  begin
    { A constructor may fail before Start. Resume the terminated thread so its
      inherited destructor cannot wait forever for a never-started Execute. }
    Terminate;
    Start;
  end;
  if FHTTP <> nil then
  begin
    FHTTP.Active := False;
  end;
  Terminate;
  WaitFor;
end;

procedure TNyxStudioMCP.WriteCodexConfiguration;
var
  LPath: TNyxText;
  LBlock: TNyxText;
begin
  LPath := FRepository + '.codex' + PathDelim + 'config.toml';
  LBlock := NyxMCPConfigBegin + LineEnding + '[mcp_servers.nyx_studio]' + LineEnding +
    'url = "' + Endpoint + '"' + LineEnding +
    'http_headers = { Authorization = "Bearer ' + FToken + '" }' + LineEnding +
    'startup_timeout_sec = 15' + LineEnding + 'tool_timeout_sec = 30' + LineEnding +
    NyxMCPConfigEnd;
  NyxMCPPublishBlock(LPath, LBlock);
  { The user chooses global enrollment explicitly. Thereafter new per-session
    credentials refresh that same managed entry without touching other servers. }
  NyxMCPRefreshRegistration(FRepository, LBlock);
end;

function TNyxStudioMCP.ConnectEditor(const ARequest: TNyxDataValue): TNyxDataValue;
begin
  if ARequest.Field('op').AsText <> 'claim' then
  begin
    raise ENyxProjectConflict.Create('Connect an editor with its explicit paired project');
  end;
  FGuard.Acquire;
  try
    PollBuilds;
    Result := NyxObject([NyxField('token', NyxData(FEditorToken)),
      NyxField('endpoint', NyxData(Endpoint)),
      NyxField('warning', NyxData(FConfigurationIssue + FFailure)),
      NyxField('state', FCore.Exchange(ARequest))]);
  finally
    FGuard.Release;
  end;
end;

function TNyxStudioMCP.EditorExchange(const AToken: TNyxText;
  const ARequest: TNyxDataValue): TNyxDataValue;
begin

  if AToken <> FEditorToken then
  begin
    raise ENyxProjectConflict.Create('Editor connection capability is missing or expired');
  end;
  FGuard.Acquire;
  try
    PollBuilds;
    Result := FCore.Exchange(ARequest);
  finally
    FGuard.Release;
  end;
end;

function TNyxStudioMCP.ClientIndex(const AID: TNyxText): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to High(FClients) do
  begin

    if FClients[LIndex].Field('id').AsText = AID then
    begin
      Exit(LIndex);
    end;
  end;
end;

procedure TNyxStudioMCP.ConfigureOutputs(const AProfile: TNyxText);
begin
  FGuard.Acquire;
  try
    FBuilds.Configure(AProfile);
  finally
    FGuard.Release;
  end;
end;

procedure TNyxStudioMCP.PollBuilds;
const
  CEarlierDesign: TNyxText = ' · earlier design';
var
  LActor: TNyxText;
  LOutcome: TNyxText;
  LPair: TNyxProjectPair;
  LReport: INyxCompilerReport;
begin
  while FBuilds.TakeCompletion(LActor, LOutcome, LPair, LReport) do
  begin

    if FCore.CurrentPair(LPair) then
    begin
      FCore.PublishCompilerReport(LReport);
    end
    else
    begin
      LOutcome := LOutcome + CEarlierDesign;
    end;
    FCore.RecordActivity(LActor, 'nyx_build', LOutcome);
  end;
end;

function TNyxStudioMCP.BuildTool(const AArguments: TNyxDataValue;
  const AActor: TNyxText): TNyxDataValue;
const
  CRunning: TNyxText = 'running · ';
  CSeparator: TNyxText = ' · ';
var
  LMode: TNyxText;
  LPair: TNyxProjectPair;
  LScope: TNyxBuildScope;
  LView: TNyxText;
  LCurrent: Boolean;
  LCurrentOutput: Boolean;
  LFields: array of TNyxDataField;
  LItems: array of TNyxDataValue;
  LItem: TNyxDataValue;
  LDiagnostics: TNyxDataValue;
  LIndex: Integer;
begin

  if FCore.Permission = apDisabled then
  begin
    raise ENyxProjectConflict.Create('Agent access is disabled in Studio');
  end;
  LMode := AArguments.Field('mode').AsText;

  if LMode = 'outputs' then
  begin
    NyxAgentFields(AArguments, '|mode|');
    Exit(FBuilds.Outputs);
  end;

  if LMode = 'status' then
  begin
    Result := FBuilds.Status(AArguments, LPair, LCurrentOutput);
    LCurrent := FCore.CurrentPair(LPair);
    LDiagnostics := Result.Field('diagnostics');
    SetLength(LItems, LDiagnostics.Field('items').Count);
    for LIndex := 0 to High(LItems) do
    begin
      LItem := LDiagnostics.Field('items').Item(LIndex);
      LItems[LIndex] := NyxObject([
        NyxField('file', LItem.Field('file')), NyxField('severity', LItem.Field('severity')),
        NyxField('message', LItem.Field('message')), NyxField('line', LItem.Field('line')),
        NyxField('column', LItem.Field('column')),
        NyxField('navigable', NyxData(LCurrent and LItem.Field('mapped').AsBoolean))]);
    end;
    SetLength(LFields, Result.Count + 3);
    for LIndex := 0 to Result.Count - 1 do
    begin
      LFields[LIndex] := NyxField(Result.Key(LIndex), Result.Field(Result.Key(LIndex)));
    end;
    { Immutable bounded response: overwrite the diagnostic member as a copy,
      then append currentness without altering the worker's cached value. }
    for LIndex := 0 to Result.Count - 1 do
    begin

      if Result.Key(LIndex) = 'diagnostics' then
      begin
        LFields[LIndex] := NyxField('diagnostics', NyxObject([
          NyxField('offset', LDiagnostics.Field('offset')),
          NyxField('order', LDiagnostics.Field('order')),
          NyxField('severity', LDiagnostics.Field('severity')),
          NyxField('available', LDiagnostics.Field('available')),
          NyxField('total', LDiagnostics.Field('total')), NyxField('items', NyxArray(LItems))]));
      end;
    end;
    LIndex := Result.Count;
    LFields[LIndex] := NyxField('currentSource', NyxData(LCurrent));
    LFields[LIndex + 1] := NyxField('currentRevision', NyxData(FCore.Revision));
    LFields[LIndex + 2] := NyxField('currentOutput', NyxData(LCurrentOutput));
    Result := NyxObject(LFields);

    if Length(Result.ToJSON) > 48 * 1024 then
    begin
      raise ENyxProjectConflict.Create('Build status exceeds response budget; use a smaller window');
    end;
    Exit;
  end;

  if LMode <> 'request' then
  begin
    raise ENyxProjectConflict.Create('Build mode must be outputs, request or status');
  end;
  FBuilds.AdmitRequest(AArguments);

  if FCore.Permission <> apEdit then
  begin
    raise ENyxProjectConflict.Create('Agent builds require Allow edits in Studio');
  end;

  if FBuilds.Retry(AActor, AArguments, Result) then
  begin
    FCore.RecordActivity(AActor, 'nyx_build', 'retry returned original job receipt');
    Exit;
  end;
  LScope := ParseNyxBuildScope(AArguments.Field('scope').AsText);
  LView := '';

  if NyxAgentHas(AArguments, 'view') then
  begin
    LView := AArguments.Field('view').AsText;
  end;
  LPair := FCore.BuildPair(AArguments.Field('expectedRevision').AsInteger, LScope, LView);
  Result := FBuilds.Submit(AActor, AArguments, LPair);
  FCore.RecordActivity(AActor, 'nyx_build', CRunning +
    NyxBuildScopeName(LScope) + CSeparator + AArguments.Field('target').AsText);
end;

function Schema(const AProperties: TNyxDataValue;
  const ARequired: array of TNyxDataValue): TNyxDataValue;
begin
  Result := NyxObject([NyxField('type', NyxData('object')),
    NyxField('properties', AProperties), NyxField('required', NyxArray(ARequired)),
    NyxField('additionalProperties', NyxData(False))]);
end;

function TextSchema(const ADescription: TNyxText): TNyxDataValue;
begin
  Result := NyxObject([NyxField('type', NyxData('string')),
    NyxField('description', NyxData(ADescription))]);
end;

function IntSchema(AMinimum, AMaximum: Integer): TNyxDataValue;
begin
  Result := NyxObject([NyxField('type', NyxData('integer')),
    NyxField('minimum', NyxData(AMinimum)), NyxField('maximum', NyxData(AMaximum))]);
end;

function Tool(const AName, ADescription: TNyxText; const ASchema: TNyxDataValue;
  AReadOnly: Boolean): TNyxDataValue;
begin
  Result := NyxObject([NyxField('name', NyxData(AName)),
    NyxField('description', NyxData(ADescription)), NyxField('inputSchema', ASchema),
    NyxField('annotations', NyxObject([
      NyxField('readOnlyHint', NyxData(AReadOnly)),
      NyxField('destructiveHint', NyxData(not AReadOnly)),
      NyxField('idempotentHint', NyxData(True)), NyxField('openWorldHint', NyxData(False))]))]);
end;

function CallbackSchema: TNyxDataValue;
var
  LTriggers: array of TNyxDataValue;
  LTrigger: TNyxTrigger;
  LEvent: TNyxDataValue;
  LChanges: TNyxDataValue;
  LBase: TNyxDataValue;
  LFields: array of TNyxDataField;
  LVariants: array[0..3] of TNyxDataValue;
  LIndex: Integer;
const
  COperations: array[0..3] of TNyxText = ('add', 'policy', 'move', 'remove');
begin
  { Closed wire choices are derived from the public Pascal runtime contract.
    Named semantic events remain distinct open references, never a fake trigger. }
  LTriggers := nil;
  for LTrigger := Low(TNyxTrigger) to High(TNyxTrigger) do
  begin

    if NyxIsRuntimeTrigger(LTrigger) then
    begin
      SetLength(LTriggers, Length(LTriggers) + 1);
      LTriggers[High(LTriggers)] := NyxData(NyxTriggerName(LTrigger));
    end;
  end;
  LEvent := NyxObject([NyxField('oneOf', NyxArray([
    Schema(NyxObject([NyxField('trigger', NyxObject([NyxField('enum', NyxArray(LTriggers))]))]), [NyxData('trigger')]),
    Schema(NyxObject([NyxField('name', TextSchema('Exact published semantic event name'))]), [NyxData('name')])]))]);
  for LIndex := 0 to 3 do
  begin
    SetLength(LFields, 3);
    LFields[0] := NyxField('op', NyxObject([NyxField('const', NyxData(COperations[LIndex]))]));
    LFields[1] := NyxField('id', TextSchema('Exact authored owner, including a reusable definition or instance part'));
    LFields[2] := NyxField('event', LEvent);
    case LIndex of
      0: LVariants[LIndex] := Schema(NyxObject(LFields), [NyxData('op'), NyxData('id'), NyxData('event')]);
      1:
        begin
          SetLength(LFields, 4);
          LFields[3] := NyxField('policy', NyxObject([NyxField('enum', NyxArray([
            NyxData('sequential'), NyxData('asynchronous'), NyxData('ui-queue'), NyxData('threaded')]))]));
          LVariants[LIndex] := Schema(NyxObject(LFields), [NyxData('op'), NyxData('id'), NyxData('event'), NyxData('policy')]);
        end;
      2, 3:
        begin
          SetLength(LFields, 4);
          LFields[3] := NyxField('registration', TextSchema('Exact registration ID returned by add or nyx_node'));

          if LIndex = 2 then
          begin
            SetLength(LFields, 5);
            LFields[4] := NyxField('index', IntSchema(0, 127));
            LVariants[LIndex] := Schema(NyxObject(LFields), [NyxData('op'), NyxData('id'), NyxData('event'), NyxData('registration'), NyxData('index')]);
          end
          else
          begin
            LVariants[LIndex] := Schema(NyxObject(LFields), [NyxData('op'), NyxData('id'), NyxData('event'), NyxData('registration')]);
          end;
        end;
    end;
  end;
  LChanges := NyxObject([NyxField('type', NyxData('array')),
    NyxField('minItems', NyxData(1)), NyxField('maxItems', NyxData(32)),
    NyxField('items', NyxObject([NyxField('oneOf', NyxArray(LVariants))]))]);
  LBase := Schema(NyxObject([NyxField('expectedRevision', IntSchema(1, High(Integer))),
    NyxField('mode', NyxObject([NyxField('enum', NyxArray([NyxData('review'), NyxData('apply')]))])),
    NyxField('changes', LChanges), NyxField('operationId', TextSchema('Required only in apply mode; unique retry identity, 1..120 characters')),
    NyxField('reviewID', TextSchema('Required for apply with removals; returned by nonediting review for the exact actor/revision/changes'))]),
    [NyxData('expectedRevision'), NyxData('mode'), NyxData('changes')]);
  SetLength(LFields, LBase.Count + 1);
  for LIndex := 0 to LBase.Count - 1 do
  begin
    LFields[LIndex] := NyxField(LBase.Key(LIndex), LBase.Field(LBase.Key(LIndex)));
  end;
  LFields[High(LFields)] := NyxField('oneOf', NyxArray([
    NyxObject([NyxField('properties', NyxObject([NyxField('mode', NyxObject([NyxField('const', NyxData('apply'))]))])),
      NyxField('required', NyxArray([NyxData('operationId')]))]),
    NyxObject([NyxField('properties', NyxObject([NyxField('mode', NyxObject([NyxField('const', NyxData('review'))]))])),
      NyxField('not', NyxObject([NyxField('anyOf', NyxArray([
        NyxObject([NyxField('required', NyxArray([NyxData('operationId')]))]),
        NyxObject([NyxField('required', NyxArray([NyxData('reviewID')]))])]))]))])
  ]));
  Result := NyxObject(LFields);
end;

function TNyxStudioMCP.Tools: TNyxDataValue;
var
  LPage: TNyxDataValue;
  LTransaction: TNyxDataValue;
  LBoolean: TNyxDataValue;
begin
  LPage := NyxObject([NyxField('offset', IntSchema(0, 100000)), NyxField('limit', IntSchema(1, 50))]);
  LBoolean := NyxObject([NyxField('type', NyxData('boolean'))]);
  LTransaction := TNyxDataValue.ParseJSON(
    '{"type":"array","minItems":1,"maxItems":64,"items":{"oneOf":[' +
    '{"type":"object","properties":{"op":{"const":"create"},"kind":{"type":"string"},"id":{"type":"string"},"parent":{"type":"string"},"root":{"enum":["page","component"]},"index":{"type":"integer","minimum":0},"properties":{"type":"object","additionalProperties":{"type":["string","boolean","integer","number","null"]}}},"required":["op","kind","id"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"update"},"id":{"type":"string"},"properties":{"type":"object","additionalProperties":{"type":["string","boolean","integer","number","null"]}}},"required":["op","id","properties"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"move"},"id":{"type":"string"},"parent":{"type":"string"},"index":{"type":"integer","minimum":0}},"required":["op","id","parent"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"delete"},"id":{"type":"string"}},"required":["op","id"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"title"},"value":{"type":"string"}},"required":["op","value"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"tokens"},"values":{"type":"object"}},"required":["op","values"],"additionalProperties":false}]}}');
  Result := NyxObject([NyxField('tools', NyxArray([
    Tool('nyx_session', 'Inspect current revision, selection, active view, permissions and undo/draft state. No document dump.', Schema(NyxObject([]), []), True),
    Tool('nyx_outline', 'Page through pages, reusable definitions or one component''s immediate children. Descend by parent ID.',
      Schema(NyxObject([NyxField('parent', TextSchema('Optional exact component ID')),
        NyxField('scope', NyxObject([NyxField('enum', NyxArray([NyxData('pages'), NyxData('components')]))])),
        NyxField('offset', LPage.Field('offset')), NyxField('limit', LPage.Field('limit'))]), []), True),
    Tool('nyx_node', 'Inspect one component''s paged typed properties and optionally events, registrations and semantic source routes. Routes and registrations page across the requested event window. Omitted ID uses selection.',
      Schema(NyxObject([NyxField('id', TextSchema('Exact component ID')),
        NyxField('offset', LPage.Field('offset')), NyxField('limit', LPage.Field('limit')),
        NyxField('events', LBoolean),
        NyxField('eventOffset', IntSchema(0, 100000)),
        NyxField('eventLimit', IntSchema(1, 50)),
        NyxField('registrationOffset', IntSchema(0, 100000)),
        NyxField('registrationLimit', IntSchema(1, 50)),
        NyxField('routeOffset', IntSchema(0, 100000)),
        NyxField('routeLimit', IntSchema(1, 50)),
        NyxField('keys', NyxObject([NyxField('type', NyxData('array')),
          NyxField('maxItems', NyxData(20)), NyxField('items', TextSchema('Exact published property key'))])),
        NyxField('textOffset', IntSchema(0, 1000000)),
        NyxField('textLimit', IntSchema(1, 2048))]), []), True),
    Tool('nyx_components', 'Search catalog intent, labels, creator descriptions and groups; never instantiate controls.',
      Schema(NyxObject([NyxField('query', TextSchema('AND search terms')),
        NyxField('group', TextSchema('Intent group key, e.g. inputs, feedback, composition, all')),
        NyxField('offset', LPage.Field('offset')), NyxField('limit', LPage.Field('limit'))]), []), True),
    Tool('nyx_tokens', 'Read effective semantic theme colors and typed logical-pixel metrics. Change through a grouped tokens operation.', Schema(NyxObject([]), []), True),
    Tool('nyx_diagnostics', 'Page through compiler diagnostics. Locations are Unicode scalar coordinates in submitted source; stale locations cannot navigate.', Schema(LPage, []), True),
    Tool('nyx_source', 'Read only the needed accepted Pascal lines, e.g. around a compiler diagnostic. Does not return pending drafts.',
      Schema(NyxObject([NyxField('line', IntSchema(1, 100000)), NyxField('count', IntSchema(1, 80))]), []), True),
    Tool('nyx_transaction', 'Apply 1..64 semantic operations atomically as ONE undoable paired design/Pascal edit. Use published property types, exact IDs and current expectedRevision. Pending drafts reject. operationId deduplicates the last 64 successful mutations per session.',
      Schema(NyxObject([NyxField('expectedRevision', IntSchema(1, High(Integer))),
        NyxField('operationId', TextSchema('Unique retry identity, 1..120 characters')),
        NyxField('operations', LTransaction)]), [NyxData('expectedRevision'), NyxData('operationId'), NyxData('operations')]), False),
    Tool('nyx_select', 'Select an exact authored component. activate=true requires a page or reusable root. Revision checked; selection does not add content undo history.',
      Schema(NyxObject([NyxField('expectedRevision', IntSchema(1, High(Integer))),
        NyxField('operationId', TextSchema('Unique retry identity')),
        NyxField('id', TextSchema('Exact authored ID')), NyxField('activate', LBoolean)]),
        [NyxData('expectedRevision'), NyxData('operationId'), NyxData('id')]), False),
    Tool('nyx_history', 'Undo or redo one ordinary Studio content transaction; revision remains monotonic. Pending drafts reject.',
      Schema(NyxObject([NyxField('expectedRevision', IntSchema(1, High(Integer))),
        NyxField('operationId', TextSchema('Unique retry identity')),
        NyxField('direction', NyxObject([NyxField('enum', NyxArray([NyxData('undo'), NyxData('redo')]))]))]),
        [NyxData('expectedRevision'), NyxData('operationId'), NyxData('direction')]), False),
    Tool('nyx_preview', 'Selectively render an immutable revision/view snapshot with Nyx. Returns a preview resource link; capture=true additionally returns an actual browser PNG when the local renderer is available.',
      Schema(NyxObject([NyxField('expectedRevision', IntSchema(1, High(Integer))),
        NyxField('view', TextSchema('Exact page or reusable root ID')),
        NyxField('width', IntSchema(320, 1600)), NyxField('height', IntSchema(240, 1200)),
        NyxField('capture', LBoolean)]), [NyxData('expectedRevision'), NyxData('view')]), True),
    Tool('nyx_callbacks', 'Author 1..32 ordered add/policy/move/remove changes as ONE undoable paired source edit. Add returns crafted handler/registration names and final-source TODO lines. Inspect registrations with nyx_node. Results describe each operation in order. Apply requires expectedRevision and operationId; drafts reject. Before removal, review the exact batch for warnings and reviewID, then apply unchanged at that revision/actor. Review does not edit or add history; removal retains Pascal implementations.',
      CallbackSchema, False),
    Tool('nyx_build', 'Inspect output readiness, request an immutable accepted view/reusable/application compiler job, or page through its status/diagnostics. Request requires Allow edits, exact revision/outputID and operationId. Returns immediately; no document history changes. At most two jobs run and sixteen handles remain. Exact retries return the original receipt without rebuilding; changing arguments refuses. Compiler commands, options, paths and source overrides are forbidden. Successful status includes exact source/design/output fingerprints and artifact manifest; stale diagnostics cannot navigate.',
      TNyxDataValue.ParseJSON('{"type":"object","oneOf":[' +
        '{"type":"object","properties":{"mode":{"const":"outputs"}},"required":["mode"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"status"},"job":{"type":"string"},"offset":{"type":"integer","minimum":0,"maximum":512},"limit":{"type":"integer","minimum":1,"maximum":20},"severity":{"enum":["all","error","fatal","warning","hint","note","info"]}},"required":["mode","job"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"request"},"expectedRevision":{"type":"integer","minimum":1},"operationId":{"type":"string","minLength":1,"maxLength":120},"outputID":{"type":"string","minLength":32,"maxLength":32},"target":{"enum":["browser","lcl"]},"scope":{"enum":["view","reusable","application"]},"view":{"type":"string","minLength":1}},"required":["mode","expectedRevision","operationId","outputID","target","scope"],"allOf":[{"if":{"properties":{"scope":{"const":"application"}}},"then":{"not":{"required":["view"]}},"else":{"required":["view"]}}],"additionalProperties":false}]}'), False)
  ]))]);
end;

function TNyxStudioMCP.PreviewData(const AToken: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  Result := '';
  FGuard.Acquire;
  try
    for LIndex := 0 to High(FPreviews) do
    begin

      if FPreviews[LIndex].Field('token').AsText = AToken then
      begin
        Exit(FPreviews[LIndex].Field('packet').AsText);
      end;
    end;
  finally
    FGuard.Release;
  end;
end;

function TNyxStudioMCP.CapturePreview(const APreview: TNyxDataValue): TNyxDataValue;
var
  LExecutable: TNyxText;
  LDirectory: TNyxText;
  LPNG: TNyxText;
  LProcess: TProcess;
  LStarted: QWord;
  LBuffer: array[0..4095] of Byte;
  LRead: Integer;
  LOutput: TNyxText;
  LChunk: TNyxText;
  LFile: TFileStream;
  LBase64: TStringStream;
  LEncoder: TBase64EncodingStream;
begin
  LExecutable := GetEnvironmentVariable('NYX_PREVIEW_BROWSER');
  {$ifdef WINDOWS}

  if LExecutable = '' then
  begin
    LExecutable := GetEnvironmentVariable('ProgramFiles(x86)') +
      '\Microsoft\Edge\Application\msedge.exe';
  end;
  {$endif}

  if not FileExists(LExecutable) then
  begin
    raise ENyxProjectConflict.Create('PNG rendering is unavailable; open the returned preview URL or configure NYX_PREVIEW_BROWSER');
  end;
  LDirectory := FRepository + 'build' + PathDelim + 'agent-previews' + PathDelim + NewCapability;
  ForceDirectories(LDirectory);
  LPNG := LDirectory + PathDelim + 'preview.png';
  LProcess := TProcess.Create(nil);
  try
    { The installed browser is a selective rendering adapter. Product logic and
      the preview itself remain Pascal/Nyx. Every argument is fixed or admitted
      numeric/opaque session data, never application-supplied shell text. }
    LProcess.Executable := LExecutable;
    LProcess.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    LProcess.Parameters.Add('--headless=new');
    LProcess.Parameters.Add('--disable-gpu');
    LProcess.Parameters.Add('--no-first-run');
    LProcess.Parameters.Add('--disable-extensions');
    LProcess.Parameters.Add('--no-default-browser-check');
    LProcess.Parameters.Add('--hide-scrollbars');
    LProcess.Parameters.Add('--user-data-dir=' + LDirectory + PathDelim + 'profile');
    LProcess.Parameters.Add('--window-size=' + IntToStr(APreview.Field('width').AsInteger) +
      ',' + IntToStr(APreview.Field('height').AsInteger));
    LProcess.Parameters.Add('--virtual-time-budget=2000');
    LProcess.Parameters.Add('--dump-dom');
    LProcess.Parameters.Add('--screenshot=' + LPNG);
    LProcess.Parameters.Add(APreview.Field('url').AsText);
    LProcess.Execute;
    LStarted := GetTickCount64;
    LOutput := '';
    repeat
      while LProcess.Output.NumBytesAvailable > 0 do
      begin
        LRead := LProcess.Output.Read(LBuffer, SizeOf(LBuffer));
        SetLength(LChunk, LRead);

        if LRead > 0 then
        begin
          Move(LBuffer[0], LChunk[1], LRead);
        end;
        LOutput := LOutput + LChunk;

        if Length(LOutput) > 1024 * 1024 then
        begin
          LProcess.Terminate(1);
          raise ENyxProjectConflict.Create('Preview renderer output budget exceeded');
        end;
      end;

      if GetTickCount64 - LStarted > 20000 then
      begin
        LProcess.Terminate(1);
        raise ENyxProjectConflict.Create('Preview renderer exceeded its 20-second budget');
      end;

      if LProcess.Running then
      begin
        Sleep(10);
      end;
    until not LProcess.Running and (LProcess.Output.NumBytesAvailable = 0);

    if (LProcess.ExitStatus <> 0) or not FileExists(LPNG) or
      (Pos('data-nyx-preview-ready="true"', LOutput) = 0) or
      (Pos('data-nyx-preview-revision="' + IntToStr(APreview.Field('revision').AsInteger) + '"', LOutput) = 0) then
    begin
      raise ENyxProjectConflict.Create('Preview did not render its admitted Nyx revision');
    end;
  finally
    LProcess.Free;
  end;
  LFile := TFileStream.Create(LPNG, fmOpenRead or fmShareDenyNone);
  LBase64 := TStringStream.Create('');
  try

    if LFile.Size > 2 * 1024 * 1024 then
    begin
      raise ENyxProjectConflict.Create('Preview PNG exceeds 2 MiB');
    end;
    LEncoder := TBase64EncodingStream.Create(LBase64);
    try
      LEncoder.CopyFrom(LFile, LFile.Size);
    finally
      LEncoder.Free;
    end;
    Result := NyxObject([NyxField('type', NyxData('image')),
      NyxField('mimeType', NyxData('image/png')), NyxField('data', NyxData(TNyxText(LBase64.DataString)))]);
  finally
    LBase64.Free;
    LFile.Free;
  end;
end;

function TNyxStudioMCP.Preview(const AArguments: TNyxDataValue;
  const AActor: TNyxText): TNyxDataValue;
var
  LPair: TNyxProjectPair;
  LToken: TNyxText;
  LView: TNyxText;
  LRevision: Integer;
  LWidth: Integer;
  LHeight: Integer;
  LIndex: Integer;
begin
  NyxAgentFields(AArguments, '|expectedRevision|view|width|height|capture|');
  LRevision := AArguments.Field('expectedRevision').AsInteger;
  LView := AArguments.Field('view').AsText;
  LWidth := 1024;
  LHeight := 768;

  if NyxAgentHas(AArguments, 'width') then
  begin
    LWidth := AArguments.Field('width').AsInteger;
  end;

  if NyxAgentHas(AArguments, 'height') then
  begin
    LHeight := AArguments.Field('height').AsInteger;
  end;

  if (LWidth < 320) or (LWidth > 1600) or (LHeight < 240) or (LHeight > 1200) then
  begin
    raise ENyxProjectConflict.Create('Preview viewport exceeds its published budget');
  end;
  LToken := NewCapability;
  FGuard.Acquire;
  try
    LPair := FCore.PreviewPair(LRevision, LView, AActor);

    if Length(FPreviews) = 16 then
    begin
      for LIndex := 1 to High(FPreviews) do
      begin
        FPreviews[LIndex - 1] := FPreviews[LIndex];
      end;
      SetLength(FPreviews, 15);
    end;
    SetLength(FPreviews, Length(FPreviews) + 1);
    FPreviews[High(FPreviews)] := NyxObject([NyxField('token', NyxData(LToken)),
      NyxField('packet', NyxData(NyxObject([NyxField('design', NyxData(LPair.Design)),
        NyxField('view', NyxData(LView)), NyxField('revision', NyxData(LRevision))]).ToJSON))]);
  finally
    FGuard.Release;
  end;
  Result := NyxObject([NyxField('revision', NyxData(LRevision)),
    NyxField('view', NyxData(LView)), NyxField('width', NyxData(LWidth)),
    NyxField('height', NyxData(LHeight)),
    NyxField('url', NyxData('http://127.0.0.1:' + IntToStr(FStudioPort) +
      '/agent-preview.html?token=' + LToken))]);
end;

procedure TNyxStudioMCP.Request(ASender: TObject;
  var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
var
  LRequest: TNyxDataValue;
  LID: TNyxDataValue;
  LParams: TNyxDataValue;
  LResult: TNyxDataValue;
  LArguments: TNyxDataValue;
  LMethod: TNyxText;
  LTool: TNyxText;
  LOrigin: TNyxText;
  LClientID: TNyxText;
  LActor: TNyxText;
  LProtocol: TNyxText;
  LIndex: Integer;
  LNotification: Boolean;
  LPreviewContent: array of TNyxDataValue;
begin
  AResponse.CustomHeaders.Values['Cache-Control'] := 'no-store';
  AResponse.CustomHeaders.Values['X-Content-Type-Options'] := 'nosniff';
  LID := NyxNull;
  try
    LOrigin := ARequest.GetFieldByName('Origin');

    if ((ARequest.GetFieldByName('Host') <> '127.0.0.1:' + IntToStr(FPort)) and
      (ARequest.GetFieldByName('Host') <> 'localhost:' + IntToStr(FPort))) or
      ((LOrigin <> '') and (LOrigin <> 'http://127.0.0.1:' + IntToStr(FPort)) and
        (LOrigin <> 'http://localhost:' + IntToStr(FPort))) then
    begin
      AResponse.Code := 403;
      Exit;
    end;

    if ARequest.PathInfo <> '/mcp/' + FID then
    begin
      AResponse.Code := 404;
      Exit;
    end;

    if ARequest.GetFieldByName('Authorization') <> 'Bearer ' + FToken then
    begin
      AResponse.Code := 401;
      Exit;
    end;
    LClientID := ARequest.GetFieldByName('Mcp-Session-Id');
    LIndex := ClientIndex(LClientID);

    if ARequest.Method = 'DELETE' then
    begin

      if LIndex < 0 then
      begin
        AResponse.Code := 404;
      end
      else
      begin
        FClients[LIndex] := FClients[High(FClients)];
        SetLength(FClients, Length(FClients) - 1);
        AResponse.Code := 200;
      end;
      Exit;
    end;

    if ARequest.Method <> 'POST' then
    begin
      AResponse.Code := 405;
      AResponse.CustomHeaders.Values['Allow'] := 'POST, DELETE';
      Exit;
    end;

    if Pos('application/json', ARequest.ContentType) = 0 then
    begin
      AResponse.Code := 415;
      Exit;
    end;

    if Length(ARequest.Content) > 262144 then
    begin
      AResponse.Code := 413;
      Exit;
    end;
    LProtocol := ARequest.GetFieldByName('MCP-Protocol-Version');

    if (LProtocol <> '') and (LProtocol <> '2025-11-25') and
      (LProtocol <> '2025-06-18') and (LProtocol <> '2025-03-26') then
    begin
      AResponse.Code := 400;
      Reply(AResponse, RPCError(LID, -32600, 'Unsupported MCP protocol version'));
      Exit;
    end;
    try
      LRequest := TNyxDataValue.ParseJSON(Body(ARequest));
    except
      AResponse.Code := 400;
      Reply(AResponse, RPCError(LID, -32700, 'Parse error'));
      Exit;
    end;
    NyxAgentFields(LRequest, '|jsonrpc|id|method|params|');

    if LRequest.Field('jsonrpc').AsText <> '2.0' then
    begin
      raise ENyxProjectConflict.Create('JSON-RPC 2.0 required');
    end;
    LMethod := LRequest.Field('method').AsText;
    LNotification := not NyxAgentHas(LRequest, 'id');

    if not LNotification then
    begin
      LID := LRequest.Field('id');

      if (LID.Kind <> ndText) and (LID.Kind <> ndNumber) then
      begin
        raise ENyxProjectConflict.Create('Request ID must be text or number');
      end;
    end;
    LParams := NyxObject([]);

    if NyxAgentHas(LRequest, 'params') then
    begin
      LParams := LRequest.Field('params');
    end;

    if (LMethod = 'initialize') and not LNotification then
    begin

      if Length(FClients) >= 64 then
      begin
        Reply(AResponse, RPCError(LID, -32000, 'Transport session limit reached; terminate unused clients'));
        Exit;
      end;
      LActor := LParams.Field('clientInfo').Field('name').AsText;

      if (LActor = '') or (Length(LActor) > 100) then
      begin
        raise ENyxProjectConflict.Create('Client name must contain 1..100 characters');
      end;
      LProtocol := LParams.Field('protocolVersion').AsText;

      if (LProtocol <> '2025-11-25') and (LProtocol <> '2025-06-18') and
        (LProtocol <> '2025-03-26') then LProtocol := '2025-11-25';
      LClientID := NewCapability;
      SetLength(FClients, Length(FClients) + 1);
      FClients[High(FClients)] := NyxObject([NyxField('id', NyxData(LClientID)),
        NyxField('actor', NyxData(LActor)), NyxField('initialized', NyxData(False))]);
      AResponse.CustomHeaders.Values['Mcp-Session-Id'] := LClientID;
      Reply(AResponse, RPCResult(LID, NyxObject([
        NyxField('protocolVersion', NyxData(LProtocol)),
        NyxField('capabilities', NyxObject([NyxField('tools', NyxObject([])), NyxField('resources', NyxObject([]))])),
        NyxField('serverInfo', NyxObject([NyxField('name', NyxData('Nyx Studio')), NyxField('version', NyxData('1.0'))])),
        NyxField('instructions', NyxData('Use nyx_session for revision/selection, bounded queries for context and nyx_transaction for grouped edits. Studio controls permissions and displays activity. Preview selectively.'))])));
      Exit;
    end;

    if LIndex < 0 then
    begin
      AResponse.Code := 404;
      Reply(AResponse, RPCError(LID, -32000, 'MCP transport session is missing or expired; initialize again'));
      Exit;
    end;

    if LNotification then
    begin

      if LMethod = 'notifications/initialized' then
      begin
        FClients[LIndex] := NyxObject([NyxField('id', NyxData(LClientID)),
          NyxField('actor', FClients[LIndex].Field('actor')), NyxField('initialized', NyxData(True))]);
      end;
      AResponse.Code := 202;
      Exit;
    end;

    if not FClients[LIndex].Field('initialized').AsBoolean then
    begin
      Reply(AResponse, RPCError(LID, -32000, 'Send notifications/initialized before tools'));
      Exit;
    end;

    if LMethod = 'ping' then
    begin
      LResult := NyxObject([]);
    end
    else if LMethod = 'tools/list' then
    begin
      LResult := Tools;
    end
    else if LMethod = 'resources/list' then
    begin
      LResult := NyxObject([NyxField('resources', NyxArray([NyxObject([
        NyxField('uri', NyxData('nyx://' + FID + '/session')),
        NyxField('name', NyxData('Active Studio session')),
        NyxField('mimeType', NyxData('application/json'))])]))]);
    end
    else if LMethod = 'resources/read' then
    begin

      if LParams.Field('uri').AsText <> 'nyx://' + FID + '/session' then
      begin
        Reply(AResponse, RPCError(LID, -32002, 'Resource not found'));
        Exit;
      end;
      FGuard.Acquire;
      try
        LResult := FCore.Call('nyx_session', FClients[LIndex].Field('actor').AsText, NyxObject([]));
      finally
        FGuard.Release;
      end;
      LResult := NyxObject([NyxField('contents', NyxArray([NyxObject([
        NyxField('uri', LParams.Field('uri')), NyxField('mimeType', NyxData('application/json')),
        NyxField('text', NyxData(LResult.ToJSON))])]))]);
    end
    else if LMethod = 'tools/call' then
    begin
      LTool := LParams.Field('name').AsText;
      LArguments := NyxObject([]);

      if NyxAgentHas(LParams, 'arguments') then
      begin
        LArguments := LParams.Field('arguments');
      end;
      try

        if LTool = 'nyx_build' then
        begin
          FGuard.Acquire;
          try
            PollBuilds;
            LResult := BuildTool(LArguments, FClients[LIndex].Field('actor').AsText);
            LResult := ToolResult(LResult);
          finally
            FGuard.Release;
          end;
        end
        else if LTool = 'nyx_preview' then
        begin
          LResult := Preview(LArguments, FClients[LIndex].Field('actor').AsText);
          SetLength(LPreviewContent, 2);
          LPreviewContent[0] := NyxObject([
            NyxField('type', NyxData('resource_link')), NyxField('uri', LResult.Field('url')),
            NyxField('name', NyxData('Rendered Nyx view')), NyxField('mimeType', NyxData('text/html'))]);
          LPreviewContent[1] := NyxObject([NyxField('type', NyxData('text')),
            NyxField('text', NyxData(LResult.ToJSON))]);

          if NyxAgentHas(LArguments, 'capture') and LArguments.Field('capture').AsBoolean then
          begin
            SetLength(LPreviewContent, 3);
            LPreviewContent[2] := CapturePreview(LResult);
          end;
          LResult := NyxObject([NyxField('content', NyxArray(LPreviewContent)),
            NyxField('structuredContent', LResult), NyxField('isError', NyxData(False))]);
          FGuard.Acquire;
          try
            FCore.RecordActivity(FClients[LIndex].Field('actor').AsText, LTool, 'completed');
          finally
            FGuard.Release;
          end;
        end
        else
        begin
          FGuard.Acquire;
          try
            PollBuilds;
            LResult := FCore.Call(LTool, FClients[LIndex].Field('actor').AsText, LArguments);
            LResult := ToolResult(LResult);
          finally
            FGuard.Release;
          end;
        end;
      except
        on LException: Exception do
        begin
          FGuard.Acquire;
          try
            if (LTool = 'nyx_preview') or (LTool = 'nyx_build') then
            begin
              FCore.RecordActivity(FClients[LIndex].Field('actor').AsText, LTool,
                'refused: ' + LException.Message);
            end;
            LResult := ToolResult(NyxObject([NyxField('code', NyxData('operation_rejected')),
              NyxField('message', NyxData(LException.Message)),
              NyxField('currentRevision', NyxData(FCore.Revision))]), True);
          finally
            FGuard.Release;
          end;
        end;
      end;
    end
    else
    begin
      Reply(AResponse, RPCError(LID, -32601, 'Method not found'));
      Exit;
    end;
    Reply(AResponse, RPCResult(LID, LResult));
  except
    on LException: Exception do
    begin
      AResponse.Code := 400;
      Reply(AResponse, RPCError(LID, -32600, LException.Message));
    end;
  end;
end;

end.
