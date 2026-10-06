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
  nyx.text, nyx.data, nyx.studio.agents, nyx.studio.projects, nyx.studio.buildjobs,
  nyx.studio.reviews, nyx.studio.workspaces, nyx.presentations, nyx.studio.directories,
  nyx.studio.recovery;

type
  { Authority is supplied by the authenticated transport, never client JSON.
    Operator compilation remains available when agent access is disabled. }
  TNyxBuildAuthority = (baAgent, baEditor);
  { Native/server host persists operator profiles. The service owns this borrowed
    callback; standalone protocol fixtures may keep configuration in memory. }
  TNyxOperatorProfileChange = procedure(const AProfile: TNyxText) of object;

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
    FReviews: TNyxReviewWorkspaces;
    FWorkspaces: TNyxStudioWorkspaces;
    FRecovery: TNyxStudioRuntimeStore;
    FBuilds: TNyxBuildJobs;
    FID: TNyxText;
    FToken: TNyxText;
    FEditorToken: TNyxText;
    FDirectories: TNyxStudioDirectories;
    FPort: Integer;
    FStudioPort: Integer;
    FClients: array of TNyxDataValue;
    FPreviews: array of TNyxDataValue;
    { Operator-only live views have separate capabilities from immutable MCP
      snapshots. At most eight tokens survive; retirement invalidates them. }
    FReviewPreviews: array of record
      Reference: TNyxReviewRef;
      Token: TNyxText;
    end;
    FFailure: TNyxText;
    FConfigurationIssue: TNyxText;
    FOnOperatorProfileChange: TNyxOperatorProfileChange;
    procedure Request(ASender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure WriteCodexConfiguration;
    function Tools: TNyxDataValue;
    function Preview(const AWireArguments: TNyxDataValue;
      const AActor, AOwner: TNyxText): TNyxDataValue;
    function CapturePreview(const APreview: TNyxDataValue): TNyxDataValue;
    function ClientIndex(const AID: TNyxText): Integer;
    function BuildTool(const AWireArguments: TNyxDataValue;
      const AActor, AOwner: TNyxText;
      AAuthority: TNyxBuildAuthority = baAgent): TNyxDataValue;
    procedure PollBuilds;
    procedure FinishDocumentChange(var ARollback: TNyxStudioRuntimeRollback);
    procedure RestoreDocumentChange(var ARollback: TNyxStudioRuntimeRollback);
    function EditorState(const ARequest: TNyxDataValue): TNyxDataValue;
    function ReviewViews: TNyxDataValue;
    { One explicit scope is admitted before every read/edit/build/preview.
      Browser project navigation never changes the primary MCP alias. }
    function ContextSession(const AArguments: TNyxDataValue;
      const AOwner: TNyxText; const AActor: TNyxText = ''): TNyxAgentSession;
    { Caller holds the session guard. Refusal metadata belongs to the admitted
      context; unknown/foreign/retired reviews receive no substitute revision. }
    function Rejection(const AArguments: TNyxDataValue;
      const AOwner, AMessage: TNyxText): TNyxDataValue;
  protected
    procedure Execute; override;
  public
    constructor Create(const ARepository: TNyxText; AStudioPort, AMCPPort: Integer;
      const AOutputProfile: TNyxText); overload;
    { Source, writable artifacts and enrollment are explicit host roles. The
      suspended constructor enrolls only the admitted project, never a payload. }
    constructor Create(const ADirectories: TNyxStudioDirectories;
      AStudioPort, AMCPPort: Integer; const AOutputProfile: TNyxText); overload;
    destructor Destroy; override;
    { Only trusted same-origin editor requests receive this independent token.
      MCP bearer credentials cannot call the operator exchange or raise access. }
    function ConnectEditor(const ARequest: TNyxDataValue): TNyxDataValue;
    function EditorExchange(const AToken: TNyxText;
      const ARequest: TNyxDataValue): TNyxDataValue;
    { Trusted native hosting seam used by the authenticated MCP ordinary-tool
      route. It owns locking, semantic dispatch and durable admission together.
      Host supplies the already authenticated private owner/display actor; neither
      comes from tool arguments. This is not an additional network endpoint.
      Build/preview keep their separately owned external-work paths. }
    function InvokeTool(const ATool, AOwner, AActor: TNyxText;
      const AArguments: TNyxDataValue): TNyxDataValue;
    { Trusted compiler seam shared with authenticated MCP. Serializes admission,
      queue pumping, cancellation and exact context; never waits for a compiler
      under the editor lock. Owner is supplied by transport, not tool arguments. }
    function InvokeBuild(const AOwner, AActor: TNyxText;
      const AArguments: TNyxDataValue): TNyxDataValue;
    function PreviewData(const AToken: TNyxText): TNyxText;
    { Read-only operator observation of accepted review design. Unchanged
      revisions return metadata only. Retired capabilities return empty text;
      neither Pascal source nor pending editor drafts are exposed here. }
    function ReviewData(const AToken: TNyxText; AAfter: Integer): TNyxText;
    function Endpoint: TNyxText;
    { Trusted operator route changes future job profiles. Running jobs retain
      their captured configuration and output identity. MCP cannot set paths. }
    procedure ConfigureOutputs(const AProfile: TNyxText);
    procedure Stop;
    property Failure: TNyxText read FFailure;
    property OnOperatorProfileChange: TNyxOperatorProfileChange
      read FOnOperatorProfileChange write FOnOperatorProfileChange;
  end;

{ Pure discovery snapshot, shared with tools/list. It creates no service/thread,
  configuration, credentials or document. Useful for offline schema qualification
  while the runtime listener is deliberately kept at its protected checkpoint. }
function NyxStudioMCPTools: TNyxDataValue;

implementation

uses
  nyx.studio.mcpconfig, nyx.types, nyx.studio.builds, nyx.studio.compiler,
  nyx.model, nyx.codec, nyx.studio.outputs, nyx.studio.stateedits,
  nyx.studio.collectionedits, nyx.editing;

function NewCapability: TNyxText;
var
  LID: TGUID;
begin
  CreateGUID(LID);
  Result := Copy(GUIDToString(LID), 2, 36);
end;

function EditorBuildError(const AMessage: TNyxText): TNyxText;
var
  LIndex: Integer;
  LCount: Integer;
  LScalar: Integer;
begin
  LIndex := 1;
  LCount := 0;
  while (LIndex <= Length(AMessage)) and (LCount < 1024) do
  begin

    if not NyxNextScalar(AMessage, LIndex, LScalar) then
    begin
      Exit('Compiler request was refused');
    end;
    Inc(LCount);
  end;
  { Bound the reply without cutting a native UTF-8 or browser UTF-16 scalar. }
  Result := Copy(AMessage, 1, LIndex - 1);
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
  Create(TNyxStudioDirectories.ForRepository(ARepository), AStudioPort, AMCPPort,
    AOutputProfile);
end;

constructor TNyxStudioMCP.Create(const ADirectories: TNyxStudioDirectories;
  AStudioPort, AMCPPort: Integer; const AOutputProfile: TNyxText);
var
  LRestoredCore: TNyxAgentSession;
  LRestoredWorkspaces: TNyxStudioWorkspaces;
begin
  inherited Create(True);
  FreeOnTerminate := False;

  if (AMCPPort < 1024) or (AMCPPort > 65535) or (AMCPPort = AStudioPort) then
  begin
    raise ENyxProjectConflict.Create('MCP requires a distinct localhost port in 1024..65535');
  end;
  FPort := AMCPPort;
  FStudioPort := AStudioPort;
  ADirectories.Validate;
  FDirectories := ADirectories;
  FID := NewCapability;
  FToken := NewCapability + NewCapability;
  FEditorToken := NewCapability + NewCapability;
  FGuard := SyncObjs.TCriticalSection.Create;
  FCore := TNyxAgentSession.Create;
  FBuilds := TNyxBuildJobs.Create(FDirectories, AOutputProfile);
  FWorkspaces := TNyxStudioWorkspaces.Create(FCore, FID);
  FRecovery := TNyxStudioRuntimeStore.Create(FDirectories);

  if FRecovery.Load(LRestoredCore, LRestoredWorkspaces) then
  begin
    FWorkspaces.Free;
    FCore.Free;
    FCore := LRestoredCore;
    FWorkspaces := LRestoredWorkspaces;
  end;
  FReviews := TNyxReviewWorkspaces.Create(FCore);
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
  FReviews.Free;
  FWorkspaces.Free;
  FCore.Free;
  FRecovery.Free;
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
  LPath := FDirectories.EnrollmentRoot + '.codex' + PathDelim + 'config.toml';
  LBlock := NyxMCPConfigBegin + LineEnding + '[mcp_servers.nyx_studio]' + LineEnding +
    'url = "' + Endpoint + '"' + LineEnding +
    'http_headers = { Authorization = "Bearer ' + FToken + '" }' + LineEnding +
    'startup_timeout_sec = 15' + LineEnding + 'tool_timeout_sec = 30' + LineEnding +
    NyxMCPConfigEnd;
  NyxMCPPublishBlock(LPath, LBlock);
  { The user chooses global enrollment explicitly. Thereafter new per-session
    credentials refresh that same managed entry without touching other servers. }
  NyxMCPRefreshRegistration(FDirectories.EnrollmentRoot, LBlock);
end;

function TNyxStudioMCP.ConnectEditor(const ARequest: TNyxDataValue): TNyxDataValue;
var
  LRollback: TNyxStudioRuntimeRollback;
begin
  if ARequest.Field('op').AsText <> 'claim' then
  begin
    raise ENyxProjectConflict.Create('Connect an editor with its explicit paired project');
  end;
  FGuard.Acquire;
  LRollback := nil;
  try
    PollBuilds;
    LRollback := TNyxStudioRuntimeRollback.Create(FCore, FWorkspaces);
    try
      Result := NyxObject([NyxField('token', NyxData(FEditorToken)),
        NyxField('endpoint', NyxData(Endpoint)),
        NyxField('warning', NyxData(FConfigurationIssue + FFailure)),
        NyxField('state', EditorState(ARequest))]);
      FinishDocumentChange(LRollback);
    except
      RestoreDocumentChange(LRollback);
      raise;
    end;
  finally
    LRollback.Free;
    FGuard.Release;
  end;
end;

function TNyxStudioMCP.EditorExchange(const AToken: TNyxText;
  const ARequest: TNyxDataValue): TNyxDataValue;
var
  LRollback: TNyxStudioRuntimeRollback;
  LOperation: TNyxText;
begin

  if AToken <> FEditorToken then
  begin
    raise ENyxProjectConflict.Create('Editor connection capability is missing or expired');
  end;
  FGuard.Acquire;
  LRollback := nil;
  try
    PollBuilds;
    LOperation := ARequest.Field('op').AsText;

    if (LOperation = 'claim') or (LOperation = 'commit') or
      (LOperation = 'configure') or (LOperation = 'history') or
      (LOperation = 'close-workspace') then
    begin
      LRollback := TNyxStudioRuntimeRollback.Create(FCore, FWorkspaces);
    end;
    try
      Result := EditorState(ARequest);
      FinishDocumentChange(LRollback);
    except
      RestoreDocumentChange(LRollback);
      raise;
    end;
  finally
    LRollback.Free;
    FGuard.Release;
  end;
end;

procedure TNyxStudioMCP.RestoreDocumentChange(var ARollback: TNyxStudioRuntimeRollback);
var
  LPreviousCore: TNyxAgentSession;
  LPreviousWorkspaces: TNyxStudioWorkspaces;
begin

  if ARollback = nil then
  begin
    Exit;
  end;
  { Every replacement owner was prepared before mutation. Publication contains
    no parsing, filesystem access or allocation; independent reviews only rebind
    their borrowed primary before former owners are released. Workers own text. }
  LPreviousCore := FCore;
  LPreviousWorkspaces := FWorkspaces;
  FReviews.RebindActive(ARollback.Primary);
  FCore := ARollback.Primary;
  FWorkspaces := ARollback.Workspaces;
  ARollback.Primary := nil;
  ARollback.Workspaces := nil;
  FreeAndNil(ARollback);
  LPreviousWorkspaces.Free;
  LPreviousCore.Free;
end;

procedure TNyxStudioMCP.FinishDocumentChange(var ARollback: TNyxStudioRuntimeRollback);
begin

  if ARollback = nil then
  begin
    Exit;
  end;

  if ARollback.Changed(FCore, FWorkspaces) then
  begin
    FRecovery.Save(FCore, FWorkspaces);
  end;
  FreeAndNil(ARollback);
end;

function DurableTool(const ATool: TNyxText; const AArguments: TNyxDataValue): Boolean;
var
  LMode: TNyxText;
begin
  { Names/modes belong only to the MCP wire boundary, never application authoring.
    Review-local work expires with its transport and cannot change user projects. }
  Result := False;

  if NyxAgentHas(AArguments, 'review') then
  begin
    Exit;
  end;
  Result := (ATool = 'nyx_transaction') or (ATool = 'nyx_select') or
    (ATool = 'nyx_history');

  if NyxAgentHas(AArguments, 'mode') then
  begin
    LMode := AArguments.Field('mode').AsText;
    Result := Result or ((ATool = 'nyx_workspaces') and (LMode = 'create')) or
      (((ATool = 'nyx_callbacks') or (ATool = 'nyx_pascal') or
        (ATool = 'nyx_roots') or (ATool = 'nyx_state') or
        (ATool = 'nyx_collections')) and (LMode = 'apply')) or
      ((ATool = 'nyx_pascal') and ((LMode = 'edit-imports') or
        (LMode = 'edit-routines') or (LMode = 'edit-declarations')));
  end;
end;

function TNyxStudioMCP.InvokeTool(const ATool, AOwner, AActor: TNyxText;
  const AArguments: TNyxDataValue): TNyxDataValue;
var
  LRollback: TNyxStudioRuntimeRollback;
begin

  if (AOwner = '') or (AActor = '') or (NyxTextScalarCount(AOwner) > 120) or
    (NyxTextScalarCount(AActor) > 120) or (ATool = 'nyx_build') or (ATool = 'nyx_preview') then
  begin
    raise ENyxProjectConflict.Create('Ordinary tool dispatch requires a trusted bounded transport context');
  end;
  LRollback := nil;
  FGuard.Acquire;
  try
    PollBuilds;

    if DurableTool(ATool, AArguments) then
    begin
      LRollback := TNyxStudioRuntimeRollback.Create(FCore, FWorkspaces);
    end;
    try

      if ATool = 'nyx_workspaces' then
      begin
        Result := FWorkspaces.Manage(AOwner, AActor, AArguments);
      end
      else if NyxAgentHas(AArguments, 'workspace') then
      begin
        Result := FWorkspaces.Call(ATool, AOwner, AActor, AArguments);
      end
      else
      begin
        Result := FReviews.Call(ATool, AOwner, AActor, AArguments);
      end;
      FinishDocumentChange(LRollback);
    except
      on LException: Exception do
      begin
        RestoreDocumentChange(LRollback);
        FCore.RecordActivity(AActor, ATool, 'refused: ' + LException.Message);
        raise;
      end;
    end;
  finally
    LRollback.Free;
    FGuard.Release;
  end;
end;

function TNyxStudioMCP.InvokeBuild(const AOwner, AActor: TNyxText;
  const AArguments: TNyxDataValue): TNyxDataValue;
begin

  if (AOwner = '') or (AActor = '') or (NyxTextScalarCount(AOwner) > 120) or
    (NyxTextScalarCount(AActor) > 120) then
  begin
    raise ENyxProjectConflict.Create('Compiler dispatch requires a trusted bounded connection');
  end;
  FGuard.Acquire;
  try
    PollBuilds;
    try
      Result := BuildTool(AArguments, AActor, AOwner);
    except
      on LException: Exception do
      begin
        FCore.RecordActivity(AActor, 'nyx_build', 'refused: ' + LException.Message);
        raise;
      end;
    end;
  finally
    FGuard.Release;
  end;
end;

function TNyxStudioMCP.EditorState(const ARequest: TNyxDataValue): TNyxDataValue;
var
  LState: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LWorkspace: TNyxWorkspaceRef;
  LSession: TNyxAgentSession;
  LArguments: TNyxDataValue;
  LTarget: TNyxWorkspaceRef;
  LBuild: TNyxDataValue;
  LAfter: Integer;
begin
  LWorkspace := NyxWorkspaceArgument(ARequest);
  LSession := FWorkspaces.Find(LWorkspace);

  if LSession = nil then
  begin
    raise ENyxProjectConflict.Create('Editor project is missing or closed; your local work is retained');
  end;

  LBuild := NyxNull;

  if ARequest.Field('op').AsText = 'build' then
  begin
    { A private capability admits operator work. Keep job/profile/scoped-pair
      validation in the same bounded asynchronous compiler service as MCP. }
    LArguments := NyxWorkspaceArguments(ARequest);
    NyxAgentFields(LArguments, '|op|after|build|');
    { Admit observation metadata before a profile write or job submission.
      A malformed envelope must not leave admitted work behind a lost receipt. }
    LAfter := LArguments.Field('after').AsInteger;

    if LAfter < 0 then
    begin
      raise ENyxProjectConflict.Create('Editor build observation revision must be nonnegative');
    end;
    try
      LBuild := BuildTool(NyxWithWorkspace(LArguments.Field('build'), LWorkspace),
        'Studio', 'private-editor', baEditor);
    except
      on LException: Exception do
      begin
        { Compiler admission failure is not an editor synchronization conflict.
          Keep ordinary observation/publication alive and return bounded help. }
        LBuild := NyxObject([NyxField('state', NyxData('rejected')),
          NyxField('error', NyxData(EditorBuildError(TNyxText(LException.Message))))]);
      end;
    end;
    LState := LSession.Exchange(NyxObject([NyxField('op', NyxData('observe')),
      NyxField('after', NyxData(LAfter))]));
  end
  else if ARequest.Field('op').AsText = 'close-workspace' then
  begin
    { This route requires the private editor capability. Project lifecycle MCP
      deliberately omits close; an agent cannot manufacture operator consent.
      Recheck the warning's exact revision under the same registry lock. }
    LArguments := NyxWorkspaceArguments(ARequest);
    NyxAgentFields(LArguments, '|op|after|target|expectedRevision|confirmed|');
    LTarget := NyxWorkspace(LArguments.Field('target').AsText);

    if LTarget.ID = LWorkspace.ID then
    begin
      raise ENyxProjectConflict.Create('Switch to another project before closing this editor');
    end;
    FWorkspaces.CloseProject(LTarget, LArguments.Field('expectedRevision').AsInteger,
      LArguments.Field('confirmed').AsBoolean);
    LState := LSession.Exchange(NyxObject([NyxField('op', NyxData('observe'))]));
  end
  else if ARequest.Field('op').AsText = 'configure' then
  begin
    { Enablement is a global operator choice, even when another project is
      displayed. Apply it only to the primary authority, then observe the
      selected project with the updated inherited permission. }
    FCore.Exchange(NyxWorkspaceArguments(ARequest));
    LSession := FWorkspaces.Find(LWorkspace);
    LState := LSession.Exchange(NyxObject([NyxField('op', NyxData('observe'))]));
  end
  else
  begin
    LState := LSession.Exchange(NyxWorkspaceArguments(ARequest));
  end;
  SetLength(LFields, LState.Count + 4);
  for LIndex := 0 to LState.Count - 1 do
  begin
    LFields[LIndex] := NyxField(LState.Key(LIndex), LState.Field(LState.Key(LIndex)));
  end;
  LFields[LState.Count] := NyxField('reviews', ReviewViews);
  LFields[LState.Count + 1] := NyxField('workspaces', FWorkspaces.Observe);
  LFields[LState.Count + 2] := NyxField('workspaceClosing', NyxData(True));
  LFields[LState.Count + 3] := NyxField('editorBuilds', NyxData(True));

  if LBuild.Kind <> ndNull then
  begin
    SetLength(LFields, LState.Count + 5);
    LFields[LState.Count + 4] := NyxField('buildReply', LBuild);
  end;
  Result := NyxWithWorkspace(NyxObject(LFields), LWorkspace);
end;

function TNyxStudioMCP.ContextSession(const AArguments: TNyxDataValue;
  const AOwner: TNyxText; const AActor: TNyxText): TNyxAgentSession;
var
  LWorkspace: TNyxWorkspaceRef;
begin
  LWorkspace := NyxWorkspaceArgument(AArguments);

  if LWorkspace.ID <> '' then
  begin
    Result := FWorkspaces.Resolve(LWorkspace);

    if AActor <> '' then
    begin
      FWorkspaces.RecordRequest(AOwner, AActor, LWorkspace);
    end;
  end
  else
  begin
    Result := FReviews.Resolve(AOwner, NyxReviewArgument(AArguments));
  end;
end;

function TNyxStudioMCP.ReviewViews: TNyxDataValue;
var
  LMetadata: TNyxDataValue;
  LItems: array of TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LToken: Integer;
  LMove: Integer;
  LField: Integer;
  LRef: TNyxReviewRef;
begin
  { Caller holds FGuard. Prune retired capabilities before minting replacements;
    the live workspace budget also bounds this presentation cache. }
  for LIndex := High(FReviewPreviews) downto 0 do
  begin

    if FReviews.Find(FReviewPreviews[LIndex].Reference) = nil then
    begin
      for LMove := LIndex + 1 to High(FReviewPreviews) do
      begin
        FReviewPreviews[LMove - 1] := FReviewPreviews[LMove];
      end;
      SetLength(FReviewPreviews, Length(FReviewPreviews) - 1);
    end;
  end;
  LMetadata := FReviews.Observe;
  SetLength(LItems, LMetadata.Count);
  for LIndex := 0 to LMetadata.Count - 1 do
  begin
    LRef := NyxReview(LMetadata.Item(LIndex).Field('review').AsText);
    LToken := 0;
    while (LToken < Length(FReviewPreviews)) and
      (FReviewPreviews[LToken].Reference.ID <> LRef.ID) do
    begin
      Inc(LToken);
    end;

    if LToken = Length(FReviewPreviews) then
    begin
      SetLength(FReviewPreviews, LToken + 1);
      FReviewPreviews[LToken].Reference := LRef;
      FReviewPreviews[LToken].Token := NewCapability + NewCapability;
    end;
    SetLength(LFields, LMetadata.Item(LIndex).Count + 1);
    for LField := 0 to LMetadata.Item(LIndex).Count - 1 do
    begin
      LFields[LField] := NyxField(LMetadata.Item(LIndex).Key(LField),
        LMetadata.Item(LIndex).Field(LMetadata.Item(LIndex).Key(LField)));
    end;
    LFields[High(LFields)] := NyxField('preview', NyxData('agent-review.html?token=' +
      FReviewPreviews[LToken].Token));
    LItems[LIndex] := NyxObject(LFields);
  end;
  Result := NyxArray(LItems);
end;

function TNyxStudioMCP.ReviewData(const AToken: TNyxText; AAfter: Integer): TNyxText;
var
  LIndex: Integer;
  LSession: TNyxAgentSession;
  LState: TNyxDataValue;
  LPair: TNyxProjectPair;
  LView: TNyxText;
  LDocument: TNyxDocument;
begin
  Result := '';

  if AAfter < 0 then
  begin
    raise ENyxProjectConflict.Create('Review observation revision must be nonnegative');
  end;
  FGuard.Acquire;
  try
    for LIndex := 0 to High(FReviewPreviews) do
    begin

      if FReviewPreviews[LIndex].Token = AToken then
      begin
        LSession := FReviews.Find(FReviewPreviews[LIndex].Reference);

        if LSession = nil then
        begin
          Exit;
        end;
        LState := LSession.Exchange(NyxObject([NyxField('op', NyxData('observe')),
          NyxField('after', NyxData(AAfter))]));

        if not NyxAgentHas(LState, 'project') then
        begin
          Exit(NyxObject([NyxField('revision', NyxData(LSession.Revision)),
            NyxField('review', NyxData(FReviewPreviews[LIndex].Reference.ID))]).ToJSON);
        end;
        LPair := DecodeNyxProject(LState.Field('project').AsText);
        LView := LState.Field('session').Field('view').AsText;
        LDocument := TNyxCodec.Decode(LPair.Design);
        try

          if LDocument.Find(LView) = nil then
          begin
            LView := '';

            if LDocument.Count > 0 then
            begin
              LView := LDocument.Pages[0].ID;
            end
            else if LDocument.ComponentCount > 0 then
            begin
              LView := LDocument.Components[0].ID;
            end;
          end;
        finally
          LDocument.Free;
        end;
        Exit(NyxObject([NyxField('revision', NyxData(LSession.Revision)),
          NyxField('review', NyxData(FReviewPreviews[LIndex].Reference.ID)),
          NyxField('design', NyxData(LPair.Design)), NyxField('view', NyxData(LView))]).ToJSON);
      end;
    end;
  finally
    FGuard.Release;
  end;
end;

function TNyxStudioMCP.Rejection(const AArguments: TNyxDataValue;
  const AOwner, AMessage: TNyxText): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LRef: TNyxReviewRef;
  LSession: TNyxAgentSession;
  LWorkspace: TNyxWorkspaceRef;
begin
  SetLength(LFields, 2);
  LFields[0] := NyxField('code', NyxData('operation_rejected'));
  LFields[1] := NyxField('message', NyxData(AMessage));
  LSession := nil;
  LRef := NyxActiveWorkspace;
  LWorkspace := NyxPrimaryWorkspace;

  if AArguments.Kind = ndObject then
  begin
    try
      LRef := NyxReviewArgument(AArguments);
      LWorkspace := NyxWorkspaceArgument(AArguments);
      LSession := ContextSession(AArguments, AOwner);
    except
      on Exception do
      begin
        { Context admission already refused. Do not disclose another owner's
          revision or label the user's active revision as this review's value. }
        LSession := nil;
      end;
    end;
  end;

  if LSession <> nil then
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField('currentRevision', NyxData(LSession.Revision));

    if LRef.ID <> '' then
    begin
      SetLength(LFields, Length(LFields) + 1);
      LFields[High(LFields)] := NyxField('review', NyxData(LRef.ID));
    end;
    if LWorkspace.ID <> '' then
    begin
      SetLength(LFields, Length(LFields) + 1);
      LFields[High(LFields)] := NyxField('workspace', NyxData(LWorkspace.ID));
    end;
  end;
  Result := NyxObject(LFields);
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
  LReview: TNyxReviewRef;
  LSession: TNyxAgentSession;
  LWorkspace: TNyxWorkspaceRef;
begin
  while FBuilds.TakeCompletion(LActor, LOutcome, LPair, LReport, LReview, LWorkspace) do
  begin

    if LWorkspace.ID <> '' then
    begin
      LSession := FWorkspaces.Find(LWorkspace);
    end
    else
    begin
      LSession := FReviews.Find(LReview);
    end;

    if (LReport <> nil) and (LSession <> nil) and LSession.CurrentPair(LPair) then
    begin
      LSession.PublishCompilerReport(LReport);
    end
    else if (LSession = nil) or not LSession.CurrentPair(LPair) then
    begin
      LOutcome := LOutcome + CEarlierDesign;
    end;

    if LReview.ID <> '' then
    begin
      LOutcome := LReview.ID + ' / ' + LOutcome;
    end;
    if LWorkspace.ID <> '' then
    begin
      LOutcome := LWorkspace.ID + ' / ' + LOutcome;
    end;
    FCore.RecordActivity(LActor, 'nyx_build', LOutcome);
  end;
end;

function TNyxStudioMCP.BuildTool(const AWireArguments: TNyxDataValue;
  const AActor, AOwner: TNyxText; AAuthority: TNyxBuildAuthority): TNyxDataValue;
const
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
  AArguments: TNyxDataValue;
  LReview: TNyxReviewRef;
  LSession: TNyxAgentSession;
  LRetryOwner: TNyxText;
  LWorkspace: TNyxWorkspaceRef;
  LProfile: TNyxOutputConfiguration;
begin
  LWorkspace := NyxWorkspaceArgument(AWireArguments);
  LReview := NyxReviewArgument(AWireArguments);

  if AAuthority = baEditor then
  begin

    if LReview.ID <> '' then
    begin
      raise ENyxProjectConflict.Create('Operator compilation requires a project context');
    end;
    LSession := FWorkspaces.Find(LWorkspace);

    if LSession = nil then
    begin
      raise ENyxProjectConflict.Create('Editor project is missing or closed');
    end;
  end
  else
  begin
    LSession := ContextSession(AWireArguments, AOwner, AActor);
  end;
  AArguments := NyxReviewArguments(NyxWorkspaceArguments(AWireArguments));
  LRetryOwner := NyxObject([NyxField('owner', NyxData(AOwner)),
    NyxField('authority', NyxData(Ord(AAuthority))),
    NyxField('review', NyxData(LReview.ID)),
    NyxField('workspace', NyxData(LWorkspace.ID))]).ToJSON;

  if (AAuthority = baAgent) and (FCore.Permission = apDisabled) then
  begin
    raise ENyxProjectConflict.Create('Agent access is disabled in Studio');
  end;
  LMode := AArguments.Field('mode').AsText;

  if (AAuthority = baEditor) and (LMode = 'profile') then
  begin
    NyxAgentFields(AArguments, '|mode|profile|expectedOutputID|');

    if NyxAgentHas(AArguments, 'expectedOutputID') and
      not NyxAgentHas(AArguments, 'profile') then
    begin
      raise ENyxProjectConflict.Create('A profile identity accompanies a profile save');
    end;

    if NyxAgentHas(AArguments, 'profile') then
    begin

      if AArguments.Field('expectedOutputID').AsText <>
        NyxBuildFingerprint(FBuilds.OperatorProfile) then
      begin
        raise ENyxProjectConflict.Create('Output configuration changed; local profile retained');
      end;
      LItem := AArguments.Field('profile');
      { Validate before asking the host to persist. Configure itself re-admits
        the same versioned text; a failed write never publishes a new profile. }
      LProfile := TNyxOutputConfiguration.Decode(LItem.ToJSON);
      LProfile.Free;

      if Assigned(FOnOperatorProfileChange) then
      begin
        FOnOperatorProfileChange(LItem.ToJSON);
      end;
      FBuilds.Configure(LItem.ToJSON);
    end;
    Exit(NyxObject([NyxField('profile', TNyxDataValue.ParseJSON(FBuilds.OperatorProfile)),
      NyxField('outputID', NyxData(NyxBuildFingerprint(FBuilds.OperatorProfile)))]));
  end;

  if LMode = 'outputs' then
  begin
    NyxAgentFields(AArguments, '|mode|');
    Exit(NyxWithWorkspace(NyxWithReview(FBuilds.Outputs, LReview), LWorkspace));
  end;

  if LMode = 'status' then
  begin

    if (FBuilds.Context(AArguments.Field('job').AsText).ID <> LReview.ID) or
      (FBuilds.WorkspaceContext(AArguments.Field('job').AsText).ID <> LWorkspace.ID) then
    begin
      raise ENyxProjectConflict.Create('Build job belongs to a different workspace');
    end;
    Result := FBuilds.Status(AArguments, LPair, LCurrentOutput);
    LCurrent := LSession.CurrentPair(LPair);
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
    LFields[LIndex + 1] := NyxField('currentRevision', NyxData(LSession.Revision));
    LFields[LIndex + 2] := NyxField('currentOutput', NyxData(LCurrentOutput));
    Result := NyxObject(LFields);
    Result := NyxWithWorkspace(NyxWithReview(Result, LReview), LWorkspace);

    if Length(Result.ToJSON) > 48 * 1024 then
    begin
      raise ENyxProjectConflict.Create('Build status exceeds response budget; use a smaller window');
    end;
    Exit;
  end;

  if LMode = 'cancel' then
  begin
    FBuilds.AdmitCancel(AArguments);

    if (AAuthority = baAgent) and (FCore.Permission <> apEdit) then
    begin
      raise ENyxProjectConflict.Create('Agent cancellation requires Allow edits in Studio');
    end;

    if (FBuilds.Context(AArguments.Field('job').AsText).ID <> LReview.ID) or
      (FBuilds.WorkspaceContext(AArguments.Field('job').AsText).ID <> LWorkspace.ID) then
    begin
      raise ENyxProjectConflict.Create('Build job belongs to a different workspace');
    end;

    if FBuilds.Retry(LRetryOwner, AArguments, Result) then
    begin
      Exit(NyxWithWorkspace(NyxWithReview(Result, LReview), LWorkspace));
    end;

    if AArguments.Field('expectedRevision').AsInteger <> LSession.Revision then
    begin
      raise ENyxProjectConflict.Create('Project revision changed; inspect before cancelling');
    end;
    Result := FBuilds.Cancel(LRetryOwner, AArguments, AAuthority = baEditor);
    FCore.RecordActivity(AActor, 'nyx_build', Result.Field('state').AsText);
    Exit(NyxWithWorkspace(NyxWithReview(Result, LReview), LWorkspace));
  end;

  if LMode <> 'request' then
  begin
    raise ENyxProjectConflict.Create('Build mode must be outputs, request, status or cancel');
  end;
  FBuilds.AdmitRequest(AArguments);

  if (AAuthority = baAgent) and (FCore.Permission <> apEdit) then
  begin
    raise ENyxProjectConflict.Create('Agent builds require Allow edits in Studio');
  end;

  if FBuilds.Retry(LRetryOwner, AArguments, Result) then
  begin
    FCore.RecordActivity(AActor, 'nyx_build', 'retry returned original job receipt');
    Result := NyxWithWorkspace(NyxWithReview(Result, LReview), LWorkspace);
    Exit;
  end;
  LScope := ParseNyxBuildScope(AArguments.Field('scope').AsText);
  LView := '';

  if NyxAgentHas(AArguments, 'view') then
  begin
    LView := AArguments.Field('view').AsText;
  end;

  if AAuthority = baEditor then
  begin
    LPair := LSession.EditorBuildPair(AArguments.Field('expectedRevision').AsInteger, LScope, LView);
  end
  else
  begin
    LPair := LSession.BuildPair(AArguments.Field('expectedRevision').AsInteger, LScope, LView);
  end;
  Result := NyxWithWorkspace(NyxWithReview(FBuilds.Submit(AActor, AArguments,
    LPair, LReview, LRetryOwner, LWorkspace), LReview), LWorkspace);
  FCore.RecordActivity(AActor, 'nyx_build', Result.Field('state').AsText + CSeparator +
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

{ Add routing only to each tool's outer argument object/alternative. Nested
  callback changes, property patches and conditions retain their exact closed
  schemas. Omitting review always denotes the user's active workspace. }
function ReviewSchema(const ASchema: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LProperties: array of TNyxDataField;
  LVariants: array of TNyxDataValue;
  LValue: TNyxDataValue;
  LExclusive: TNyxDataValue;
  LHasNot: Boolean;
  LIndex: Integer;
  LProperty: Integer;
begin
  LHasNot := NyxAgentHas(ASchema, 'not');
  LExclusive := NyxObject([
    NyxField('required', NyxArray([NyxData('review'), NyxData('workspace')]))]);
  SetLength(LFields, ASchema.Count);

  if not LHasNot then
  begin
    SetLength(LFields, Length(LFields) + 1);
  end;
  for LIndex := 0 to ASchema.Count - 1 do
  begin
    LValue := ASchema.Field(ASchema.Key(LIndex));

    if ASchema.Key(LIndex) = 'properties' then
    begin
      SetLength(LProperties, LValue.Count + 2);
      for LProperty := 0 to LValue.Count - 1 do
      begin
        LProperties[LProperty] := NyxField(LValue.Key(LProperty),
          LValue.Field(LValue.Key(LProperty)));
      end;
      LProperties[LValue.Count] := NyxField('review', NyxObject([
        NyxField('type', NyxData('string')), NyxField('minLength', NyxData(1)),
        NyxField('maxLength', NyxData(120)),
        NyxField('description', NyxData('Exact owned nyx_reviews handle; omitted uses the active user design. Never infer it from a root ID.'))]));
      LProperties[LValue.Count + 1] := NyxField('workspace', NyxObject([
        NyxField('type', NyxData('string')), NyxField('minLength', NyxData(1)),
        NyxField('maxLength', NyxData(120)),
        NyxField('description', NyxData('Exact nyx_workspaces project handle. Omit both context fields for the stable primary project. Supply workspace or review, never both. Editor navigation cannot retarget this request.'))]));
      LValue := NyxObject(LProperties);
    end
    else if ASchema.Key(LIndex) = 'oneOf' then
    begin
      SetLength(LVariants, LValue.Count);
      for LProperty := 0 to LValue.Count - 1 do
      begin
        LVariants[LProperty] := ReviewSchema(LValue.Item(LProperty));
      end;
      LValue := NyxArray(LVariants);
    end
    else if ASchema.Key(LIndex) = 'not' then
    begin
      { A callback review alternative already excludes operation/review IDs.
        not(A or B) retains that prohibition and adds context exclusivity without
        duplicating a JSON key or weakening the original schema condition. }
      LValue := NyxObject([NyxField('anyOf', NyxArray([LValue, LExclusive]))]);
    end;
    LFields[LIndex] := NyxField(ASchema.Key(LIndex), LValue);
  end;
  { Context exclusivity is discoverable as well as enforced by routing. Nested
    operation schemas retain their original closed fields and alternatives. }
  if not LHasNot then
  begin
    LFields[ASchema.Count] := NyxField('not', LExclusive);
  end;
  Result := NyxObject(LFields);
end;

function Tool(const AName, ADescription: TNyxText; const ASchema: TNyxDataValue;
  AReadOnly: Boolean): TNyxDataValue;
var
  LSchema: TNyxDataValue;
begin
  LSchema := ASchema;

  if (AName <> 'nyx_reviews') and (AName <> 'nyx_workspaces') then
  begin
    LSchema := ReviewSchema(ASchema);
  end;
  Result := NyxObject([NyxField('name', NyxData(AName)),
    NyxField('description', NyxData(ADescription)), NyxField('inputSchema', LSchema),
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

function ContentRuleSchema(const AScope: TNyxText;
  const AWhen: TNyxDataValue): TNyxDataValue;
begin
  Result := Schema(NyxObject([
    NyxField('scope', NyxObject([NyxField('const', NyxData(AScope))])),
    NyxField('platform', NyxObject([NyxField('enum', NyxArray([
      NyxData('any'), NyxData('browser'), NyxData('native-lcl')]))])),
    NyxField('component', TNyxDataValue.ParseJSON(
      '{"type":"string","minLength":1,"maxLength":128}')),
    NyxField('when', AWhen)]),
    [NyxData('scope'), NyxData('platform'), NyxData('component'), NyxData('when')]);
end;

function ContentOperationSchema: TNyxDataValue;
var
  LViewport: TNyxDataValue;
  LRules: TNyxDataValue;
begin
  LViewport := Schema(NyxObject([
    NyxField('widthMinimum', IntSchema(0, High(Integer))),
    NyxField('widthMaximum', IntSchema(0, High(Integer))),
    NyxField('heightMinimum', IntSchema(0, High(Integer))),
    NyxField('heightMaximum', IntSchema(0, High(Integer))),
    NyxField('orientation', NyxObject([NyxField('enum', NyxArray([
      NyxData('any'), NyxData('portrait'), NyxData('landscape'), NyxData('square')]))]))]),
    [NyxData('widthMinimum'), NyxData('widthMaximum'), NyxData('heightMinimum'),
      NyxData('heightMaximum'), NyxData('orientation')]);
  LRules := NyxObject([NyxField('type', NyxData('array')),
    NyxField('maxItems', NyxData(64)),
    NyxField('items', NyxObject([NyxField('oneOf', NyxArray([
      ContentRuleSchema('default', NyxObject([NyxField('type', NyxData('null'))])),
      ContentRuleSchema('viewport', LViewport),
      ContentRuleSchema('presentation', TNyxDataValue.ParseJSON(
        '{"type":"string","minLength":1,"maxLength":128}'))]))]))]);
  Result := Schema(NyxObject([
    NyxField('op', NyxObject([NyxField('const', NyxData('content-set'))])),
    NyxField('id', TextSchema('Exact authored reusable instance ID')),
    NyxField('content', Schema(NyxObject([
      NyxField('version', NyxObject([NyxField('const', NyxData(1))])),
      NyxField('rules', LRules)]), [NyxData('version'), NyxData('rules')]))]),
    [NyxData('op'), NyxData('id'), NyxData('content')]);
end;

function NyxStudioMCPTools: TNyxDataValue;
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
    '{"type":"object","properties":{"op":{"const":"tokens"},"values":{"type":"object"}},"required":["op","values"],"additionalProperties":false},' +
    ContentOperationSchema.ToJSON + ',' +
    '{"type":"object","properties":{"op":{"const":"derive"},"source":{"type":"string"},"id":{"type":"string"},"identities":{"type":"object","additionalProperties":{"type":"string"}}},"required":["op","source","id","identities"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"instance"},"id":{"type":"string"},"component":{"type":"string"},"parent":{"type":"string"},"index":{"type":"integer","minimum":0}},"required":["op","id","component","parent"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"override"},"id":{"type":"string"},"instance":{"type":"string"},"path":{"type":"string"},"mode":{"enum":["properties","append","prepend","replace","remove"]}},"required":["op","id","instance","path","mode"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"inherit"},"id":{"type":"string"},"instance":{"type":"string"},"path":{"type":"string"}},"required":["op","id","instance","path"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"place"},"id":{"type":"string"},"target":{"type":"string"},"placement":{"enum":["inside","before","after"]}},"required":["op","id","target","placement"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"place-new"},"kind":{"type":"string"},"id":{"type":"string"},"target":{"type":"string"},"placement":{"enum":["inside","before","after"]}},"required":["op","kind","id","target","placement"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"presentation-define"},"name":{"type":"string","minLength":1,"maxLength":128},"widthMinimum":{"type":"integer","minimum":0,"maximum":2147483647},"widthMaximum":{"type":"integer","minimum":0,"maximum":2147483647},"heightMinimum":{"type":"integer","minimum":0,"maximum":2147483647},"heightMaximum":{"type":"integer","minimum":0,"maximum":2147483647},"orientation":{"enum":["any","portrait","landscape","square"]},"activation":{"enum":["automatic","manual"]},"container":{"type":"string","maxLength":128}},"required":["op","name","widthMinimum","widthMaximum","heightMinimum","heightMaximum","orientation"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"presentation-remove"},"name":{"type":"string","minLength":1,"maxLength":128}},"required":["op","name"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"enum":["presentation-use","presentation-reset"]},"name":{"type":"string","minLength":1,"maxLength":128},"id":{"type":"string"},"attribute":{"type":"string"},"platform":{"enum":["any","browser","native-lcl"]}},"required":["op","name","id","attribute","platform"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"presentation-set"},"name":{"type":"string","minLength":1,"maxLength":128},"id":{"type":"string"},"attribute":{"type":"string"},"platform":{"enum":["any","browser","native-lcl"]},"value":{"type":["string","boolean","number"]}},"required":["op","name","id","attribute","platform","value"],"additionalProperties":false}]}}');
  Result := NyxObject([NyxField('tools', NyxArray([
    Tool('nyx_session', 'Inspect current revision, selection, active view, permissions and undo/draft state. No document dump.', Schema(NyxObject([]), []), True),
    Tool('nyx_outline', 'Page through pages, reusable definitions or one component''s immediate children. Descend by parent ID.',
      Schema(NyxObject([NyxField('parent', TextSchema('Optional exact component ID')),
        NyxField('scope', NyxObject([NyxField('enum', NyxArray([NyxData('pages'), NyxData('components')]))])),
        NyxField('offset', LPage.Field('offset')), NyxField('limit', LPage.Field('limit'))]), []), True),
    Tool('nyx_node', 'Inspect one component''s paged typed properties and optionally events, registrations, semantic source routes and reachable effective named parts. parts=true returns at most partLimit paths with exact source/design and local override identity. Removed parts are absent; inspect the definition separately for inherited paths. Routes and registrations page across the requested event window. Omitted ID uses selection.',
      Schema(NyxObject([NyxField('id', TextSchema('Exact component ID')),
        NyxField('offset', LPage.Field('offset')), NyxField('limit', LPage.Field('limit')),
        NyxField('events', LBoolean),
        NyxField('parts', LBoolean),
        NyxField('content', LBoolean),
        NyxField('contentOffset', IntSchema(0, 100000)),
        NyxField('contentLimit', IntSchema(1, 16)),
        NyxField('partOffset', IntSchema(0, 100000)),
        NyxField('partLimit', IntSchema(1, 50)),
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
    Tool('nyx_presentations', 'Inspect one exact named presentation or at most 16 definitions per page (default 8). Names are exact, case-sensitive Unicode application references; max 64 per document. Automatic definitions combine logical width, height and orientation. An optional exact container name selects the nearest eligible measured ancestor content box; omission measures the whole view. Width containment supports width rules; size containment also supports height/orientation. Missing boxes stay inactive. Manual definitions require activation manual, all bounds zero, orientation any and no container; one manual choice may be previewed alongside automatic rules. Controls use typed WhenPresentation scopes. In nyx_transaction, presentation-define creates/replaces a shared definition; presentation-use initializes a supported override; presentation-set upserts one typed scalar. presentation-reset removes one exact override; presentation-remove refuses remaining references. Group related edits as one paired Undo step. Queries preserve navigation/history; no document dump.',
      Schema(NyxObject([NyxField('name', TextSchema('Optional exact presentation name; excludes pagination')),
        NyxField('offset', IntSchema(0, 64)), NyxField('limit', IntSchema(1, 16))]), []), True),
    Tool('nyx_state', 'Inspect paged authored defaults (case-sensitive name substring filter), exact text windows in Unicode scalars, or supported/local/effective bindings for an exact authored owner. Defaults previews contain at most 80 scalars; value windows at most 4096. Apply 1..32 ordered create/set/rename/remove/bind/clear-binding/inherit-binding changes as ONE paired Undo step. Primitive types and scalar families are exact. Rename updates authored references across pages and reusable definitions. Clear deliberately masks inheritance; inherit removes a local descriptor. Existing named-part override IDs are supported; this tool does not create overrides. Clear dependent bindings before removing a default. Apply requires Allow edits, current expectedRevision, unique operationId and no draft. Queries do not change selection or history; operator activity shows success and refusal.',
      NyxStateAgentSchema, False),
    Tool('nyx_collections', 'Inspect document defaults with bounded list/schema/rows/domain pages, Unicode scalar value/title windows, and local/effective authored view bindings. Row queries return IDs unless exact fields are requested (up to 16); text previews contain at most 80 scalars. Runtime stores are independent. Apply 1..32 ordered typed changes as ONE paired Undo step: define named schema/initial rows, add/update a named field, remove a field, append/update/move a scoped row, bind a full fluent view, or reuse seventeen ordinary editor intents. Field definitions carry typed defaults/domains; partial updates preserve other cells. Existing families cannot change. Bind uses the versioned collection-view descriptor. Clear masks inheritance; inherit removes a local override. Named keys/rows are exact and case-sensitive. Clear dependencies before removal; each ordered intermediate must admit. Current revision, unique operationId, Allow edits and no draft are required. Queries preserve navigation/history; activity shows success and refusal.',
      NyxCollectionAgentSchema, False),
    Tool('nyx_diagnostics', 'Page through compiler diagnostics. Locations are Unicode scalar coordinates in submitted source; stale locations cannot navigate.', Schema(LPage, []), True),
    Tool('nyx_source', 'Read only the needed accepted Pascal lines, e.g. around a compiler diagnostic. Does not return pending drafts.',
      Schema(NyxObject([NyxField('line', IntSchema(1, 100000)), NyxField('count', IntSchema(1, 80))]), []), True),
    Tool('nyx_transaction', 'Apply 1..64 semantic operations atomically as ONE undoable paired design/Pascal edit. Place moves an exact authored control relative to target: inside appends to an editable container, before/after use its owner and resolve ordering after detaching the source. Place-new creates an unused catalog control/recipe at that location. Roots, cycles, self-placement, leaf containers and inherited instance content refuse. Customize a named layout part first. Derive copies an exact subtree into a reusable root; identities maps every descendant source ID, excluding the root. Instance inserts a reusable reference. Override edits an exact instance-owned named-part descriptor; create/move payload and update typed properties in the same group. Inherit removes the exact matching descriptor/payload. Use nyx_node for bounded parts and property types. Incomplete payloads, foreign/occupied identities, invalid paths and drafts reject the whole group. Supply current expectedRevision; operationId deduplicates the last 64 successful mutations per session.',
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
    Tool('nyx_preview', 'Selectively render an immutable revision/view snapshot with Nyx. Optional presentation selects one exact manual definition; omit/null retains automatic defaults. It changes no editor selection or history. Returns a preview link; capture=true additionally returns an actual browser PNG when the local renderer is available.',
      Schema(NyxObject([NyxField('expectedRevision', IntSchema(1, High(Integer))),
        NyxField('view', TextSchema('Exact page or reusable root ID')),
        NyxField('width', IntSchema(320, 1600)), NyxField('height', IntSchema(240, 1200)),
        NyxField('presentation', TNyxDataValue.ParseJSON('{"type":["string","null"],"minLength":1,"maxLength":128}')),
        NyxField('capture', LBoolean)]), [NyxData('expectedRevision'), NyxData('view')]), True),
    Tool('nyx_callbacks', 'Author 1..32 ordered add/policy/move/remove changes as ONE undoable paired source edit. Add returns crafted handler/registration names and final-source TODO lines. Inspect registrations with nyx_node. Results describe each operation in order. Apply requires expectedRevision and operationId; drafts reject. Before removal, review the exact batch for warnings and reviewID, then apply unchanged at that revision/actor. Review does not edit or add history; removal retains Pascal implementations.',
      CallbackSchema, False),
    Tool('nyx_build', 'Inspect readiness, request an immutable accepted build, page bounded diagnostics, or cancel an owned job. Request/cancel require Allow edits, exact project revision and operationId. Cancel requires the admitting connection and exact project/review; operators may cancel jobs in their project. Two running slots, eight FIFO queued jobs, sixteen retained handles. Queued jobs never spawn when cancelled; cancelling retains its slot until process and worker join. Exact retries return the original receipt without rebuilding. No document history changes, compiler paths/options or source overrides. Only succeeded status advertises artifacts; cancellation retains the accepted source, report and preview.',
      TNyxDataValue.ParseJSON('{"type":"object","oneOf":[' +
        '{"type":"object","properties":{"mode":{"const":"outputs"}},"required":["mode"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"status"},"job":{"type":"string"},"offset":{"type":"integer","minimum":0,"maximum":512},"limit":{"type":"integer","minimum":1,"maximum":20},"severity":{"enum":["all","error","fatal","warning","hint","note","info"]}},"required":["mode","job"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"cancel"},"job":{"type":"string","minLength":36,"maxLength":36},"expectedRevision":{"type":"integer","minimum":1},"operationId":{"type":"string","minLength":1,"maxLength":120}},"required":["mode","job","expectedRevision","operationId"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"request"},"expectedRevision":{"type":"integer","minimum":1},"operationId":{"type":"string","minLength":1,"maxLength":120},"outputID":{"type":"string","minLength":32,"maxLength":32},"target":{"enum":["browser","lcl"]},"scope":{"enum":["view","reusable","application"]},"view":{"type":"string","minLength":1}},"required":["mode","expectedRevision","operationId","outputID","target","scope"],"allOf":[{"if":{"properties":{"scope":{"const":"application"}}},"then":{"not":{"required":["view"]}},"else":{"required":["view"]}}],"additionalProperties":false}]}'), False),
    Tool('nyx_pascal', 'Inspect bounded Unicode-scalar windows of one exact local callback implementation, or replace 1..16 implementations as ONE paired undoable source edit. Discover handler names through nyx_node/nyx_callbacks. Inspect returns accepted text after its immutable signature through end;, including whitespace and local declarations. Concatenate windows at the same revision for expected. Apply requires exact expected implementation text, current revision, unique operationId, Allow edits and no pending draft; signatures, imports, sibling helpers and managed views are retained. Duplicate or ambiguous/directive methods refuse. Syntax/type diagnostics come from nyx_build, not source admission. Imports mode pages exact interface/implementation namespaces and source lines. edit-imports applies 1..32 ordered add/remove changes as one paired Undo step, retaining comments and authored order. Namespace identity is Pascal case insensitive; duplicate additions, missing removals, file clauses and conditional/directive clauses refuse. Drafts/revision/permission/authority/retry guards apply. Import resolution and helper diagnostics still require nyx_build. Routines pages top-level names/kinds/lines and editability reasons (20 default, 50 maximum). Routine reads at most 4096 Unicode scalars of one exact implementation. edit-routines replaces 1..16 implementations as one paired Undo step with exact expected text. Ordinary functions/procedures and qualified methods including constructors/destructors are supported; nested routines belong to their parent. Signatures, imports, surrounding helpers and managed code remain retained. Overloads/duplicates, directives, forward/external and compiler-managed infrastructure refuse edits. Implementation editing retains signatures and class declarations. Declaration reads bounded exact interface or implementation signature windows through part; a private helper has an empty interface counterpart. edit-declarations applies 1..16 ordered create/edit/remove/signature operations as one paired Undo step. Creation supplies typed procedure/function kind, interface/implementation visibility and exact signature/code fragments. Edit retains signatures. Remove requires exact implementation signature/body and interface counterpart; any possible retained lexical reference blocks removal. Creation/removal never change class-member signatures or managed infrastructure. Signature supplies typed kind, retained visibility, complete replacement signature/body and all three exact expected counterparts. Public counterpart replacement is paired; identity stays retained. Related caller edits share the group. External-unit callers/type correctness still require compiler diagnostics. This tool does not execute code or change compiler profiles.',
      TNyxDataValue.ParseJSON('{"type":"object","oneOf":[' +
        '{"type":"object","properties":{"mode":{"const":"inspect"},"handler":{"type":"string"},"offset":{"type":"integer","minimum":0,"maximum":4194304},"count":{"type":"integer","minimum":1,"maximum":4096}},"required":["mode","handler"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"apply"},"expectedRevision":{"type":"integer","minimum":1},"operationId":{"type":"string","minLength":1,"maxLength":120},"changes":{"type":"array","minItems":1,"maxItems":16,"items":{"type":"object","properties":{"handler":{"type":"string"},"expected":{"type":"string","maxLength":32768},"implementation":{"type":"string","maxLength":32768}},"required":["handler","expected","implementation"],"additionalProperties":false}}},"required":["mode","expectedRevision","operationId","changes"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"imports"},"section":{"enum":["interface","implementation"]},"offset":{"type":"integer","minimum":0,"maximum":256},"limit":{"type":"integer","minimum":1,"maximum":50}},"required":["mode","section"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"edit-imports"},"expectedRevision":{"type":"integer","minimum":1},"operationId":{"type":"string","minLength":1,"maxLength":120},"changes":{"type":"array","minItems":1,"maxItems":32,"items":{"type":"object","properties":{"op":{"enum":["add","remove"]},"section":{"enum":["interface","implementation"]},"unit":{"type":"string","minLength":1,"maxLength":120}},"required":["op","section","unit"],"additionalProperties":false}}},"required":["mode","expectedRevision","operationId","changes"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"routines"},"offset":{"type":"integer","minimum":0,"maximum":4096},"limit":{"type":"integer","minimum":1,"maximum":50}},"required":["mode"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"routine"},"routine":{"type":"string","minLength":1,"maxLength":120},"offset":{"type":"integer","minimum":0,"maximum":4194304},"count":{"type":"integer","minimum":1,"maximum":4096}},"required":["mode","routine"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"edit-routines"},"expectedRevision":{"type":"integer","minimum":1},"operationId":{"type":"string","minLength":1,"maxLength":120},"changes":{"type":"array","minItems":1,"maxItems":16,"items":{"type":"object","properties":{"routine":{"type":"string","minLength":1,"maxLength":120},"expected":{"type":"string","maxLength":32768},"implementation":{"type":"string","maxLength":32768}},"required":["routine","expected","implementation"],"additionalProperties":false}}},"required":["mode","expectedRevision","operationId","changes"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"declaration"},"routine":{"type":"string","minLength":1,"maxLength":120},"part":{"enum":["interface","implementation"]},"offset":{"type":"integer","minimum":0,"maximum":4194304},"count":{"type":"integer","minimum":1,"maximum":4096}},"required":["mode","routine"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"edit-declarations"},"expectedRevision":{"type":"integer","minimum":1},"operationId":{"type":"string","minLength":1,"maxLength":120},"changes":{"type":"array","minItems":1,"maxItems":16,"items":{"oneOf":[{"type":"object","properties":{"op":{"const":"create"},"routine":{"type":"string","minLength":1,"maxLength":120},"kind":{"enum":["procedure","function"]},"visibility":{"enum":["interface","implementation"]},"signature":{"type":"string","maxLength":32768},"implementation":{"type":"string","maxLength":32768}},"required":["op","routine","kind","visibility","signature","implementation"],"additionalProperties":false},{"type":"object","properties":{"op":{"const":"edit"},"routine":{"type":"string","minLength":1,"maxLength":120},"expectedImplementation":{"type":"string","maxLength":32768},"implementation":{"type":"string","maxLength":32768}},"required":["op","routine","expectedImplementation","implementation"],"additionalProperties":false},{"type":"object","properties":{"op":{"const":"remove"},"routine":{"type":"string","minLength":1,"maxLength":120},"expectedSignature":{"type":"string","maxLength":32768},"expectedImplementation":{"type":"string","maxLength":32768},"expectedDeclaration":{"type":"string","maxLength":32768}},"required":["op","routine","expectedSignature","expectedImplementation","expectedDeclaration"],"additionalProperties":false},{"type":"object","properties":{"op":{"const":"signature"},"routine":{"type":"string","minLength":1,"maxLength":120},"kind":{"enum":["procedure","function"]},"visibility":{"enum":["interface","implementation"]},"signature":{"type":"string","maxLength":32768},"implementation":{"type":"string","maxLength":32768},"expectedSignature":{"type":"string","maxLength":32768},"expectedImplementation":{"type":"string","maxLength":32768},"expectedDeclaration":{"type":"string","maxLength":32768}},"required":["op","routine","kind","visibility","signature","implementation","expectedSignature","expectedImplementation","expectedDeclaration"],"additionalProperties":false}]}}},"required":["mode","expectedRevision","operationId","changes"],"additionalProperties":false}]}'), False),
    Tool('nyx_roots', 'Review removal of 1..16 exact page/reusable roots, then apply the unchanged group as ONE paired Undo step. Inspect roots through nyx_outline. Review reports descendant/callback counts and retained reusable dependencies; dependencies block removal. Pascal imports, helpers, handler classes and state defaults remain; compile afterward to check application references to removed IDs. Review requires expectedRevision and no draft; apply also requires Allow edits, unique operationId and its current actor/revision/exact-roots reviewID. Eight reviews are retained. This is active-document cleanup, not an isolated review workspace.',
      TNyxDataValue.ParseJSON('{"type":"object","properties":{' +
        '"mode":{"enum":["review","apply"]},"expectedRevision":{"type":"integer","minimum":1},' +
        '"operationId":{"type":"string","minLength":1,"maxLength":120},"reviewID":{"type":"string","minLength":1},' +
        '"roots":{"type":"array","minItems":1,"maxItems":16,"items":{"type":"object","properties":{"root":{"enum":["page","component"]},"id":{"type":"string","minLength":1}},"required":["root","id"],"additionalProperties":false}}},' +
        '"required":["mode","expectedRevision","roots"],"additionalProperties":false,"allOf":[{"if":{"properties":{"mode":{"const":"apply"}}},"then":{"required":["operationId","reviewID"]},"else":{"not":{"anyOf":[{"required":["operationId"]},{"required":["reviewID"]}]}}}]}'), False),
    Tool('nyx_reviews', 'Create, inspect, list or discard an independent review workspace. Use its explicit review handle on every semantic query/edit/build/preview. Its document, accepted Pascal, selection and Undo/Redo are independent; an accepted seed excludes the user pending draft. Owner is the authenticated MCP transport session, not its display name. Eight reviews and 64 exact lifecycle receipts are bounded. Create checks the active revision; discard checks the review revision. Studio operator permissions apply immediately. Disposal never changes the user project or publishes a review into it.',
      TNyxDataValue.ParseJSON('{"type":"object","oneOf":[' +
        '{"type":"object","properties":{"mode":{"const":"list"}},"required":["mode"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"inspect"},"review":{"type":"string","minLength":1,"maxLength":120}},"required":["mode","review"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"create"},"expectedRevision":{"type":"integer","minimum":1},"operationId":{"type":"string","minLength":1,"maxLength":120},"label":{"type":"string","minLength":1,"maxLength":256},"base":{"enum":["empty","accepted"]}},"required":["mode","expectedRevision","operationId","label","base"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"discard"},"review":{"type":"string","minLength":1,"maxLength":120},"expectedRevision":{"type":"integer","minimum":1},"operationId":{"type":"string","minLength":1,"maxLength":120}},"required":["mode","review","expectedRevision","operationId"],"additionalProperties":false}]}'), False),
    Tool('nyx_workspaces', 'List, inspect or create concurrent user project sessions. Projects survive agent disconnection and are distinct from temporary owned reviews. Use an explicit workspace and its revision on each semantic operation; omitting context always addresses the stable primary project, independently of editor navigation. Creation copies an empty or primary accepted pair and excludes its pending draft. Eight additional projects and 64 creation receipts per transport are bounded. Studio operator permissions apply globally. Only the operator may close a user project after warning; MCP cannot close projects or grant access.',
      TNyxDataValue.ParseJSON('{"type":"object","oneOf":[' +
        '{"type":"object","properties":{"mode":{"const":"list"}},"required":["mode"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"inspect"},"workspace":{"type":"string","minLength":1,"maxLength":120}},"required":["mode","workspace"],"additionalProperties":false},' +
        '{"type":"object","properties":{"mode":{"const":"create"},"expectedRevision":{"type":"integer","minimum":1},"operationId":{"type":"string","minLength":1,"maxLength":120},"label":{"type":"string","minLength":1,"maxLength":256},"base":{"enum":["empty","accepted"]}},"required":["mode","expectedRevision","operationId","label","base"],"additionalProperties":false}]}'), False)
  ]))]);
end;

function TNyxStudioMCP.Tools: TNyxDataValue;
begin
  Result := NyxStudioMCPTools;
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
  LDirectory := FDirectories.Previews + NewCapability;
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
      (Pos('data-nyx-preview-width="' + IntToStr(APreview.Field('width').AsInteger) + '"', LOutput) = 0) or
      (Pos('data-nyx-preview-height="' + IntToStr(APreview.Field('height').AsInteger) + '"', LOutput) = 0) or
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

function TNyxStudioMCP.Preview(const AWireArguments: TNyxDataValue;
  const AActor, AOwner: TNyxText): TNyxDataValue;
var
  LPair: TNyxProjectPair;
  LToken: TNyxText;
  LView: TNyxText;
  LRevision: Integer;
  LWidth: Integer;
  LHeight: Integer;
  LIndex: Integer;
  AArguments: TNyxDataValue;
  LReview: TNyxReviewRef;
  LWorkspace: TNyxWorkspaceRef;
  LSelection: TNyxPresentationSelection;
  LPresentation: TNyxDataValue;
begin
  LWorkspace := NyxWorkspaceArgument(AWireArguments);
  LReview := NyxReviewArgument(AWireArguments);
  AArguments := NyxReviewArguments(NyxWorkspaceArguments(AWireArguments));
  NyxAgentFields(AArguments, '|expectedRevision|view|width|height|capture|presentation|');
  LSelection := TNyxPresentationSelection.None;
  LPresentation := NyxNull;

  if NyxAgentHas(AArguments, 'presentation') then
  begin
    LPresentation := AArguments.Field('presentation');

    if LPresentation.Kind <> ndNull then
    begin
      LSelection := TNyxPresentationSelection.Use(NyxPresentation(LPresentation.AsText));
    end;
  end;
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
    LPair := ContextSession(AWireArguments, AOwner, AActor).PreviewPair(LRevision, LView, AActor, LSelection);

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
        NyxField('view', NyxData(LView)), NyxField('revision', NyxData(LRevision)),
        NyxField('presentation', LPresentation),
        NyxField('width', NyxData(LWidth)), NyxField('height', NyxData(LHeight))]).ToJSON))]);
  finally
    FGuard.Release;
  end;
  Result := NyxObject([NyxField('revision', NyxData(LRevision)),
    NyxField('presentation', LPresentation),
    NyxField('view', NyxData(LView)), NyxField('width', NyxData(LWidth)),
    NyxField('height', NyxData(LHeight)),
    NyxField('url', NyxData('http://127.0.0.1:' + IntToStr(FStudioPort) +
      '/agent-preview.html?token=' + LToken))]);
  Result := NyxWithWorkspace(NyxWithReview(Result, LReview), LWorkspace);
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
        FGuard.Acquire;
        try
          FReviews.ReleaseOwner(LClientID);
          FWorkspaces.ReleaseOwner(LClientID);
        finally
          FGuard.Release;
        end;
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
          LResult := ToolResult(InvokeBuild(LClientID,
            FClients[LIndex].Field('actor').AsText, LArguments));
        end
        else if LTool = 'nyx_preview' then
        begin
          LResult := Preview(LArguments, FClients[LIndex].Field('actor').AsText, LClientID);
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
          LResult := ToolResult(InvokeTool(LTool, LClientID,
            FClients[LIndex].Field('actor').AsText, LArguments));
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
            LResult := ToolResult(Rejection(LArguments, LClientID,
              TNyxText(LException.Message)), True);
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
