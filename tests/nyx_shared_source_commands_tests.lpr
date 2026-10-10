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
program nyx_shared_source_commands_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Classes, SysUtils, SyncObjs, nyx.text, nyx.bytes, nyx.data, nyx.types,
  nyx.source,
  nyx.studio.projects, nyx.studio.session, nyx.studio.sourcejobs,
  nyx.studio.agentbridge, nyx.studio.exchange, nyx.studio.mcp,
  nyx.studio.directories, nyx.studio.outputs, nyx.studio.buildexecutor,
  nyx.studio.builds, nyx.studio.editorbuild, nyx.studio.workspaces,
  nyx.studio.sourceprojection, nyx.studio.sourcepublications,
  nyx.studio.sourcecompilation, nyx.studio.sourcecompilation.shared,
  nyx.studio.sourcecompilation.shared.native, nyx.studio.transport, nyx.scheduler, nyx.codec,
  nyx.studio.lcl, nyx.test.projection;

type
  { Explicit deterministic delivery to the actual owning backend. The bridge
    owns this transport; fixtures never start sockets or use production paths. }
  TEngineExchange = class(TNyxStudioEditorExchange)
  public
    Engine: TNyxStudioMCP;
    Reply: TNyxEditorReply;
    Tick: TNyxEditorTick;
    Body: TNyxDataValue;
    Token: TNyxText;
    Connecting: Boolean;
    procedure Post(AConnect: Boolean; const AToken, ABody: TNyxText;
      AReply: TNyxEditorReply); override;
    procedure CancelRequest; override;
    procedure Schedule(ADelayMS: Integer; ATick: TNyxEditorTick); override;
    procedure CancelTick; override;
    procedure Deliver;
    procedure Fire;
  end;
  { Single-journey provider: capture real precompile authority, then use actual
    FPC constructor execution and guarded native publication. Completion delivery
    is held explicitly. This tests portable coordination, not browser/HTTP input. }
  TNativeProvider = class(TInterfacedObject, INyxSharedSourceCompilerFactory,
    INyxSharedVisualSourceCompilerFactory,
    INyxSharedSourceCompiler, INyxSourceCompilation)
  public
    Engine: TNyxStudioMCP;
    Capability: TNyxText;
    Issuer: TNyxText;
    Workspace: TNyxWorkspaceRef;
    Revision: Integer;
    Source: TNyxText;
    Ticket: TNyxStudioSourcePublication;
    Port: INyxSharedSourceCompilationPort;
    StateValue: TNyxSourceCompilationState;
    Receipt: TNyxSourcePublicationReceipt;
    Intent: TNyxDataValue;
    Starts: Integer;
    function CreateCompiler(const ACapability, AIssuer: TNyxText;
      const AWorkspace: TNyxWorkspaceRef; ARevision: Integer): INyxSharedSourceCompiler;
    function CreateVisualCompiler(const ACapability, AIssuer: TNyxText;
      const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
      const AIntent: TNyxDataValue): INyxSharedSourceCompiler;
    function Start(const ASource: TNyxText;
      const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
    function GetState: TNyxSourceCompilationState;
    procedure Cancel;
    procedure Publish(const ABuild: INyxSourceProjectionBuild);
    procedure Deliver(const AProjection: INyxSourceProjection; AWrongReceipt: Boolean = False);
    procedure Refuse;
  end;
  TScenario = (ssNormal, ssTypingDuringCompile, ssTypingDuringAck,
    ssWrongReceipt, ssRemoteRace, ssCancelledAfterCommit, ssRetired, ssRefused,
    ssQueuedEdit, ssReloaded);
  TVisualScenario = (vsOrdered, vsTypingDuringCompile, vsTypingDuringAck,
    vsWrongReceipt, vsCancelledAfterCommit, vsRetired, vsRefused, vsBackendRace);
  TTransportScenario = (tsOrdinary, tsLostReplies, tsWrongAcknowledgement, tsCancelled,
    tsRefused, tsRetired, tsMalformedAcknowledgement, tsServerErrorAcknowledgement,
    tsUnconfirmedDeadline);
  { Transparent test instrumentation retains the actual returned producer
    tokens, so cancellation/retirement is qualified from terminal native state,
    never inferred from an editor's locally idle presentation or a timeout. }
  TObservedFactory = class(TInterfacedObject, INyxSharedSourceCompilerFactory,
    INyxSharedVisualSourceCompilerFactory)
  public
    Inner: INyxSharedSourceCompilerFactory;
    Operations: array of INyxSourceCompilation;
    function CreateCompiler(const ACapability, AIssuer: TNyxText;
      const AWorkspace: TNyxWorkspaceRef; ARevision: Integer): INyxSharedSourceCompiler;
    function CreateVisualCompiler(const ACapability, AIssuer: TNyxText;
      const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
      const AIntent: TNyxDataValue): INyxSharedSourceCompiler;
    function Terminal: Boolean;
  end;
  TObservedCompiler = class(TInterfacedObject, INyxSharedSourceCompiler)
  public
    Inner: INyxSharedSourceCompiler;
    Owner: TObservedFactory;
    Lease: INyxSharedSourceCompilerFactory;
    function Start(const ASource: TNyxText;
      const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
  end;
  { The real native provider is retained unchanged. Only this worker-safe private
    transport replaces HTTP with the production owning engine. Controlled reply
    loss happens AFTER real admission/publication and never creates another job.
    Its engine is borrowed until the journey has joined all provider tokens. }
  TProviderTransport = class(TInterfacedObject, INyxNativeSourceTransport)
  private
    FGuard: TCriticalSection;
    FRequests: Integer;
    FCompletions: Integer;
    FCancels: Integer;
    FOriginalRequest: TNyxText;
    FJob: TNyxText;
    FExactRetries: Boolean;
  public
    Engine: TNyxStudioMCP;
    Scenario: TTransportScenario;
    Entered: TEvent;
    ReleaseReply: TEvent;
    constructor Create;
    destructor Destroy; override;
    function Post(const ACapability, ABody: TNyxText; AMaximumReplyBytes,
      ARemainingMS: Integer): TNyxNativeSourceReply;
    function Requests: Integer;
    function Completions: Integer;
    function Cancels: Integer;
    function ExactRetries: Boolean;
  end;
  TJourney = class
  public
    Session: TNyxStudioSession;
    Commands: TNyxSourceCommands;
    Bridge: TNyxStudioAgentBridge;
    Exchange: TEngineExchange;
    Engine: TNyxStudioMCP;
    Provider: TNativeProvider;
    Factory: INyxSharedSourceCompilerFactory;
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
    procedure Drain;
    { Wait for actual scheduler preparation, not an assumed delay. The held
      provider is the observable terminal point before real compilation. }
    procedure AwaitProvider;
    procedure AwaitCompletion;
    destructor Destroy; override;
  end;

var
  GChecks: Integer;
  GSource: TNyxText;
  GBuild: INyxSourceProjectionBuild;
  GProfile: TNyxText;
  GDirectories: TNyxStudioDirectories;

const
  CLaterNotes: TNyxText = #10 + '{ Later notes 🚀 }';

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Shared source commands: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Recovery checkpoints are opaque binary input. Text files opt in to UTF-8
  decoding below; the durable comparison never interprets checkpoint bytes. }
function ReadBytes(const APath: TNyxText): TNyxBytes;
var
  LFile: TFileStream;
begin
  Result := nil;
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if (LFile.Size < 1) or (LFile.Size > 4 * 1024 * 1024) then
    begin
      raise Exception.Create('Fixture input must be bounded');
    end;
    SetLength(Result, LFile.Size);
    LFile.ReadBuffer(Result[0], Length(Result));
  finally
    LFile.Free;
  end;
end;

function ReadText(const APath: TNyxText): TNyxText;
begin
  Result := NyxDecodeUTF8(ReadBytes(APath));
end;

function SameBytes(const ALeft, ARight: TNyxBytes): Boolean;
var
  LIndex: Integer;
begin
  Result := Length(ALeft) = Length(ARight);

  if not Result then
  begin
    Exit;
  end;
  for LIndex := 0 to High(ALeft) do
  begin

    if ALeft[LIndex] <> ARight[LIndex] then
    begin
      Exit(False);
    end;
  end;
end;

procedure TEngineExchange.Post(AConnect: Boolean; const AToken, ABody: TNyxText;
  AReply: TNyxEditorReply);
begin

  if Assigned(Reply) then
  begin
    raise Exception.Create('Only one owned editor request may wait');
  end;
  Connecting := AConnect;
  Token := AToken;
  Body := TNyxDataValue.ParseJSON(ABody);
  Reply := AReply;
end;

procedure TEngineExchange.CancelRequest;
begin
  Reply := nil;
end;

procedure TEngineExchange.Schedule(ADelayMS: Integer; ATick: TNyxEditorTick);
begin
  Tick := ATick;
end;

procedure TEngineExchange.CancelTick;
begin
  Tick := nil;
end;

procedure TEngineExchange.Fire;
var
  LTick: TNyxEditorTick;
begin
  LTick := Tick;
  Tick := nil;

  if Assigned(LTick) then
  begin
    LTick;
  end;
end;

procedure TEngineExchange.Deliver;
var
  LReply: TNyxEditorReply;
  LResponse: TNyxDataValue;
begin
  LReply := Reply;
  Reply := nil;

  if not Assigned(LReply) then
  begin
    raise Exception.Create('Delivery requires a live owned request');
  end;

  if Connecting then
  begin
    LResponse := Engine.ConnectEditor(Body);
  end
  else
  begin
    LResponse := Engine.EditorExchange(Token, Body);
  end;
  LReply(200, LResponse.ToJSON);
end;

constructor TProviderTransport.Create;
begin
  inherited Create;
  FGuard := TCriticalSection.Create;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseReply := TEvent.Create(nil, True, False, '');
  FExactRetries := True;
end;

function TObservedFactory.Terminal: Boolean;
var
  LIndex: Integer;
begin
  Result := Length(Operations) > 0;
  for LIndex := 0 to High(Operations) do
  begin
    Result := Result and (Operations[LIndex].State in [scsCompleted, scsCancelled, scsFailed]);
  end;
end;

function TObservedFactory.CreateCompiler(const ACapability, AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer): INyxSharedSourceCompiler;
var
  LCompiler: TObservedCompiler;
begin
  LCompiler := TObservedCompiler.Create;
  Result := LCompiler;
  LCompiler.Owner := Self;
  LCompiler.Lease := Self;
  LCompiler.Inner := Inner.CreateCompiler(ACapability, AIssuer, AWorkspace, ARevision);
end;

function TObservedFactory.CreateVisualCompiler(const ACapability, AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const AIntent: TNyxDataValue): INyxSharedSourceCompiler;
var
  LCompiler: TObservedCompiler;
  LVisual: INyxSharedVisualSourceCompilerFactory;
begin

  if not Supports(Inner, INyxSharedVisualSourceCompilerFactory, LVisual) then
  begin
    raise Exception.Create('Real native provider lacks its visual continuation contract');
  end;
  LCompiler := TObservedCompiler.Create;
  Result := LCompiler;
  LCompiler.Owner := Self;
  LCompiler.Lease := Self;
  LCompiler.Inner := LVisual.CreateVisualCompiler(ACapability, AIssuer, AWorkspace, ARevision, AIntent);
end;

function TObservedCompiler.Start(const ASource: TNyxText;
  const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
begin
  Result := Inner.Start(ASource, APort);
  SetLength(Owner.Operations, Length(Owner.Operations) + 1);
  Owner.Operations[High(Owner.Operations)] := Result;
end;

destructor TProviderTransport.Destroy;
begin
  Entered.Free;
  ReleaseReply.Free;
  FGuard.Free;
  inherited Destroy;
end;

function TProviderTransport.Requests: Integer;
begin
  Result := InterlockedCompareExchange(FRequests, 0, 0);
end;

function TProviderTransport.Completions: Integer;
begin
  Result := InterlockedCompareExchange(FCompletions, 0, 0);
end;

function TProviderTransport.Cancels: Integer;
begin
  Result := InterlockedCompareExchange(FCancels, 0, 0);
end;

function TProviderTransport.ExactRetries: Boolean;
begin
  FGuard.Acquire;
  try
    Result := FExactRetries;
  finally
    FGuard.Release;
  end;
end;

function TProviderTransport.Post(const ACapability, ABody: TNyxText;
  AMaximumReplyBytes, ARemainingMS: Integer): TNyxNativeSourceReply;
var
  LBody: TNyxDataValue;
  LReply: TNyxDataValue;
  LMode: TNyxText;
  LAttempt: Integer;
begin
  Result := Default(TNyxNativeSourceReply);
  LBody := TNyxDataValue.ParseJSON(ABody);
  LMode := LBody.Field('compile').Field('mode').AsText;
  LAttempt := 0;

  if LMode = 'request' then
  begin
    LAttempt := InterlockedIncrement(FRequests);
    FGuard.Acquire;
    try

      if FOriginalRequest = '' then
      begin
        FOriginalRequest := ABody;
      end
      else if LAttempt > 1 then
      begin
        FExactRetries := FExactRetries and (FOriginalRequest = ABody);
      end;
    finally
      FGuard.Release;
    end;
  end
  else if LMode = 'complete' then
  begin
    LAttempt := InterlockedIncrement(FCompletions);
  end
  else if LMode = 'cancel' then
  begin
    InterlockedIncrement(FCancels);
  end;

  if Scenario = tsRefused then
  begin
    Result.Status := 403;
    Result.Text := NyxObject([NyxField('error', NyxData('Owned qualification refusal'))]).ToJSON;
    Exit;
  end;

  if Scenario = tsUnconfirmedDeadline then
  begin
    { No admission receipt is visible. The real client must bound its retries
      and preserve work as unconfirmed rather than inventing remote refusal. }
    Exit;
  end;
  try
    LReply := Engine.EditorSourceExchange(ACapability, LBody);
    Result.Status := 200;
  except
    on LException: Exception do
    begin
      Result.Status := 409;
      Result.Text := NyxObject([NyxField('error', NyxData(
        UTF8Encode(UnicodeString(LException.Message))))]).ToJSON;
      Exit;
    end;
  end;

  if LMode = 'request' then
  begin
    FGuard.Acquire;
    try
      FJob := LReply.Field('job').AsText;
    finally
      FGuard.Release;
    end;
    Entered.SetEvent;

    if (Scenario in [tsCancelled, tsRetired]) and (LAttempt = 1) then
    begin

      if ReleaseReply.WaitFor(ARemainingMS) <> wrSignaled then
      begin
        raise Exception.Create('Owned held source reply exceeded its request budget');
      end;
    end;
  end;

  if (Scenario = tsLostReplies) and ((LMode = 'request') or (LMode = 'complete')) and
    (LAttempt = 1) then
  begin
    { Only after real admission/publication: a status-zero reply must recover
      the original job/receipt instead of allocating or committing another. }
    Result := Default(TNyxNativeSourceReply);
    Exit;
  end;

  if (Scenario = tsWrongAcknowledgement) and (LMode = 'complete') then
  begin
    LReply := NyxObject([
      NyxField('version', LReply.Field('version')),
      NyxField('state', LReply.Field('state')),
      NyxField('issuer', LReply.Field('issuer')),
      NyxField('workspace', LReply.Field('workspace')),
      NyxField('job', NyxData('wrong-acknowledgement-job')),
      NyxField('reference', LReply.Field('reference')),
      NyxField('revision', LReply.Field('revision'))]);
  end;

  if (Scenario in [tsMalformedAcknowledgement, tsServerErrorAcknowledgement]) and
    (LMode = 'complete') then
  begin
    { The real service has committed; an invalid success/error body must still
      require reconciliation. Deliberately do not replace its remote state. }
    Result.Text := '{not an acknowledgement';

    if Scenario = tsServerErrorAcknowledgement then
    begin
      Result.Status := 500;
    end;
    Exit;
  end;
  Result.Text := LReply.ToJSON;

  if NyxUTF8ByteCount(Result.Text) > AMaximumReplyBytes then
  begin
    raise Exception.Create('Owned engine transport reply exceeds the supplied byte budget');
  end;
end;

function TNativeProvider.CreateCompiler(const ACapability, AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer): INyxSharedSourceCompiler;
begin
  Capability := ACapability;
  Issuer := AIssuer;
  Workspace := AWorkspace;
  Revision := ARevision;
  Intent := NyxNull;
  Result := Self;
end;

function TNativeProvider.CreateVisualCompiler(const ACapability, AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const AIntent: TNyxDataValue): INyxSharedSourceCompiler;
begin
  Result := CreateCompiler(ACapability, AIssuer, AWorkspace, ARevision);
  Intent := AIntent.Copy;
end;

function TNativeProvider.Start(const ASource: TNyxText;
  const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
begin
  Source := ASource;
  if Intent.Kind = ndNull then
  begin
    Ticket := Engine.EditorCaptureSourcePublication(Capability, Workspace, Revision, Source);
  end
  else
  begin
    Ticket := Engine.EditorCaptureVisualPublication(Capability, Workspace, Revision, Intent, Source);
  end;
  Port := APort;
  StateValue := scsRunning;
  Inc(Starts);
  Result := Self;
end;

function TNativeProvider.GetState: TNyxSourceCompilationState;
begin
  Result := StateValue;
end;

procedure TNativeProvider.Cancel;
begin

  if StateValue in [scsPending, scsRunning] then
  begin
    StateValue := scsCancelled;
    Port := nil;
    Ticket := Default(TNyxStudioSourcePublication);
  end;
end;

procedure TNativeProvider.Publish(const ABuild: INyxSourceProjectionBuild);
var
  LState: TNyxDataValue;
begin
  LState := Engine.EditorCommitSourceProjection(Capability, Ticket,
    ABuild.Projection, Default(TNyxControlRef), Default(TNyxControlRef));
  Receipt := NyxSourcePublicationReceipt(Issuer, Workspace,
    NyxBuildJob('owned-native-qualification'), ABuild.Reference,
    LState.Field('session').Field('revision').AsInteger);
end;

procedure TNativeProvider.Deliver(const AProjection: INyxSourceProjection;
  AWrongReceipt: Boolean);
var
  LPort: INyxSharedSourceCompilationPort;
  LReceipt: TNyxSourcePublicationReceipt;
begin
  LPort := Port;
  Port := nil;
  StateValue := scsCompleted;
  LReceipt := Receipt;

  if AWrongReceipt then
  begin
    LReceipt := NyxSourcePublicationReceipt('different-owning-server', Workspace,
      Receipt.Job, Receipt.Reference, Receipt.Revision);
  end;

  if LPort <> nil then
  begin
    LPort.Complete(npoCommitted, AProjection, LReceipt);
  end;
end;

procedure TNativeProvider.Refuse;
var
  LPort: INyxSharedSourceCompilationPort;
begin
  LPort := Port;
  Port := nil;
  StateValue := scsFailed;
  LPort.Complete(npoRefused, nil, Default(TNyxSourcePublicationReceipt),
    'Explicit compiler refusal before publication');
end;

procedure TJourney.Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
begin

  if AState = nssApplied then
  begin
    Bridge.RecordLocal;
  end;
end;

procedure TJourney.Drain;
var
  LIndex: Integer;
begin
  for LIndex := 0 to 19 do
  begin
    CheckSynchronize(0);

    if Assigned(Exchange.Reply) then
    begin
      Exchange.Deliver;
    end;

    if Bridge.SourceSynchronized or Bridge.State.Conflict then
    begin
      Exit;
    end;

    if (Commands.State = nssPreparing) and ((Provider = nil) or
      ((Provider.Port <> nil) and (Provider.StateValue = scsRunning))) then
    begin
      { Dispatch ends this deterministic editor exchange drain. The original
        provider holds publication explicitly; the real asynchronous provider
        is awaited separately from its actual command/producer state. }
      Exit;
    end;
    Exchange.Fire;
  end;
  raise Exception.Create('Owned observing protocol failed to settle within bounded steps');
end;

destructor TJourney.Destroy;
begin

  if Commands <> nil then
  begin
    Commands.Detach;
  end;
  Commands.Free;
  Bridge.Free;
  Factory := nil;
  Provider := nil;
  Session.Free;
  Engine.Free;
  inherited Destroy;
end;

procedure TJourney.AwaitProvider;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    CheckSynchronize(1);

    if Assigned(Exchange.Reply) then
    begin
      Exchange.Deliver;
    end;

    if Provider.Port <> nil then
    begin
      Exit;
    end;

    if Commands.State in [nssFailed, nssRejected, nssStale] then
    begin
      raise Exception.Create('Visual provider refused: ' + Commands.Message);
    end;
    Exchange.Fire;
  until GetTickCount64 - LStarted > 5000;
  raise Exception.Create('Actual visual preparation did not reach its owned provider');
end;

procedure TJourney.AwaitCompletion;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    CheckSynchronize(1);

    if Commands.State <> nssPreparing then
    begin
      Exit;
    end;
  until GetTickCount64 - LStarted > 5000;
  raise Exception.Create('Shared compiler delivery did not reach its UI owner');
end;

function CompileOwnedSource(const ASource: TNyxText): INyxSourceProjectionBuild;
var
  LExecutor: TNyxBuildExecutor;
begin
  LExecutor := TNyxBuildExecutor.Create(GDirectories, GProfile);
  try
    Result := LExecutor.ProjectSource(ASource, NyxPascalUnit(NyxCompanionUnitName(ASource)),
      btNativeLCL, spcChecked);
    Check(Result.Projection.State = spsExecuted, 'actual customized native constructor executes');
  finally
    LExecutor.Free;
  end;
end;

procedure Run(AScenario: TScenario; ASecondary: Boolean = False);
var
  LJourney: TJourney;
  LBefore: TNyxProjectPair;
  LLater: TNyxText;
  LState: TNyxDataValue;
  LRemote: TNyxProjectPair;
  LWorkspace: TNyxWorkspaceRef;
  LRetainedHost: INyxSharedSourceHost;
  LStarted: QWord;
  LEdit: TNyxStudioDesignEdit;
  LVisualBuild: INyxSourceProjectionBuild;
begin
  LJourney := TJourney.Create;
  try
    LJourney.Engine := TNyxStudioMCP.Create(GDirectories
      .RunningIn(GDirectories.RuntimeRoot + 'scenario-' + IntToStr(Ord(AScenario)) + '-' + BoolToStr(ASecondary, True))
      .EnrollingProject(GDirectories.RuntimeRoot + 'scenario-' + IntToStr(Ord(AScenario)) + '-' + BoolToStr(ASecondary, True)),
      8762, 8763, GProfile);
    LWorkspace := NyxPrimaryWorkspace;

    if ASecondary then
    begin
      LWorkspace := NyxWorkspace(LJourney.Engine.InvokeTool('nyx_workspaces',
        'shared-commands-review', 'Scooty', NyxObject([
          NyxField('mode', NyxData('create')), NyxField('expectedRevision', NyxData(1)),
          NyxField('operationId', NyxData('second-shared-command-project')),
          NyxField('label', NyxData('Second source project')),
          NyxField('base', NyxData('empty'))])).Field('workspace').AsText);
    end;
    LJourney.Session := TNyxStudioSession.Create;
    LJourney.Exchange := TEngineExchange.Create;
    LJourney.Exchange.Engine := LJourney.Engine;
    LJourney.Bridge := TNyxStudioAgentBridge.Create(LJourney.Session, nil, LWorkspace, LJourney.Exchange);
    LJourney.Provider := TNativeProvider.Create;
    LJourney.Provider.Engine := LJourney.Engine;
    LJourney.Factory := LJourney.Provider;
    LJourney.Bridge.UseSharedSourceFactory(LJourney.Factory);
    LJourney.Commands := TNyxSourceCommands.Create(LJourney.Session, LJourney.Changed);
    LJourney.Commands.UseSharedCompiler(LJourney.Bridge.SharedSourceHost);
    LJourney.Bridge.Connect;
    LJourney.Drain;
    LBefore := LJourney.Session.ProjectSnapshot;
    LJourney.Session.SetSourceDraft(GSource);
    LJourney.Bridge.RecordDraft;
    LJourney.Commands.Apply;
    Check(LJourney.Provider.Starts = 0, 'Apply waits for draft acknowledgement before provider capture');
    LJourney.Drain;
    Check((LJourney.Provider.Starts = 1) and LJourney.Commands.Busy,
      'acknowledged draft dispatches through the ordinary command queue');
    Check((LJourney.Provider.Workspace.ID = LWorkspace.ID) and
      (LJourney.Provider.Source = GSource), 'provider captures exact project and source');
    { First construction compiles after its actual ticket was captured. Later
      scenarios reuse this immutable real result to isolate coordinator races. }

    if GBuild = nil then
    begin
      with TNyxBuildExecutor.Create(GDirectories, GProfile) do
      try
        GBuild := ProjectSource(GSource, NyxPascalUnit('nyx.projection.fixture'), btNativeLCL, spcChecked);
      finally
        Free;
      end;
      Check((GBuild.Projection.State = spsExecuted) and
        (GBuild.Projection.Design = ExpectedNyxProjectionDesign), 'real FPC helpers and loops execute');
    end;
    LLater := GSource + CLaterNotes;

    if AScenario = ssQueuedEdit then
    begin
      LEdit := Default(TNyxStudioDesignEdit);
      LEdit.Action := sdaTitle;
      { The Apply ahead of this command replaces the starter's home root. This
        edit intentionally addresses the resulting executed fixture's view. }
      LEdit.Selection := 'heading-1';
      LEdit.View := 'notebook-1';
      LEdit.Value := 'A queued title';
      LJourney.Commands.Edit(LEdit);
    end;

    if AScenario = ssReloaded then
    begin
      LJourney.Session.LoadProject(LBefore);
      LJourney.Bridge.RecordLocal;
    end;

    if AScenario = ssTypingDuringCompile then
    begin
      LJourney.Session.SetSourceDraft(LLater);
      LJourney.Bridge.RecordDraft;
      LJourney.Exchange.Fire;
      Check(not Assigned(LJourney.Exchange.Reply), 'typing persists locally without racing the reserved server');
    end;

    if AScenario = ssRefused then
    begin
      LJourney.Provider.Refuse;
    end
    else
    begin
      LJourney.Provider.Publish(GBuild);

      if AScenario in [ssCancelledAfterCommit, ssRetired] then
      begin
        LJourney.Commands.Cancel;
      end;

      if AScenario = ssRetired then
      begin
        LRetainedHost := LJourney.Bridge.SharedSourceHost;
        LJourney.Commands.Detach;
        FreeAndNil(LJourney.Bridge);
        LJourney.Exchange := nil;
        Check(not LRetainedHost.Ready and not LRetainedHost.Waiting,
          'retained shared host revokes its owner before bridge retirement');
        LJourney.Provider.Deliver(GBuild.Projection);
        CheckSynchronize(0);
        Check(LJourney.Session.Source = LBefore.Source, 'retired completion cannot publish locally');
        Exit;
      end;
      LJourney.Provider.Deliver(GBuild.Projection, AScenario = ssWrongReceipt);
    end;
    { Observe actual scheduler delivery; timeout is a failure, never success. }
    LStarted := GetTickCount64;
    repeat
      CheckSynchronize(1);

      if LJourney.Commands.State <> nssPreparing then
      begin
        Break;
      end;
    until GetTickCount64 - LStarted > 5000;

    if AScenario in [ssTypingDuringCompile, ssWrongReceipt, ssCancelledAfterCommit, ssReloaded] then
    begin
      Check(LJourney.Bridge.State.Conflict and (LJourney.Session.Source = LBefore.Source),
        'uncertain/stale/cancelled committed result preserves earlier local accepted files');

      if AScenario = ssTypingDuringCompile then
      begin
        Check(LJourney.Session.DraftSource = LLater, 'newer typing is retained exactly after remote admission');
      end;
      Exit;
    end;

    if AScenario = ssRefused then
    begin
      Check((LJourney.Commands.State = nssFailed) and not LJourney.Bridge.State.Conflict and
        (LJourney.Session.Source = LBefore.Source), 'explicit refusal retains files and releases reservation');
      LJourney.Drain;
      Exit;
    end;
    Check((LJourney.Commands.State = nssApplied) and LJourney.Commands.Busy and
      not LJourney.Bridge.SourceSynchronized, 'local admission holds command dispatch until observing acknowledgement');
    Check((LJourney.Session.Source = GSource) and LJourney.Session.CanUndo,
      'ordinary local completion owns one paired history entry');

    if AScenario = ssQueuedEdit then
    begin
      Check(LJourney.Session.Document.Title = 'Handwritten notebook',
        'queued title remains undispatched while the shared result awaits acknowledgement');
    end;

    if AScenario = ssTypingDuringAck then
    begin
      LJourney.Session.SetSourceDraft(LLater);
      LJourney.Bridge.RecordDraft;
    end;

    if AScenario = ssRemoteRace then
    begin
      LState := LJourney.Engine.EditorExchange(LJourney.Provider.Capability,
        NyxWithWorkspace(NyxObject([NyxField('op', NyxData('observe')),
          NyxField('after', NyxData(0))]), LWorkspace));
      LRemote := DecodeNyxProject(LState.Field('project').AsText);
      LRemote.Pending := True;
      LRemote.Draft := GSource + #10 + '{ Other session notes. }';
      LRemote.DraftBase := GSource;
      LJourney.Engine.EditorExchange(LJourney.Provider.Capability,
        NyxWithWorkspace(NyxObject([NyxField('op', NyxData('commit')),
          NyxField('expectedRevision', LState.Field('session').Field('revision')),
          NyxField('project', NyxData(EncodeNyxProject(LRemote))),
          NyxField('selection', LState.Field('session').Field('selection')),
          NyxField('view', LState.Field('session').Field('view'))]), LWorkspace));
    end;
    LJourney.Drain;

    if AScenario = ssQueuedEdit then
    begin
      LJourney.AwaitProvider;
      Check((LJourney.Provider.Starts = 2) and LJourney.Provider.Ticket.IsVisual,
        'acknowledgement dispatches the queued visual edit with independent server preparation');
      LVisualBuild := CompileOwnedSource(LJourney.Provider.Source);
      LJourney.Provider.Publish(LVisualBuild);
      LJourney.Provider.Deliver(LVisualBuild.Projection);
      LJourney.AwaitCompletion;
      Check((LJourney.Commands.State = nssApplied) and LJourney.Commands.Busy,
        'verified visual publication also holds its observing acknowledgement');
      LJourney.Drain;
      Check(LJourney.Bridge.SourceSynchronized and
        (LJourney.Session.Document.Title = 'A queued title'),
        'shared visual continuation publishes typed title meaning');
      LJourney.Session.Undo;
      Check(LJourney.Session.Source = GSource, 'visual Undo preserves the original handwritten builder');
      LJourney.Session.Undo;
      Check(LJourney.Session.Source = LBefore.Source, 'Apply and visual edits own separate paired steps');
      Exit;
    end;

    if AScenario = ssRemoteRace then
    begin
      Check(LJourney.Bridge.State.Conflict and (LJourney.Session.Source = GSource),
        'later server revision refuses acknowledgement without replacing local accepted pair');
      Exit;
    end;
    Check(LJourney.Bridge.SourceSynchronized and not LJourney.Commands.Busy,
      'exact observing acknowledgement releases the ordinary command queue');

    if AScenario = ssTypingDuringAck then
    begin
      Check(LJourney.Session.SourceDraftPending and (LJourney.Session.DraftSource = LLater),
        'acknowledgement preserves and subsequently shares exact newer draft');
    end;
    LJourney.Session.Undo;
    Check(LJourney.Session.Source = LBefore.Source, 'one local Undo restores earlier full source');
    LJourney.Session.Redo;
    Check(LJourney.Session.Source = GSource, 'paired Redo retains complete executed source');
  finally
    LJourney.Free;
  end;
end;

function NewVisualJourney(AScenario: TVisualScenario): TJourney;
begin
  Result := TJourney.Create;
  try
    Result.Engine := TNyxStudioMCP.Create(GDirectories
      .RunningIn(GDirectories.RuntimeRoot + 'visual-' + IntToStr(Ord(AScenario)))
      .EnrollingProject(GDirectories.RuntimeRoot + 'visual-' + IntToStr(Ord(AScenario))),
      8762, 8763, GProfile);
    Result.Session := TNyxStudioSession.Create;
    Result.Exchange := TEngineExchange.Create;
    Result.Exchange.Engine := Result.Engine;
    Result.Bridge := TNyxStudioAgentBridge.Create(Result.Session, nil,
      NyxPrimaryWorkspace, Result.Exchange);
    Result.Provider := TNativeProvider.Create;
    Result.Provider.Engine := Result.Engine;
    Result.Factory := Result.Provider;
    Result.Bridge.UseSharedSourceFactory(Result.Factory);
    Result.Commands := TNyxSourceCommands.Create(Result.Session, Result.Changed);
    Result.Commands.UseSharedCompiler(Result.Bridge.SharedSourceHost);
    Result.Bridge.Connect;
    Result.Drain;
    Result.Session.SetSourceDraft(GSource);
    Result.Bridge.RecordDraft;
    Result.Commands.Apply;
    Result.Drain;
    Result.Provider.Publish(GBuild);
    Result.Provider.Deliver(GBuild.Projection);
    Result.AwaitCompletion;
    Result.Drain;
    Check(Result.Bridge.SourceSynchronized, 'visual journey begins with an acknowledged actual executed pair');
  except
    Result.Free;
    raise;
  end;
end;

function ObserveJourney(AJourney: TJourney): TNyxDataValue;
begin
  Result := AJourney.Engine.EditorExchange(AJourney.Exchange.Token,
    NyxObject([NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
end;

procedure RunVisual(AScenario: TVisualScenario);
const
  CVisualText: TNyxText = 'A carefully crafted heading 🚀 𐐷';
var
  LJourney: TJourney;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LRemote: TNyxProjectPair;
  LEdit: TNyxStudioDesignEdit;
  LBuild: INyxSourceProjectionBuild;
  LState: TNyxDataValue;
  LTicket: TNyxStudioSourcePublication;
  LRefused: Boolean;
  LHost: INyxSharedSourceHost;
  LLater: TNyxText;
  LCheckpoint: TNyxText;
  LCheckpointBefore: TNyxBytes;
  LLock: TFileStream;
begin
  LJourney := NewVisualJourney(AScenario);
  try
    { This buffer is unfinished independent user work. Visual changes must keep
      its exact text and original accepted base on both sides and in history. }
    LJourney.Session.SetSourceDraft(GSource + CLaterNotes);
    LJourney.Bridge.RecordDraft;
    LJourney.Drain;
    LBefore := LJourney.Session.ProjectSnapshot;
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaProperty;
    LEdit.Selection := 'heading-1';
    LEdit.View := 'notebook-1';
    LEdit.Name := 'text';
    LEdit.Value := CVisualText;
    LJourney.Commands.Edit(LEdit);

    if AScenario = vsOrdered then
    begin
      LEdit.Name := 'padding';
      LEdit.Value := '27';
      LJourney.Commands.Edit(LEdit);
    end;
    LJourney.AwaitProvider;
    Check((LJourney.Provider.Starts = 2) and LJourney.Provider.Ticket.IsVisual and
      (LJourney.Provider.Intent.Count = 2), 'shared visual dispatch carries a bounded intent and sealed server proposal');
    Check(EncodeNyxProject(DecodeNyxProject(ObserveJourney(LJourney).Field('project').AsText)) =
      EncodeNyxProject(LBefore), 'precompile capture does not replace accepted source or pending draft');
    Check((Pos('TNotebookCards.Caption', LJourney.Provider.Source) > 0) and
      (Pos('for LIndex := 1 to 2 do', LJourney.Provider.Source) > 0) and
      (Pos('INyxHeading', LJourney.Provider.Source) > 0),
      'shared proposal retains handwritten helpers, loops and specialized managed controls');

    if AScenario = vsOrdered then
    begin
      LRefused := False;
      try
        LTicket := LJourney.Engine.EditorCaptureVisualPublication(
          LJourney.Provider.Capability, NyxPrimaryWorkspace, LJourney.Provider.Revision,
          LJourney.Provider.Intent, LJourney.Provider.Source + ' ');
      except
        on ENyxProjectConflict do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and not LTicket.IsCaptured,
        'server refuses source differing from its independently reconstructed intent');
      LRefused := False;
      try
        LTicket := LJourney.Engine.EditorCaptureVisualPublication(
          LJourney.Provider.Capability, NyxPrimaryWorkspace, LJourney.Provider.Revision,
          NyxObject([NyxField('version', LJourney.Provider.Intent.Field('version')),
            NyxField('edit', LJourney.Provider.Intent.Field('edit')),
            NyxField('origin', NyxData('executed'))]), LJourney.Provider.Source);
      except
        on Exception do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and not LTicket.IsCaptured,
        'semantic intent refuses client execution flags and additional authority fields');
      LRefused := False;
      try
        LJourney.Provider.Publish(GBuild);
      except
        on Exception do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (EncodeNyxProject(DecodeNyxProject(
        ObserveJourney(LJourney).Field('project').AsText)) = EncodeNyxProject(LBefore)),
        'actual mismatched compiled producer refuses before any backend publication');
    end;

    if AScenario = vsRefused then
    begin
      LJourney.Provider.Refuse;
      LJourney.AwaitCompletion;
      LJourney.Drain;
      Check((LJourney.Commands.State = nssFailed) and not LJourney.Bridge.State.Conflict and
        (EncodeNyxProject(LJourney.Session.ProjectSnapshot) = EncodeNyxProject(LBefore)),
        'explicit compiler refusal keeps the entire independent local pair and releases reservation');
      Exit;
    end;
    LBuild := CompileOwnedSource(LJourney.Provider.Source);

    if AScenario = vsBackendRace then
    begin
      LState := ObserveJourney(LJourney);
      LRemote := DecodeNyxProject(LState.Field('project').AsText);
      LRemote.Draft := LRemote.Draft + #10 + '{ Server-side notes. }';
      LJourney.Engine.EditorExchange(LJourney.Provider.Capability, NyxObject([
        NyxField('op', NyxData('commit')), NyxField('expectedRevision', LState.Field('session').Field('revision')),
        NyxField('project', NyxData(EncodeNyxProject(LRemote))),
        NyxField('selection', LState.Field('session').Field('selection')),
        NyxField('view', LState.Field('session').Field('view'))]));
      LRefused := False;
      try
        LJourney.Provider.Publish(LBuild);
      except
        on Exception do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (EncodeNyxProject(DecodeNyxProject(
        ObserveJourney(LJourney).Field('project').AsText)) = EncodeNyxProject(LRemote)),
        'changed backend revision refuses completion and retains its exact newer draft');
      Check(EncodeNyxProject(LJourney.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
        'backend refusal cannot mutate the local accepted pair');
      Exit;
    end;
    LLater := LBefore.Draft + #10 + '{ More local notes. }';

    if AScenario = vsTypingDuringCompile then
    begin
      LJourney.Session.SetSourceDraft(LLater);
      LJourney.Bridge.RecordDraft;
    end;

    if AScenario = vsOrdered then
    begin
      LCheckpoint := GDirectories.RunningIn(GDirectories.RuntimeRoot +
        'visual-' + IntToStr(Ord(AScenario))).SessionCheckpoint;
      LCheckpointBefore := ReadBytes(LCheckpoint);
      LLock := TFileStream.Create(LCheckpoint, fmOpenRead or fmShareExclusive);
      LRefused := False;
      try
        try
          LJourney.Provider.Publish(LBuild);
        except
          on Exception do
          begin
            LRefused := True;
          end;
        end;
      finally
        LLock.Free;
      end;
      Check(LRefused and SameBytes(ReadBytes(LCheckpoint), LCheckpointBefore) and
        (EncodeNyxProject(DecodeNyxProject(ObserveJourney(LJourney).Field('project').AsText)) =
          EncodeNyxProject(LBefore)),
        'failed durable visual publication rolls back the full backend pair and exact checkpoint bytes');
    end;
    LJourney.Provider.Publish(LBuild);

    if AScenario in [vsCancelledAfterCommit, vsRetired] then
    begin
      LJourney.Commands.Cancel;
    end;

    if AScenario = vsRetired then
    begin
      LHost := LJourney.Bridge.SharedSourceHost;
      LJourney.Commands.Detach;
      FreeAndNil(LJourney.Bridge);
      LJourney.Exchange := nil;
      LJourney.Provider.Deliver(LBuild.Projection);
      CheckSynchronize(0);
      Check(not LHost.Ready and not LHost.Waiting and
        (EncodeNyxProject(LJourney.Session.ProjectSnapshot) = EncodeNyxProject(LBefore)),
        'retired shared visual courier revokes its owner and preserves the complete local pair');
      Exit;
    end;
    LJourney.Provider.Deliver(LBuild.Projection, AScenario = vsWrongReceipt);
    LJourney.AwaitCompletion;

    if AScenario in [vsTypingDuringCompile, vsWrongReceipt, vsCancelledAfterCommit] then
    begin
      Check(LJourney.Bridge.State.Conflict and (LJourney.Session.Source = LBefore.Source),
        'stale, foreign or cancelled visual completion keeps local accepted source for reconciliation');

      if AScenario = vsTypingDuringCompile then
      begin
        Check(LJourney.Session.DraftSource = LLater, 'newer typing survives stale visual completion exactly');
      end;
      Exit;
    end;
    Check((LJourney.Commands.State = nssApplied) and LJourney.Commands.Busy and
      not LJourney.Bridge.SourceSynchronized, 'visual admission holds FIFO until exact observing acknowledgement');
    Check((LJourney.Session.Document.Find('heading-1').Prop('text') = CVisualText) and
      (LJourney.Session.ProjectSnapshot.Draft = LBefore.Draft) and
      (LJourney.Session.ProjectSnapshot.DraftBase = LBefore.DraftBase),
      'actual compiler-backed visual admission retains exact Unicode and independent draft/base');
    LAfter := LJourney.Session.ProjectSnapshot;

    if AScenario = vsTypingDuringAck then
    begin
      LJourney.Session.SetSourceDraft(LLater);
      LJourney.Bridge.RecordDraft;
    end;
    LJourney.Drain;

    if AScenario = vsOrdered then
    begin
      LJourney.AwaitProvider;
      Check((LJourney.Provider.Starts = 3) and
        (LJourney.Provider.Ticket.Baseline.Source = LAfter.Source),
        'second visual command captures the freshly acknowledged first publication');
      LBuild := CompileOwnedSource(LJourney.Provider.Source);
      LJourney.Provider.Publish(LBuild);
      LJourney.Provider.Deliver(LBuild.Projection);
      LJourney.AwaitCompletion;
      LJourney.Drain;
      Check(LJourney.Session.Document.Find('heading-1').Prop('padding') = '27',
        'ordered shared visual commands reproduce both typed property changes');
      LJourney.Session.Undo;
      Check(EncodeNyxProject(LJourney.Session.ProjectSnapshot) = EncodeNyxProject(LAfter),
        'one local paired Undo restores the first visual publication with its exact draft/base');
      LJourney.Session.Undo;
      Check(EncodeNyxProject(LJourney.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
        'second local paired Undo restores the complete pre-visual pair');
      LJourney.Session.Redo;
      LJourney.Session.Redo;
      LAfter := LJourney.Session.ProjectSnapshot;
      LState := LJourney.Engine.InvokeTool('nyx_session', 'visual-review', 'Scooty', NyxObject([]));
      LJourney.Engine.InvokeTool('nyx_history', 'visual-review', 'Scooty', NyxObject([
        NyxField('expectedRevision', LState.Field('revision')),
        NyxField('operationId', NyxData('visual-undo-one')), NyxField('direction', NyxData('undo'))]));
      LState := LJourney.Engine.InvokeTool('nyx_session', 'visual-review', 'Scooty', NyxObject([]));
      LJourney.Engine.InvokeTool('nyx_history', 'visual-review', 'Scooty', NyxObject([
        NyxField('expectedRevision', LState.Field('revision')),
        NyxField('operationId', NyxData('visual-undo-two')), NyxField('direction', NyxData('undo'))]));
      Check(EncodeNyxProject(DecodeNyxProject(ObserveJourney(LJourney).Field('project').AsText)) =
        EncodeNyxProject(LBefore), 'ordinary semantic backend history restores the exact pre-visual source and draft');
      LState := LJourney.Engine.InvokeTool('nyx_session', 'visual-review', 'Scooty', NyxObject([]));
      LJourney.Engine.InvokeTool('nyx_history', 'visual-review', 'Scooty', NyxObject([
        NyxField('expectedRevision', LState.Field('revision')),
        NyxField('operationId', NyxData('visual-redo-one')), NyxField('direction', NyxData('redo'))]));
      LState := LJourney.Engine.InvokeTool('nyx_session', 'visual-review', 'Scooty', NyxObject([]));
      LJourney.Engine.InvokeTool('nyx_history', 'visual-review', 'Scooty', NyxObject([
        NyxField('expectedRevision', LState.Field('revision')),
        NyxField('operationId', NyxData('visual-redo-two')), NyxField('direction', NyxData('redo'))]));
      Check(EncodeNyxProject(DecodeNyxProject(ObserveJourney(LJourney).Field('project').AsText)) =
        EncodeNyxProject(LAfter), 'backend paired Redo reproduces both compiled visual publications');
      Exit;
    end;
    Check(LJourney.Bridge.SourceSynchronized and not LJourney.Commands.Busy,
      'exact visual observing acknowledgement releases the ordinary queue');
    Check((LJourney.Session.DraftSource = LLater) and
      (DecodeNyxProject(ObserveJourney(LJourney).Field('project').AsText).Draft = LLater),
      'typing during visual acknowledgement remains exact and is subsequently shared');
  finally
    LJourney.Free;
  end;
end;

{ Exercise the production native provider through the ordinary queue/observing
  bridge. HTTP alone is replaced by the bounded engine transport above; FPC,
  source preparation, publication, paired history and acknowledgement are real. }
procedure RunNativeProvider(AScenario: TTransportScenario);
var
  LJourney: TJourney;
  LTransport: TProviderTransport;
  LTransportLease: INyxNativeSourceTransport;
  LBefore: TNyxProjectPair;
  LRemote: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LFirstVisual: TNyxProjectPair;
  LEdit: TNyxStudioDesignEdit;
  LStarted: QWord;
  LWaiting: Boolean;
  LObserved: TObservedFactory;
  LOptions: TNyxNativeSourceServiceOptions;

  procedure AwaitDispatch(AOperationCount: Integer);
  var
    LStart: QWord;
  begin
    LStart := GetTickCount64;
    repeat
      CheckSynchronize(1);

      if Length(LObserved.Operations) >= AOperationCount then
      begin
        Exit;
      end;

      if LJourney.Commands.State in [nssFailed, nssRejected, nssStale] then
      begin
        raise Exception.Create('Real native provider dispatch refused: ' + LJourney.Commands.Message);
      end;
      { Dispatch requires exact draft/frame acknowledgement. Keep delivering
        that ordinary protocol until the REAL Start token exists; only then
        hold further observing replies to qualify the publication barrier. }

      if Assigned(LJourney.Exchange.Reply) then
      begin
        LJourney.Exchange.Deliver;
      end
      else
      begin
        LJourney.Exchange.Fire;
      end;
      Sleep(1);
    until GetTickCount64 - LStart > 10000;
    raise Exception.Create('Real native provider did not receive acknowledged dispatch: ' +
      LJourney.Commands.Message);
  end;

  procedure AwaitCommand;
  var
    LStart: QWord;
  begin
    LStart := GetTickCount64;
    repeat
      CheckSynchronize(1);

      if LJourney.Commands.State <> nssPreparing then
      begin
        Exit;
      end;
      Sleep(1);
    until GetTickCount64 - LStart > 60000;
    raise Exception.Create('Real native provider command did not settle: ' + LJourney.Commands.Message);
  end;

  procedure Acknowledge;
  var
    LStart: QWord;
  begin
    LStart := GetTickCount64;
    repeat
      CheckSynchronize(1);

      if Assigned(LJourney.Exchange.Reply) then
      begin
        LJourney.Exchange.Deliver;
      end
      else
      begin
        LJourney.Exchange.Fire;
      end;

      if not LJourney.Commands.Busy and LJourney.Bridge.SourceSynchronized then
      begin
        Exit;
      end;
      Sleep(1);
    until GetTickCount64 - LStart > 60000;
    raise Exception.Create('Real provider observing acknowledgement failed: ' + LJourney.Bridge.State.Status);
  end;

begin
  LJourney := TJourney.Create;
  LTransport := TProviderTransport.Create;
  LTransportLease := LTransport;
  try
    LJourney.Engine := TNyxStudioMCP.Create(GDirectories
      .RunningIn(GDirectories.RuntimeRoot + 'native-provider-' + IntToStr(Ord(AScenario)))
      .EnrollingProject(GDirectories.RuntimeRoot + 'native-provider-' + IntToStr(Ord(AScenario))),
      8762, 8763, GProfile);
    LTransport.Engine := LJourney.Engine;
    LTransport.Scenario := AScenario;
    LObserved := TObservedFactory.Create;
    LJourney.Factory := LObserved;
    LOptions := TNyxNativeSourceServiceOptions.Defaults.Polling(10)
      .Requests(NewNyxTransportPolicy.WholeRequest(10000)).WholeOperation(60000);

    if AScenario = tsUnconfirmedDeadline then
    begin
      LOptions := LOptions.WholeOperation(150);
    end;
    LObserved.Inner := NewNyxSharedNativeSourceCompilerFactory(LTransportLease, LOptions);
    LJourney.Session := TNyxStudioSession.Create;
    LJourney.Exchange := TEngineExchange.Create;
    LJourney.Exchange.Engine := LJourney.Engine;
    LJourney.Bridge := TNyxStudioAgentBridge.Create(LJourney.Session, nil,
      NyxPrimaryWorkspace, LJourney.Exchange);
    LJourney.Bridge.UseSharedSourceFactory(LJourney.Factory);
    LJourney.Commands := TNyxSourceCommands.Create(LJourney.Session, LJourney.Changed);
    LJourney.Commands.UseSharedCompiler(LJourney.Bridge.SharedSourceHost);
    LJourney.Bridge.Connect;
    LJourney.Drain;
    LBefore := LJourney.Session.ProjectSnapshot;
    LJourney.Session.SetSourceDraft(GSource);
    LJourney.Bridge.RecordDraft;
    LJourney.Commands.Apply;
    Check(LTransport.Requests = 0, 'real native provider waits for exact draft acknowledgement');
    LJourney.Drain;
    AwaitDispatch(1);

    if AScenario in [tsCancelled, tsRetired] then
    begin
      LStarted := GetTickCount64;
      repeat
        CheckSynchronize(1);
        LWaiting := LTransport.Entered.WaitFor(0) = wrSignaled;
        Sleep(1);
      until LWaiting or (GetTickCount64 - LStarted > 10000);
      Check(LWaiting and LJourney.Commands.Busy,
        'real native operation retains its request while server admission already occurred');
      LJourney.Commands.Cancel;

      if AScenario = tsRetired then
      begin
        LJourney.Commands.Detach;
      end;
      LTransport.ReleaseReply.SetEvent;
      LStarted := GetTickCount64;
      repeat
        CheckSynchronize(1);
        Sleep(1);
      until LObserved.Terminal or (GetTickCount64 - LStarted > 60000);
      Check(LObserved.Terminal and (LTransport.Cancels > 0) and
        (LTransport.Completions = 0),
        'cancelled/retired real provider recovers the captured job and observes retirement before terminal');
      Check((LJourney.Session.Source = LBefore.Source) and
        (LJourney.Session.Save = LBefore.Design) and (LJourney.Session.DraftSource = GSource),
        'cancelled/retired provider preserves accepted source/design and exact unfinished input');
      LRemote := DecodeNyxProject(ObserveJourney(LJourney).Field('project').AsText);
      Check((LRemote.Source = LBefore.Source) and (LRemote.Design = LBefore.Design) and LRemote.Pending,
        'remote cancellation cannot publish the constructor or consume its captured draft');
      Exit;
    end;
    AwaitCommand;

    if AScenario = tsRefused then
    begin
      Check((LJourney.Commands.State = nssFailed) and
        (LJourney.Session.Source = LBefore.Source) and (LJourney.Session.DraftSource = GSource),
        'definite native transport refusal retains accepted source and exact draft');
      Check(LTransport.Completions = 0, 'refused request has no producer publication attempt');
      Exit;
    end;

    if AScenario = tsUnconfirmedDeadline then
    begin
      Check((LJourney.Commands.State = nssFailed) and LObserved.Terminal and
        (LTransport.Requests > 1) and (LTransport.Completions = 0),
        'real native whole-operation deadline bounds exact admission retries');
      Check((LJourney.Session.Source = LBefore.Source) and
        (LJourney.Session.DraftSource = GSource) and LJourney.Bridge.State.Conflict and
        (Pos('Shared source needs reconciliation:', LJourney.Bridge.State.Status) = 1),
        'unknown admission at deadline preserves work and reports uncertainty');
      Exit;
    end;

    if AScenario in [tsWrongAcknowledgement, tsMalformedAcknowledgement,
      tsServerErrorAcknowledgement] then
    begin
      LRemote := DecodeNyxProject(ObserveJourney(LJourney).Field('project').AsText);
      Check((LTransport.Completions = 1) and (LRemote.Source = GSource) and
        (LRemote.Design = ExpectedNyxProjectionDesign),
        'wrong/malformed acknowledgement can follow a real committed server result');
      Check((LJourney.Session.Source = LBefore.Source) and
        (LJourney.Session.DraftSource = GSource) and not LJourney.Bridge.SourceSynchronized and
        LJourney.Bridge.State.Conflict and
        (Pos('Shared source needs reconciliation:', LJourney.Bridge.State.Status) = 1),
        'invalid acknowledgement does not enter local history and explicitly requires reconciliation');
      Exit;
    end;
    Check((LJourney.Commands.State = nssApplied) and
      (LJourney.Session.Source = GSource) and
      (LJourney.Session.Save = ExpectedNyxProjectionDesign),
      'real native provider compiles complete helpers/loops and admits the exact paired result');
    Check(LJourney.Commands.Busy and not LJourney.Bridge.SourceSynchronized,
      'real provider holds the ordinary queue until exact observing acknowledgement');

    if AScenario = tsLostReplies then
    begin
      Check((LTransport.Requests = 2) and (LTransport.Completions = 2) and LTransport.ExactRetries,
        'lost admission and committed acknowledgement recover exactly the original native job/receipt');
    end;
    Acknowledge;
    Check(LJourney.Bridge.SourceSynchronized and not LJourney.Commands.Busy,
      'exact native observing acknowledgement releases the ordinary queue');
    Check(DecodeNyxProject(ObserveJourney(LJourney).Field('project').AsText).Source = GSource,
      'real native provider source agrees with its owning server');

    if AScenario <> tsOrdinary then
    begin
      Exit;
    end;
    LJourney.Session.SetSourceDraft('An unfinished notebook idea 🚀');
    LJourney.Bridge.RecordDraft;
    LJourney.Drain;
    LBefore := LJourney.Session.ProjectSnapshot;
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaTitle;
    LEdit.Selection := 'heading-1';
    LEdit.View := 'notebook-1';
    LEdit.Value := 'A shared native notebook';
    LJourney.Commands.Edit(LEdit);
    AwaitDispatch(2);
    AwaitCommand;
    Check((LJourney.Commands.State = nssApplied) and
      (LJourney.Session.Document.Title = LEdit.Value) and
      (LJourney.Session.DraftSource = LBefore.Draft),
      'real native visual continuation changes typed title and retains invalid unfinished Pascal exactly');
    LFirstVisual := LJourney.Session.ProjectSnapshot;
    Acknowledge;
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaProperty;
    LEdit.Selection := 'heading-1';
    LEdit.View := 'notebook-1';
    LEdit.Name := 'text';
    LEdit.Value := 'A brighter native notebook';
    LEdit.Platform := npfAny;
    LJourney.Commands.Edit(LEdit);
    AwaitDispatch(3);
    AwaitCommand;
    Acknowledge;
    LAfter := LJourney.Session.ProjectSnapshot;
    Check((LJourney.Commands.State = nssApplied) and
      (LJourney.Session.Document.Find('heading-1').Prop('text') = LEdit.Value) and
      (LAfter.Draft = LBefore.Draft) and (LAfter.DraftBase = LBefore.DraftBase),
      'ordered real native visual publications retain handwritten helpers and independent draft/base');
    LJourney.Session.Undo;
    Check(EncodeNyxProject(LJourney.Session.ProjectSnapshot) = EncodeNyxProject(LFirstVisual),
      'one local paired Undo restores the first real visual result exactly');
    LJourney.Session.Undo;
    Check(EncodeNyxProject(LJourney.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'second paired Undo restores the full pre-visual source/design/draft');
    LJourney.Session.Redo;
    LJourney.Session.Redo;
    Check(EncodeNyxProject(LJourney.Session.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'paired Redo reproduces both real native visual results');
  finally
    LTransport.ReleaseReply.SetEvent;
    LJourney.Free;
    LTransportLease := nil;
  end;
end;

var
  LTools: TNyxDataValue;
  LProfile: TNyxOutputConfiguration;
  LScenario: TScenario;
  LVisualScenario: TVisualScenario;
  LTransportScenario: TTransportScenario;
begin
  LProfile := nil;
  try

    if (ParamCount <> 3) or DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
    begin
      raise Exception.Create('Supply repository, toolchain JSON and a NEW owned runtime');
    end;
    GDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
      .RunningIn(ParamStr(3)).EnrollingProject(ParamStr(3));
    LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
    GSource := ReadText(GDirectories.SourceRoot + 'tests/fixtures/nyx.projection.fixture.pas');
    LProfile := TNyxOutputConfiguration.Create;
    LProfile.SetField('fpc', LTools.Field('FPC').AsText);
    LProfile.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LProfile.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    GProfile := LProfile.Encode;
    for LScenario := Low(TScenario) to High(TScenario) do
    begin
      Run(LScenario);
    end;
    Run(ssNormal, True);
    for LVisualScenario := Low(TVisualScenario) to High(TVisualScenario) do
    begin
      RunVisual(LVisualScenario);
    end;
    for LTransportScenario := Low(TTransportScenario) to High(TTransportScenario) do
    begin
      WriteLn('Qualifying real native provider scenario ', Ord(LTransportScenario));
      Flush(Output);
      RunNativeProvider(LTransportScenario);
    end;
    GBuild := nil;
    FreeAndNil(LProfile);
    WriteLn('PASS ', GChecks, ' actual native shared source queue/bridge coordination checks');
  except
    on LException: Exception do
    begin
      GBuild := nil;
      LProfile.Free;
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
