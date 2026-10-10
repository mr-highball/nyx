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

uses Interfaces, Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.types,
  nyx.source,
  nyx.studio.projects, nyx.studio.session, nyx.studio.sourcejobs,
  nyx.studio.agentbridge, nyx.studio.exchange, nyx.studio.mcp,
  nyx.studio.directories, nyx.studio.outputs, nyx.studio.buildexecutor,
  nyx.studio.builds, nyx.studio.editorbuild, nyx.studio.workspaces,
  nyx.studio.sourceprojection, nyx.studio.sourcepublications,
  nyx.studio.sourcecompilation, nyx.studio.sourcecompilation.shared,
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
    Starts: Integer;
    function CreateCompiler(const ACapability, AIssuer: TNyxText;
      const AWorkspace: TNyxWorkspaceRef; ARevision: Integer): INyxSharedSourceCompiler;
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

function ReadText(const APath: TNyxText): TNyxText;
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if (LFile.Size < 1) or (LFile.Size > 4 * 1024 * 1024) then
    begin
      raise Exception.Create('Fixture input must be bounded');
    end;
    SetLength(LBytes, LFile.Size);
    LFile.ReadBuffer(LBytes[0], Length(LBytes));
    Result := NyxDecodeUTF8(LBytes);
  finally
    LFile.Free;
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

function TNativeProvider.CreateCompiler(const ACapability, AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer): INyxSharedSourceCompiler;
begin
  Capability := ACapability;
  Issuer := AIssuer;
  Workspace := AWorkspace;
  Revision := ARevision;
  Result := Self;
end;

function TNativeProvider.Start(const ASource: TNyxText;
  const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
begin
  Source := ASource;
  Ticket := Engine.EditorCaptureSourcePublication(Capability, Workspace, Revision, Source);
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

    if (Provider.Port <> nil) and (Provider.StateValue = scsRunning) and
      (Commands.State = nssPreparing) then
    begin
      { Dispatch is a distinct terminal point for this deterministic exchange
        drain; publication remains explicitly held by the provider below. }
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
      LEdit.Selection := LJourney.Session.SelectedID;
      LEdit.View := LJourney.Session.ActiveViewID;
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
      LStarted := GetTickCount64;
      repeat
        CheckSynchronize(1);
      until not LJourney.Commands.Busy or (GetTickCount64 - LStarted > 5000);
      Check(not LJourney.Commands.Busy and
        (LJourney.Commands.State in [nssFailed, nssRejected]),
        'acknowledgement dispatches the queued edit through the existing executed-source refusal');
      Check(LJourney.Session.Source = GSource,
        'unsupported expression rewriting still preserves the complete accepted constructor');
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

var
  LTools: TNyxDataValue;
  LProfile: TNyxOutputConfiguration;
  LScenario: TScenario;
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
