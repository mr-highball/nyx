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

unit nyx.studio.agentbridge;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.studio.session, nyx.studio.exchange,
  nyx.studio.projects, nyx.studio.agents, nyx.studio.agentview, nyx.studio.workspaces,
  nyx.studio.editorbuild, nyx.studio.outputs, nyx.model, nyx.types, nyx.studio.buildview,
  nyx.studio.builds, nyx.resources, nyx.source, nyx.studio.sourceobservations,
  nyx.studio.sourcecompilation, nyx.studio.sourcecompilation.shared,
  nyx.studio.sourcepublications, nyx.source.preparation;

type
  TNyxAgentRefresh = procedure(AContentChanged: Boolean) of object;
  { A persistence consumer receives the exact encoded paired project captured
    for sharing. The text is owned, not a borrowed model. Notifications run on
    the UI thread and must not destroy the bridge or mutate its session inline.
    A consumer handles its own storage failures; transport admission is separate. }
  TNyxProjectCaptured = procedure(const AProject: TNyxText) of object;
  { Closed editor history choice; strings exist only in the exchange packet. }
  TNyxEditorHistory = (nehUndo, nehRedo);

  { Shared controller for the private editor exchange. Borrows its ordinary
    Studio session; server owns authoritative paired history. Local publications
    queue in order, each becoming one server history command. A revision race
    freezes synchronization and retains local pair/draft/queue for explicit
    operator resolution. Observing requests are small until revision changes. }
  TNyxStudioAgentBridge = class
  private
    FSession: TNyxStudioSession;
    FView: TNyxStudioAgentView;
    FExchange: TNyxStudioEditorExchange;
    FRequest: Boolean;
    FToken: TNyxText;
    { Captured only on an owning claim, not from arbitrary later metadata. Empty
      means an older peer; negotiated replies may not silently lose this frame. }
    FSourceObservationIssuer: TNyxText;
    FKnownFrame: TNyxText;
    FSentFrame: TNyxText;
    FSent: TNyxDataValue;
    FQueue: array of TNyxDataValue;
    FQueueSizes: array of Integer;
    FQueueUnits: Integer;
    FTimer: Boolean;
    FApplying: Boolean;
    FEnabled: Boolean;
    FAcceptRemote: Boolean;
    FActivitySerial: Integer;
    FInitialProject: TNyxText;
    FProtectLocal: Boolean;
    FWarning: TNyxText;
    FOnRefresh: TNyxAgentRefresh;
    FWorkspace: TNyxWorkspaceRef;
    FWorkspaceMetadata: TNyxText;
    FDraftCapturePending: Boolean;
    FOnProjectCaptured: TNyxProjectCaptured;
    FResourceReference: TNyxResourceRef;
    FResourceLocale: TNyxLocaleRef;
    FResourceInspection: Boolean;
    { Shared Apply holds document synchronization until exact observing ack.
      These owned strings/ports never retain a document tree or renderer. }
    FSharedFactory: INyxSharedSourceCompilerFactory;
    FSharedPort: INyxSharedSourceHost;
    FSharedDispatch: INyxSharedSourceDispatch;
    FSharedAvailable: Boolean;
    FSharedReserved: Boolean;
    FSharedAcknowledging: Boolean;
    FSharedDirty: Boolean;
    FSharedRevision: Integer;
    FSharedSource: TNyxText;
    FSharedAccepted: TNyxProjectPair;
    function StartSharedSource(const ASource: TNyxText;
      const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
    function AdmitSharedSource(const ARequest: TNyxStudioSourceRequest;
      const APrepared: INyxPreparedSource;
      const AReceipt: TNyxSourcePublicationReceipt): TNyxSourceCompletion;
    procedure AbandonSharedSource(AOutcome: TNyxSourcePublicationOutcome;
      const AMessage: TNyxText);
    procedure Initialize(ASession: TNyxStudioSession; ARefresh: TNyxAgentRefresh;
      const AWorkspace: TNyxWorkspaceRef);
    function Frame: TNyxText;
    procedure Send(const AMessage: TNyxDataValue; AConnect: Boolean = False);
    procedure Ready(AStatus: Integer; const AText: TNyxText);
    procedure Tick;
    procedure Schedule;
    procedure Queue(const AMessage: TNyxDataValue);
    procedure CaptureLocal;
  public
    constructor Create(ASession: TNyxStudioSession; ARefresh: TNyxAgentRefresh); overload;
    constructor Create(ASession: TNyxStudioSession; ARefresh: TNyxAgentRefresh;
      const AWorkspace: TNyxWorkspaceRef); overload;
    { Takes immediate ownership of AExchange; the borrowed session/receiver must
      outlive this bridge. Platform transport cancellation precedes their release. }
    constructor Create(ASession: TNyxStudioSession; ARefresh: TNyxAgentRefresh;
      const AWorkspace: TNyxWorkspaceRef; AExchange: TNyxStudioEditorExchange); overload;
    destructor Destroy; override;
    { Protect explicit native local recovery even when the bridge was attached
      after that recovery. Ordinary browser initialization retains its baseline. }
    procedure Connect(AProtectLocal: Boolean = False);
    { Capture an ordinary publication or explicit persistence/history boundary
      immediately. One fresh paired snapshot feeds sharing and optional recovery.
      Accepted content commands retain their existing order and history. }
    procedure RecordLocal;
    { Source typing only marks work. The existing owned timer captures the latest
      draft once within 250 ms instead of encoding a whole project per keystroke.
      Later keystrokes do not postpone that window indefinitely. The local editor
      and session already own exact text before this call. An unsent marker blocks
      remote adoption/build/navigation; it never grants an acknowledged frame. }
    procedure RecordDraft;
    { Explicit idle host configuration; default startup remains compiler-free.
      The returned managed port is revoked before bridge/session retirement. }
    procedure UseSharedSourceFactory(const AFactory: INyxSharedSourceCompilerFactory);
    function SharedSourceHost: INyxSharedSourceHost;
    procedure Configure(APermission: TNyxAgentPermission);
    { Desired copied presentation selector, never a document edit. Requests wait
      for capability negotiation and an acknowledged exact pair. Closing the
      Resources pane clears inspection; late replies cannot select a file. }
    procedure ObserveResource(const AReference: TNyxResourceRef;
      const ALocale: TNyxLocaleRef; AEnabled: Boolean);
    procedure History(ADirection: TNyxEditorHistory);
    procedure CompilerReport(const AReport: TNyxText);
    { Operator compiler requests use the shared asynchronous job service. Query
      packets are bounded; requests require an acknowledged exact source frame.
      Queued packets retain this bridge's immutable project identity. }
    procedure CompilerOutputs;
    { Machine profiles use the private editor capability, never public MCP.
      Compare the expected output identity before saving; local paths remain
      separate from every portable project and its paired history. }
    procedure CompilerProfile;
    procedure SaveCompilerProfile(AProfile: TNyxOutputConfiguration;
      const AExpected: TNyxBuildOutputRef);
    procedure RequestBuild(const ARequest: INyxCompilerRequest);
    procedure BuildStatus(const AJob: TNyxBuildJobRef; AOffset: Integer = 0);
    { Private launch admission rechecks current compiler context. }
    procedure PreviewGrant(const AJob: TNyxBuildJobRef; ALaunchSequence: Integer = 0);
    { Acknowledges only this observer's actual placement/refusal, with private
      editor authority. It never publishes an application success claim. }
    procedure ReportLaunch(const ALaunch: TNyxCompilerLaunch;
      AHost: TNyxBuildTarget; AResult: TNyxCompilerLaunchResult;
      const ADetail: TNyxText = '');
    procedure CompilerJobs(AFilter: TNyxCompilerJobFilter = cjfActive;
      AOffset: Integer = 0; ALimit: Integer = 10);
    { Does not cancel the transport request: asks the service to retire exactly
      this owned job through the trusted operator exchange. }
    procedure CancelBuild(const AJob: TNyxBuildJobRef;
      const AOperation: TNyxBuildOperationRef);
    { Shared ordinary-control routing. Uses exact job metadata and fresh current
      revision, never a row index or HTTP request abort. Returns whether handled. }
    function RouteBuildCancel(ANode: TNyxNode; ATrigger: TNyxTrigger): Boolean;
    procedure Pause;
    procedure AcceptRemote;
    function State: TNyxStudioAgentView;
    { Navigation must wait for every local pair/draft publication. Observations
      may be in flight because their immutable target never changes. }
    function CanSwitchWorkspace: Boolean;
    { Capture one other project's exact revision for a visible warning. Keeping
      or confirming that warning never changes this observer's fixed target. }
    procedure RequestWorkspaceClose(const AReference: TNyxWorkspaceRef);
    procedure CancelWorkspaceClose;
    procedure ConfirmWorkspaceClose;
    { Exact locally retained pair/frame must match the acknowledged service
      frame before server diagnostics can borrow this observer's source text.
      Pending local publications and protected conflicts cannot grant navigation. }
    function SourceSynchronized: Boolean;
    { Compiler polling does not alter the accepted document revision. Permit
      cancellation behind compiler-only packets, but never behind a pending
      local capture, document/history operation or unacknowledged frame. }
    function CanCancelBuild: Boolean;
    property Applying: Boolean read FApplying;
    property Enabled: Boolean read FEnabled;
    property DraftCapturePending: Boolean read FDraftCapturePending;
    { Borrowed optional persistence receiver. Capture also works while sharing
      is paused; clear this callback before releasing its owner. }
    property OnProjectCaptured: TNyxProjectCaptured read FOnProjectCaptured
      write FOnProjectCaptured;
  end;

implementation

uses
  {$ifdef PAS2JS}nyx.studio.exchange.browser{$else}nyx.studio.exchange.lcl{$endif};

type
  TBridgeSharedPort = class(TInterfacedObject, INyxSharedSourceHost)
  public
    { Sole borrow, revoked by the bridge before its transport/session retirement. }
    Owner: TNyxStudioAgentBridge;
    procedure Attach(const ADispatch: INyxSharedSourceDispatch);
    procedure Detach;
    function Ready: Boolean;
    function Waiting: Boolean;
    function Start(const ASource: TNyxText;
      const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
    function Admit(const ARequest: TNyxStudioSourceRequest;
      const APrepared: INyxPreparedSource;
      const AReceipt: TNyxSourcePublicationReceipt): TNyxSourceCompletion;
    procedure Abandon(AOutcome: TNyxSourcePublicationOutcome; const AMessage: TNyxText);
  end;

const
  CRevisionLabel: TNyxText = ' · revision ';
  CWarningSeparator: TNyxText = ' · ';

procedure TBridgeSharedPort.Attach(const ADispatch: INyxSharedSourceDispatch);
begin

  if Owner <> nil then
  begin
    Owner.FSharedDispatch := ADispatch;
  end;
end;

procedure TBridgeSharedPort.Detach;
begin

  if Owner <> nil then
  begin
    Owner.FSharedDispatch := nil;
    Owner := nil;
  end;
end;

function TBridgeSharedPort.Ready: Boolean;
begin
  Result := (Owner <> nil) and (Owner.FSharedFactory <> nil) and
    Owner.FSharedAvailable and Owner.FEnabled and Owner.SourceSynchronized;
end;

function TBridgeSharedPort.Waiting: Boolean;
begin
  Result := (Owner <> nil) and
    (Owner.FSharedReserved or Owner.FSharedAcknowledging);
end;

function TBridgeSharedPort.Start(const ASource: TNyxText;
  const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
begin

  if Owner = nil then
  begin
    raise ENyxProjectConflict.Create('Shared compiler editor has retired');
  end;
  Result := Owner.StartSharedSource(ASource, APort);
end;

function TBridgeSharedPort.Admit(const ARequest: TNyxStudioSourceRequest;
  const APrepared: INyxPreparedSource;
  const AReceipt: TNyxSourcePublicationReceipt): TNyxSourceCompletion;
begin

  if Owner = nil then
  begin
    Exit(nscStale);
  end;
  Result := Owner.AdmitSharedSource(ARequest, APrepared, AReceipt);
end;

procedure TBridgeSharedPort.Abandon(AOutcome: TNyxSourcePublicationOutcome;
  const AMessage: TNyxText);
begin

  if Owner <> nil then
  begin
    Owner.AbandonSharedSource(AOutcome, AMessage);
  end;
end;

procedure TNyxStudioAgentBridge.UseSharedSourceFactory(
  const AFactory: INyxSharedSourceCompilerFactory);
begin

  if FSharedReserved or FSharedAcknowledging then
  begin
    raise ENyxProjectConflict.Create('Configure shared source compilation on an idle editor');
  end;
  FSharedFactory := AFactory;
end;

function TNyxStudioAgentBridge.SharedSourceHost: INyxSharedSourceHost;
begin
  Result := FSharedPort;
end;

function TNyxStudioAgentBridge.StartSharedSource(const ASource: TNyxText;
  const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
var
  LCompiler: INyxSharedSourceCompiler;
begin

  if not FSharedPort.Ready or (APort = nil) or
    (ASource <> FSession.DraftSource) then
  begin
    raise ENyxProjectConflict.Create('Shared source compilation requires the exact acknowledged draft');
  end;
  LCompiler := FSharedFactory.CreateCompiler(FToken, FSourceObservationIssuer,
    FWorkspace, FView.Revision);

  if LCompiler = nil then
  begin
    raise ENyxProjectConflict.Create('Shared source factory returned no compiler');
  end;
  FSharedAccepted := FSession.ProjectSnapshot;
  FSharedSource := ASource;
  FSharedRevision := FView.Revision;
  FSharedReserved := True;
  { An older observing read has no mutation authority. Retire its delivery before
    reserving this context; the compiler's independent HTTP channel owns its work. }
  FExchange.CancelRequest;
  FRequest := False;
  FView.Busy := False;
  FExchange.CancelTick;
  FTimer := False;
  try
    Result := LCompiler.Start(ASource, APort);

    if Result = nil then
    begin
      raise ENyxProjectConflict.Create('Shared source compiler returned no operation');
    end;
  except
    AbandonSharedSource(npoUnconfirmed, 'Shared compiler start needs observing reconciliation');
    raise;
  end;
end;

function TNyxStudioAgentBridge.AdmitSharedSource(const ARequest: TNyxStudioSourceRequest;
  const APrepared: INyxPreparedSource;
  const AReceipt: TNyxSourcePublicationReceipt): TNyxSourceCompletion;
begin
  Result := nscStale;

  if not FSharedReserved or not FEnabled or FView.Conflict or
    (AReceipt.Issuer <> FSourceObservationIssuer) or
    (AReceipt.Workspace.ID <> FWorkspace.ID) or
    (AReceipt.Revision <> FSharedRevision + 1) or
    (ARequest.Source <> FSharedSource) or (APrepared = nil) or
    (APrepared.Source <> FSharedSource) then
  begin
    AbandonSharedSource(npoUnconfirmed, 'Committed source belongs to a retired or changed editor context');
    Exit;
  end;
  { The existing sealed source request refuses newer buffers, creator changes
    and reloaded owners before committing one ordinary local history step. }
  Result := FSession.CompleteSourceRequest(ARequest, APrepared);

  if not (Result in [nscApplied, nscUnchanged]) then
  begin
    AbandonSharedSource(npoUnconfirmed, 'Server committed an earlier draft; local work needs reconciliation');
    Exit;
  end;
  FSharedAccepted := FSession.ProjectSnapshot;
  FSharedRevision := AReceipt.Revision;
  FSharedReserved := False;
  FSharedAcknowledging := True;
  FView.Status := 'Pascal applied; waiting for shared acknowledgement';
  Schedule;
end;

procedure TNyxStudioAgentBridge.AbandonSharedSource(AOutcome: TNyxSourcePublicationOutcome;
  const AMessage: TNyxText);
begin

  if not FSharedReserved and not FSharedAcknowledging then
  begin
    Exit;
  end;
  FSharedReserved := False;
  FSharedAcknowledging := False;
  FSharedSource := '';
  FSharedAccepted := Default(TNyxProjectPair);

  if AOutcome <> npoRefused then
  begin
    FView.Conflict := True;
    FView.Status := 'Shared source needs reconciliation: ' + AMessage;
  end;

  if FSharedDirty then
  begin
    FDraftCapturePending := True;
    FSharedDirty := False;
  end;
  Schedule;
end;

function DefaultEditorExchange: TNyxStudioEditorExchange;
begin
  {$ifdef PAS2JS}
  Result := TNyxBrowserEditorExchange.Create;
  {$else}
  Result := TNyxLCLEditorExchange.Create('http://127.0.0.1:8088');
  {$endif}
end;

function TNyxStudioAgentBridge.SourceSynchronized: Boolean;
begin
  Result := FView.Connected and not FView.Conflict and
    not FSharedReserved and not FSharedAcknowledging and not FSharedDirty and
    not FDraftCapturePending and (Length(FQueue) = 0) and (Frame = FKnownFrame);
end;

function TNyxStudioAgentBridge.CanCancelBuild: Boolean;
var
  LIndex: Integer;
begin
  Result := FEnabled and FView.CanControlBuilds and FView.Connected and
    not FView.Conflict and not FDraftCapturePending and (Frame = FKnownFrame);

  if not Result then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FQueue) do
  begin

    if FQueue[LIndex].Field('op').AsText <> 'build' then
    begin
      Exit(False);
    end;
  end;
end;

constructor TNyxStudioAgentBridge.Create(ASession: TNyxStudioSession;
  ARefresh: TNyxAgentRefresh);
begin
  inherited Create;
  FExchange := DefaultEditorExchange;
  Initialize(ASession, ARefresh, NyxPrimaryWorkspace);
end;

constructor TNyxStudioAgentBridge.Create(ASession: TNyxStudioSession;
  ARefresh: TNyxAgentRefresh; const AWorkspace: TNyxWorkspaceRef);
begin
  inherited Create;
  FExchange := DefaultEditorExchange;
  Initialize(ASession, ARefresh, AWorkspace);
end;

constructor TNyxStudioAgentBridge.Create(ASession: TNyxStudioSession;
  ARefresh: TNyxAgentRefresh; const AWorkspace: TNyxWorkspaceRef;
  AExchange: TNyxStudioEditorExchange);
begin
  inherited Create;
  FExchange := AExchange;

  if (FExchange = nil) or (ASession = nil) then
  begin
    raise Exception.Create('An editor bridge requires a session and transport');
  end;
  Initialize(ASession, ARefresh, AWorkspace);
end;

procedure TNyxStudioAgentBridge.Initialize(ASession: TNyxStudioSession;
  ARefresh: TNyxAgentRefresh; const AWorkspace: TNyxWorkspaceRef);
var
  LSharedPort: TBridgeSharedPort;
begin
  FWorkspace := AWorkspace;
  FSession := ASession;
  FView := DefaultNyxStudioAgentView;
  FView.Workspace := FWorkspace;
  FTimer := False;
  FInitialProject := EncodeNyxProject(FSession.ProjectSnapshot);
  FOnRefresh := ARefresh;
  LSharedPort := TBridgeSharedPort.Create;
  FSharedPort := LSharedPort;
  LSharedPort.Owner := Self;
end;

destructor TNyxStudioAgentBridge.Destroy;
begin
  FEnabled := False;

  if FSharedPort <> nil then
  begin
    FSharedPort.Detach;
  end;
  FSharedPort := nil;
  FSharedFactory := nil;
  FOnProjectCaptured := nil;
  FExchange.Free;
  FExchange := nil;
  FOnRefresh := nil;
  FSession := nil;
  inherited Destroy;
end;

function TNyxStudioAgentBridge.State: TNyxStudioAgentView;
begin
  Result := FView;

  if FDraftCapturePending and FEnabled and not FView.Conflict then
  begin
    Result.Status := 'Local Pascal draft waiting to synchronize';
  end;
end;

function TNyxStudioAgentBridge.CanSwitchWorkspace: Boolean;
begin
  Result := FEnabled and FView.Connected and not FView.Conflict and
    not FSharedReserved and not FSharedAcknowledging and not FSharedDirty and
    not FDraftCapturePending and (Length(FQueue) = 0) and (Frame = FKnownFrame);
end;

function TNyxStudioAgentBridge.Frame: TNyxText;
begin
  Result := NyxObject([
    NyxField('project', NyxData(EncodeNyxProject(FSession.ProjectSnapshot))),
    NyxField('selection', NyxData(FSession.SelectedID)),
    NyxField('view', NyxData(FSession.ActiveViewID))]).ToJSON;
end;

procedure TNyxStudioAgentBridge.Connect(AProtectLocal: Boolean);
var
  LFrame: TNyxDataValue;
begin

  if FRequest then
  begin
    Exit;
  end;
  FEnabled := True;
  FView.Conflict := False;
  FView.Connected := False;
  FView.Status := 'Connecting agent session';
  FKnownFrame := Frame;
  FDraftCapturePending := False;
  FProtectLocal := AProtectLocal or
    (EncodeNyxProject(FSession.ProjectSnapshot) <> FInitialProject);
  LFrame := TNyxDataValue.ParseJSON(FKnownFrame);
  Send(NyxObject([NyxField('op', NyxData('claim')),
    NyxField('project', LFrame.Field('project')),
    NyxField('selection', LFrame.Field('selection')),
    NyxField('view', LFrame.Field('view'))]), True);
end;

procedure TNyxStudioAgentBridge.Send(const AMessage: TNyxDataValue; AConnect: Boolean);
begin
  FSent := AMessage.Copy;
  FSentFrame := FKnownFrame;
  FView.Busy := True;
  FRequest := True;
  FExchange.Post(AConnect, FToken, NyxWithWorkspace(AMessage, FWorkspace).ToJSON, Ready);
end;

procedure TNyxStudioAgentBridge.Queue(const AMessage: TNyxDataValue);
var
  LSize: Integer;
  LLast: Integer;
  LPrevious: TNyxProjectPair;
  LCurrent: TNyxProjectPair;
  LMayMerge: Boolean;
begin
  LSize := Length(AMessage.ToJSON);
  LLast := High(FQueue);
  LMayMerge := (LLast >= 0) and (AMessage.Field('op').AsText = 'commit');

  if LMayMerge and FRequest and (LLast = 0) then
  begin
    LMayMerge := FSent.Field('op').AsText <> 'commit';
  end;

  if LMayMerge and (FQueue[LLast].Field('op').AsText = 'commit') then
  begin
    LCurrent := DecodeNyxProject(AMessage.Field('project').AsText);
    LPrevious := DecodeNyxProject(FQueue[LLast].Field('project').AsText);
    { Coalesce only unsent draft updates of the SAME accepted pair. Content
      publications remain ordered, one per history command. Never replace an
      in-flight queue head whose acknowledgement would discard the newer draft. }

    if LCurrent.Pending and (LCurrent.Design = LPrevious.Design) and
      (LCurrent.Source = LPrevious.Source) and
      (LSize <= 8 * 1024 * 1024 - FQueueUnits + FQueueSizes[LLast]) then
    begin
      Dec(FQueueUnits, FQueueSizes[LLast]);
      FQueue[LLast] := AMessage.Copy;
      FQueueSizes[LLast] := LSize;
      Inc(FQueueUnits, LSize);
      Exit;
    end;
  end;

  if (Length(FQueue) >= 100) or (LSize > 8 * 1024 * 1024 - FQueueUnits) then
  begin
    FView.Conflict := True;
    FView.Status := 'Agent synchronization queue is full; local work is retained';
    Exit;
  end;
  SetLength(FQueue, Length(FQueue) + 1);
  FQueue[High(FQueue)] := AMessage.Copy;
  SetLength(FQueueSizes, Length(FQueueSizes) + 1);
  FQueueSizes[High(FQueueSizes)] := LSize;
  Inc(FQueueUnits, LSize);
end;

procedure TNyxStudioAgentBridge.CaptureLocal;
var
  LFrame: TNyxText;
  LProject: TNyxText;
begin

  if FApplying then
  begin
    Exit;
  end;
  { A snapshot is operation-owned, never cached across direct public mutation.
    Recovery and sharing consume these same bytes, including the original draft
    base. No UI callbacks run between capturing the pair and queuing its frame. }
  LProject := EncodeNyxProject(FSession.ProjectSnapshot);
  LFrame := NyxObject([NyxField('project', NyxData(LProject)),
    NyxField('selection', NyxData(FSession.SelectedID)),
    NyxField('view', NyxData(FSession.ActiveViewID))]).ToJSON;

  if FSharedReserved or FSharedAcknowledging then
  begin
    FSharedDirty := True;
  end
  else if FEnabled and not FView.Conflict and (LFrame <> FKnownFrame) then
  begin
    Queue(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('project', NyxData(LProject)),
      NyxField('selection', NyxData(FSession.SelectedID)),
      NyxField('view', NyxData(FSession.ActiveViewID))]));

    if not FView.Conflict then
    begin
      FKnownFrame := LFrame;
    end;
  end;
  FDraftCapturePending := False;

  if Assigned(FOnProjectCaptured) then
  begin
    FOnProjectCaptured(LProject);
  end;
end;

procedure TNyxStudioAgentBridge.RecordLocal;
begin

  if FApplying or ((not FEnabled or FView.Conflict) and
    not Assigned(FOnProjectCaptured)) then
  begin
    Exit;
  end;
  CaptureLocal;

  if FEnabled and not FView.Conflict and not FRequest then
  begin
    Tick;
  end;
end;

procedure TNyxStudioAgentBridge.RecordDraft;
begin

  if FApplying or ((not FEnabled or FView.Conflict) and
    not Assigned(FOnProjectCaptured)) then
  begin
    Exit;
  end;

  if FDraftCapturePending then
  begin
    Exit;
  end;
  FDraftCapturePending := True;
  { Wake an existing observation timer once. The timer remains owned by this
    immutable project bridge, so cancellation cannot retarget another session. }
  FExchange.CancelTick;
  FTimer := False;
  Schedule;
end;

procedure TNyxStudioAgentBridge.Configure(APermission: TNyxAgentPermission);
begin
  Queue(NyxObject([NyxField('op', NyxData('configure')),
    NyxField('permission', NyxData(NyxAgentPermissionName(APermission)))]));
  Tick;
end;

procedure TNyxStudioAgentBridge.RequestWorkspaceClose(const AReference: TNyxWorkspaceRef);
var
  LIndex: Integer;
  LItem: TNyxDataValue;
begin

  if not CanSwitchWorkspace or (AReference.ID = '') or
    (AReference.ID = FWorkspace.ID) then
  begin
    raise Exception.Create('Switch to another synchronized project before closing this one');
  end;
  for LIndex := 0 to FView.Workspaces.Count - 1 do
  begin
    LItem := FView.Workspaces.Item(LIndex);

    if LItem.Field('workspace').AsText = AReference.ID then
    begin
      FView.CloseWorkspace := AReference;
      FView.CloseRevision := LItem.Field('session').Field('revision').AsInteger;
      FView.CloseLabel := LItem.Field('label').AsText;

      if Assigned(FOnRefresh) then
      begin
        FOnRefresh(False);
      end;
      Exit;
    end;
  end;
  raise Exception.Create('The project to close is no longer open');
end;

procedure TNyxStudioAgentBridge.CancelWorkspaceClose;
begin
  FView.CloseWorkspace := NyxPrimaryWorkspace;
  FView.CloseRevision := 0;
  FView.CloseLabel := '';

  if Assigned(FOnRefresh) then
  begin
    FOnRefresh(False);
  end;
end;

procedure TNyxStudioAgentBridge.ConfirmWorkspaceClose;
begin

  if not FView.CanCloseWorkspace or not CanSwitchWorkspace or (FView.CloseWorkspace.ID = '') then
  begin
    raise Exception.Create('Project close requires a synchronized editor and an explicit warning');
  end;
  Queue(NyxObject([NyxField('op', NyxData('close-workspace')),
    NyxField('target', NyxData(FView.CloseWorkspace.ID)),
    NyxField('expectedRevision', NyxData(FView.CloseRevision)),
    NyxField('confirmed', NyxData(True))]));
  CancelWorkspaceClose;
  Tick;
end;

procedure TNyxStudioAgentBridge.History(ADirection: TNyxEditorHistory);
const
  CDirections: array[TNyxEditorHistory] of TNyxText = ('undo', 'redo');
begin
  RecordLocal;
  Queue(NyxObject([NyxField('op', NyxData('history')),
    NyxField('direction', NyxData(CDirections[ADirection]))]));
  Tick;
end;

procedure TNyxStudioAgentBridge.CompilerReport(const AReport: TNyxText);
begin

  if not FEnabled then
  begin
    Exit;
  end;
  Queue(NyxObject([NyxField('op', NyxData('report')), NyxField('report', NyxData(AReport))]));
end;

procedure TNyxStudioAgentBridge.Pause;
begin
  AbandonSharedSource(npoUnconfirmed, 'Shared synchronization was paused');
  FEnabled := False;
  FView.Connected := False;
  FView.Status := 'Agent sync paused; your local work is retained';

  FExchange.CancelTick;
  FTimer := False;
  { A paused observer must not apply an already-in-flight response after the
    operator chose to keep local work. Server admission may finish independently. }

  FExchange.CancelRequest;
  FRequest := False;
  FView.Busy := False;
  { Sharing can pause independently of automatic recovery. A retained local
    marker must still reach its persistence consumer through the same timer. }
  Schedule;
end;

procedure TNyxStudioAgentBridge.AcceptRemote;
begin
  FSharedReserved := False;
  FSharedAcknowledging := False;
  FSharedDirty := False;
  FSharedSource := '';
  FSharedAccepted := Default(TNyxProjectPair);
  FQueue := nil;
  FQueueSizes := nil;
  FQueueUnits := 0;
  FDraftCapturePending := False;
  FAcceptRemote := True;
  FView.Conflict := False;
  Send(NyxObject([NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
end;

procedure TNyxStudioAgentBridge.Schedule;
begin

  if not FTimer and ((FEnabled and not FView.Conflict) or
    (FDraftCapturePending and Assigned(FOnProjectCaptured))) then
  begin
    FTimer := True;

    if FDraftCapturePending then
    begin
      FExchange.Schedule(250, Tick);
    end
    else if FSharedAcknowledging or (Length(FQueue) > 0) then
    begin
      FExchange.Schedule(25, Tick);
    end
    else
    begin
      FExchange.Schedule(500, Tick);
    end;
  end;
end;

procedure TNyxStudioAgentBridge.Tick;
var
  LMessage: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LOperation: TNyxText;
  LFieldCount: Integer;
begin

  FExchange.CancelTick;
  FTimer := False;

  if FDraftCapturePending then
  begin
    try
      CaptureLocal;
    except
      on LException: Exception do
      begin
        { Timer failures retain the local marker and accepted pair. Do not spin
          on malformed direct edits or rewrite the last recovery with a partial
          capture. Explicit repair/publication may resume this bridge. }
        FView.Conflict := True;
        FView.Status := 'Local capture needs attention: ' + LException.Message;

        if Assigned(FOnRefresh) then
        begin
          FOnRefresh(False);
        end;
        Exit;
      end;
    end;
  end;

  if not FEnabled or FView.Conflict then
  begin
    Exit;
  end;

  if FSharedReserved then
  begin
    Exit;
  end;

  if FRequest or not FView.Connected then
  begin
    Schedule;
    Exit;
  end;

  if FSharedAcknowledging then
  begin
    { Force the complete admitted pair, even if another read observed this
      revision. Only exact source/design/revision acknowledgement releases edits. }
    Send(NyxObject([NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
    Exit;
  end;

  if Length(FQueue) > 0 then
  begin
    LMessage := FQueue[0];
    LOperation := LMessage.Field('op').AsText;
    LFieldCount := LMessage.Count;
    SetLength(LFields, LFieldCount + 1);
    for LIndex := 0 to LFieldCount - 1 do
    begin
      LFields[LIndex] := NyxField(LMessage.Key(LIndex), LMessage.Field(LMessage.Key(LIndex)));
    end;
    LFields[LFieldCount] := NyxField('after', NyxData(FView.Revision));

    if (LOperation = 'commit') or (LOperation = 'history') then
    begin
      SetLength(LFields, Length(LFields) + 1);
      LFields[High(LFields)] := NyxField('expectedRevision', NyxData(FView.Revision));
    end;
    Send(NyxObject(LFields));
  end
  else
  begin

    if FResourceInspection and FView.CanInspectResourceRuntime and SourceSynchronized then
    begin
      Send(NyxObject([NyxField('op', NyxData('observe')), NyxField('after', NyxData(FView.Revision)),
        NyxField('resource', NyxObject([NyxField('reference', NyxData(FResourceReference.Name)),
          NyxField('locale', NyxData(FResourceLocale.Name))]))]));
    end
    else
    begin
      Send(NyxObject([NyxField('op', NyxData('observe')), NyxField('after', NyxData(FView.Revision))]));
    end;
  end;
end;

procedure TNyxStudioAgentBridge.ObserveResource(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef; AEnabled: Boolean);
begin
  AEnabled := AEnabled and (AReference.Name <> '');

  if (FResourceInspection = AEnabled) and
    (FResourceReference.Name = AReference.Name) and (FResourceLocale.Name = ALocale.Name) then
  begin
    Exit;
  end;
  FResourceReference := AReference;
  FResourceLocale := ALocale;
  FResourceInspection := AEnabled;
  Schedule;
end;

procedure TNyxStudioAgentBridge.Ready(AStatus: Integer; const AText: TNyxText);
var
  LData: TNyxDataValue;
  LState: TNyxDataValue;
  LResourceReports: TNyxDataValue;
  LSummary: TNyxDataValue;
  LPair: TNyxProjectPair;
  LCheckpoint: TNyxSourceCheckpoint;
  LRemoteFrame: TNyxText;
  LOperation: TNyxText;
  LChanged: Boolean;
  LRefresh: Boolean;
  LIndex: Integer;
  LStatus: Integer;
  LText: TNyxText;
  LPermission: TNyxText;
  LSharedAcknowledgement: Boolean;
begin

  if not FRequest or not FEnabled then
  begin
    Exit;
  end;
  { A capture/queue refusal can occur while an older request is in flight.
    Its late acknowledgement cannot clear that refusal or replace local work.
    The operator must resolve the retained conflict explicitly. }

  if FView.Conflict and not FAcceptRemote then
  begin
    FRequest := False;
    FView.Busy := False;
    Exit;
  end;
  LStatus := AStatus;
  LText := AText;
  FRequest := False;
  FView.Busy := False;
  LChanged := False;
  LRefresh := False;
  LSharedAcknowledgement := FSharedAcknowledging;
  FApplying := True;
  try
    try

      if LStatus <> 200 then
      begin

        if LText <> '' then
        begin
          LData := TNyxDataValue.ParseJSON(LText);

          if NyxAgentHas(LData, 'error') then
          begin
            raise Exception.Create('Agent sync refused: ' + LData.Field('error').AsText + '. Local work is retained');
          end;
        end;
        raise Exception.Create('Agent sync unavailable; local work is retained');
      end;
      LData := TNyxDataValue.ParseJSON(LText);
      LOperation := FSent.Field('op').AsText;

      if LOperation = 'claim' then
      begin
        FToken := LData.Field('token').AsText;
        FView.Endpoint := LData.Field('endpoint').AsText;

        if NyxAgentHas(LData, 'warning') then
        begin
          FWarning := LData.Field('warning').AsText;
        end;
        LState := LData.Field('state');
      end
      else
      begin
        LState := LData;
      end;
      { Admission must name the same immutable editor context. A missing or
        substituted project response cannot replace local source or recovery. }

      if (NyxAgentHas(LState, 'workspace') and
        (LState.Field('workspace').AsText <> FWorkspace.ID)) or
        ((FWorkspace.ID <> '') and not NyxAgentHas(LState, 'workspace')) then
      begin
        raise Exception.Create('Editor response belongs to another project; local work is retained');
      end;
      { A fresh claim negotiates a server lifetime; subsequent replies must retain
        it. A restart or stale reply cannot quietly reauthorize local source. }

      if LOperation = 'claim' then
      begin
        FSourceObservationIssuer := '';

        if NyxAgentHas(LState, 'sourceObservationIssuer') then
        begin
          FSourceObservationIssuer := LState.Field('sourceObservationIssuer').AsText;

          if (FSourceObservationIssuer = '') or (Length(FSourceObservationIssuer) > 128) then
          begin
            raise Exception.Create('Invalid owning source observation identity');
          end;
        end;
      end
      else if FSourceObservationIssuer <> '' then
      begin

        if not NyxAgentHas(LState, 'sourceObservationIssuer') or
          (LState.Field('sourceObservationIssuer').AsText <> FSourceObservationIssuer) then
        begin
          raise Exception.Create('Owning source observation server changed; local work is retained');
        end;
      end;
      LSummary := LState.Field('session');
      FSharedAvailable := (FSourceObservationIssuer <> '') and
        NyxAgentHas(LState, 'sharedSourcePublication') and
        LState.Field('sharedSourcePublication').AsBoolean;

      if (LOperation <> 'claim') and
        (LSummary.Field('revision').AsInteger < FView.Revision) then
      begin
        raise Exception.Create('Stale editor response cannot replace the observed revision');
      end;
      { Validate every negotiated paired response before acknowledging a queued
        command or changing presentation metadata. Only the adoption branch below
        may publish its independently admitted owners. }

      if NyxAgentHas(LState, 'project') then
      begin
        LPair := DecodeNyxProject(LState.Field('project').AsText);

        if FSourceObservationIssuer <> '' then
        begin
          LCheckpoint := ReceiveNyxSourceObservation(LState.Field('sourceObservation'),
            FSourceObservationIssuer, FWorkspace, LSummary.Field('revision').AsInteger, LPair);
        end;
      end;

      if LSharedAcknowledgement then
      begin

        if (LOperation <> 'observe') or not NyxAgentHas(LState, 'project') then
        begin
          raise ENyxProjectConflict.Create('Shared source acknowledgement requires the complete observed pair');
        end;

        if (LSummary.Field('revision').AsInteger <> FSharedRevision) or
          (EncodeNyxProject(LPair) <> EncodeNyxProject(FSharedAccepted)) then
        begin
          raise ENyxProjectConflict.Create('Shared source changed before acknowledgement; local pair and draft are retained');
        end;
      end;
      LRefresh := (LSummary.Field('activitySequence').AsInteger <>
        FActivitySerial) or not FView.Connected;
      FActivitySerial := LSummary.Field('activitySequence').AsInteger;
      FView.Connected := True;
      LPermission := LSummary.Field('permission').AsText;

      if LPermission = 'disabled' then
      begin
        FView.Permission := apDisabled;
      end
      else if LPermission = 'readOnly' then
      begin
        FView.Permission := apReadOnly;
      end
      else if LPermission = 'edit' then
      begin
        FView.Permission := apEdit;
      end
      else
      begin
        raise Exception.Create('Invalid agent permission response');
      end;
      FView.Activity := LState.Field('activity').Copy;
      { A sequence is delivery identity, not a document revision or heartbeat.
        Older services omit this optional capability and retain manual builds. }

      if NyxAgentHas(LState, 'buildLaunch') and
        (LState.Field('buildLaunch').Kind = ndObject) and
        (LState.Field('buildLaunch').Field('state').AsText = 'requested') then
      begin
        LRefresh := LRefresh or (FView.CompilerLaunch.Sequence <>
          LState.Field('buildLaunch').Field('sequence').AsInteger);
        FView.CompilerLaunch := DecodeNyxCompilerLaunch(LState.Field('buildLaunch'));
      end
      else
      begin
        LRefresh := LRefresh or (FView.CompilerLaunch.Sequence <> 0);
        FView.CompilerLaunch := Default(TNyxCompilerLaunch);
      end;
      { Selection can change independently of the activity sequence. Compare
        bounded copied reports so a fresh exact read refreshes its ordinary view. }
      LResourceReports := NyxArray([]);

      if NyxAgentHas(LState, 'resourceRuntimes') then
      begin
        LResourceReports := LState.Field('resourceRuntimes').Copy;
      end;
      LRefresh := LRefresh or (FView.ResourceRuntimes.ToJSON <> LResourceReports.ToJSON);
      FView.ResourceRuntimes := LResourceReports;
      FView.CanCloseWorkspace := False;
      FView.CanBuild := False;
      FView.CanControlBuilds := False;
      FView.CanReportRuntime := False;
      FView.CanInspectResourceRuntime := False;

      if NyxAgentHas(LState, 'resourceRuntimeSelection') then
      begin
        FView.CanInspectResourceRuntime := LState.Field('resourceRuntimeSelection').AsBoolean;
      end;

      if NyxAgentHas(LState, 'resourceRuntimeReporting') then
      begin
        FView.CanReportRuntime := LState.Field('resourceRuntimeReporting').AsBoolean;
      end;

      if NyxAgentHas(LState, 'editorBuilds') then
      begin
        FView.CanBuild := LState.Field('editorBuilds').AsBoolean;
      end;

      if NyxAgentHas(LState, 'buildReply') then
      begin
        { Associate with this exact serialized request purpose. A cancellation
          receipt has no currentness flags and is not a compiled status reply. }
        FView.BuildReplyKind := ParseNyxCompilerOperation(FSent.Field('build').Field('mode').AsText);
        FView.BuildReply := LState.Field('buildReply').Copy;
        Inc(FView.BuildReplySequence);
        LRefresh := True;
      end;

      if NyxAgentHas(LState, 'buildJobControl') then
      begin
        FView.CanControlBuilds := LState.Field('buildJobControl').AsBoolean;
      end;

      if NyxAgentHas(LState, 'buildJobs') then
      begin
        LRefresh := LRefresh or (FView.BuildJobs.ToJSON <> LState.Field('buildJobs').ToJSON);
        FView.BuildJobs := LState.Field('buildJobs').Copy;
      end;

      if NyxAgentHas(LState, 'workspaceClosing') then
      begin
        FView.CanCloseWorkspace := LState.Field('workspaceClosing').AsBoolean;
      end;

      if NyxAgentHas(LState, 'workspaces') then
      begin
        LRefresh := LRefresh or (FWorkspaceMetadata <> LState.Field('workspaces').ToJSON);
        FWorkspaceMetadata := LState.Field('workspaces').ToJSON;
        FView.Workspaces := LState.Field('workspaces').Copy;
      end;

      if NyxAgentHas(LState, 'reviews') then
      begin
        FView.Reviews := LState.Field('reviews').Copy;
      end;

      if NyxAgentHas(LState, 'compiler') then
      begin
        FView.Compiler := LState.Field('compiler').Copy;
      end;

      if (LOperation <> 'observe') and (LOperation <> 'claim') and (Length(FQueue) > 0) then
      begin
        Dec(FQueueUnits, FQueueSizes[0]);
        for LIndex := 1 to High(FQueue) do
        begin
          FQueue[LIndex - 1] := FQueue[LIndex];
          FQueueSizes[LIndex - 1] := FQueueSizes[LIndex];
        end;
        SetLength(FQueue, Length(FQueue) - 1);
        SetLength(FQueueSizes, Length(FQueueSizes) - 1);
      end;

      if LSharedAcknowledgement then
      begin
        { The local sealed completion already owns its one Undo step. A matching
          observation only acknowledges the server frame: never reload/adopt it
          over newer typing or later local presentation choices. }
        FKnownFrame := NyxObject([NyxField('project', LState.Field('project')),
          NyxField('selection', LSummary.Field('selection')),
          NyxField('view', LSummary.Field('view'))]).ToJSON;
        FSharedAcknowledging := False;
        FSharedSource := '';
        FSharedAccepted := Default(TNyxProjectPair);

        if FSharedDirty or (Frame <> FKnownFrame) then
        begin
          FDraftCapturePending := True;
        end;
        FSharedDirty := False;
      end
      else if NyxAgentHas(LState, 'project') then
      begin
        LRemoteFrame := NyxObject([NyxField('project', LState.Field('project')),
          NyxField('selection', LSummary.Field('selection')),
          NyxField('view', LSummary.Field('view'))]).ToJSON;

        if (LOperation = 'observe') or (LOperation = 'claim') or (LOperation = 'build') or
          ((LOperation = 'history') and (Length(FQueue) = 0)) then
        begin

          if (LOperation = 'claim') and FProtectLocal and
            (LRemoteFrame <> FSentFrame) and not FAcceptRemote then
          begin
            raise Exception.Create('A different shared design is active; your recovered/local project and draft are retained');
          end;

          if not FAcceptRemote and (Frame <> FSentFrame) and
            (LRemoteFrame <> FSentFrame) then
          begin
            raise Exception.Create('Shared revision changed while you edited; local work and draft are retained');
          end;

          if ((Length(FQueue) = 0) and not FDraftCapturePending) or FAcceptRemote then
          begin
            LChanged := EncodeNyxProject(FSession.ProjectSnapshot) <> EncodeNyxProject(LPair);

            if LChanged then
            begin
              if FSourceObservationIssuer = '' then
              begin
                FSession.LoadProject(LPair);
              end
              else if (LOperation = 'claim') or FAcceptRemote then
              begin
                FSession.LoadCapturedProject(LPair, LCheckpoint);
              end
              else
              begin
                FSession.AdoptCapturedProject(LPair, LCheckpoint, spaSynchronization);
              end;
            end;

            if LSummary.Field('view').AsText <> '' then
            begin
              FSession.Activate(LSummary.Field('view').AsText);
            end;

            if LSummary.Field('selection').AsText <> '' then
            begin
              FSession.Select(LSummary.Field('selection').AsText);
            end;
            FKnownFrame := Frame;

            if Assigned(FOnProjectCaptured) then
            begin
              FOnProjectCaptured(LState.Field('project').AsText);
            end;
          end;
        end;
      end;
      FAcceptRemote := False;
      FView.Revision := LSummary.Field('revision').AsInteger;
      FView.Status := 'Agents ' + NyxAgentPermissionName(FView.Permission) +
        CRevisionLabel + IntToStr(FView.Revision);

      if FWarning <> '' then
      begin
        FView.Status := FView.Status + CWarningSeparator + FWarning;
      end;

      if Length(FQueue) > 0 then
      begin
        FView.Status := 'Synchronizing editor changes';
      end;
      FView.Conflict := False;
      Schedule;
    except
      on LException: Exception do
      begin
        FView.Conflict := True;
        FView.Status := LException.Message;
        LRefresh := True;
      end;
    end;

    if Assigned(FOnRefresh) and (LRefresh or LChanged) then
    begin
      FOnRefresh(LChanged);
    end;
  finally
    FApplying := False;
  end;

  if FSharedDispatch <> nil then
  begin
    FSharedDispatch.Resume;
  end;
end;

procedure TNyxStudioAgentBridge.CompilerOutputs;
begin

  if not FView.CanBuild or not FView.Connected or FView.Conflict then
  begin
    raise Exception.Create('Asynchronous editor builds are unavailable on this service');
  end;
  Queue(NyxObject([NyxField('op', NyxData('build')),
    NyxField('build', NyxCompilerOutputs)]));
end;

procedure TNyxStudioAgentBridge.CompilerProfile;
begin

  if not FView.CanBuild or not FView.Connected or FView.Conflict then
  begin
    raise Exception.Create('Compiler configuration is unavailable on this service');
  end;
  Queue(NyxObject([NyxField('op', NyxData('build')),
    NyxField('build', NyxObject([NyxField('mode', NyxData('profile'))]))]));
end;

procedure TNyxStudioAgentBridge.SaveCompilerProfile(AProfile: TNyxOutputConfiguration;
  const AExpected: TNyxBuildOutputRef);
begin

  if not FView.CanBuild or not FView.Connected or FView.Conflict or
    (AProfile = nil) or (AExpected.ID = '') then
  begin
    raise Exception.Create('Saving output configuration requires its acknowledged profile identity');
  end;
  Queue(NyxObject([NyxField('op', NyxData('build')),
    NyxField('build', NyxObject([NyxField('mode', NyxData('profile')),
      NyxField('profile', TNyxDataValue.ParseJSON(AProfile.Encode)),
      NyxField('expectedOutputID', NyxData(AExpected.ID))]))]));
end;

procedure TNyxStudioAgentBridge.RequestBuild(const ARequest: INyxCompilerRequest);
begin

  if not FView.CanBuild or not SourceSynchronized or (ARequest = nil) then
  begin
    raise Exception.Create('Build requires an acknowledged exact accepted project');
  end;
  Queue(NyxObject([NyxField('op', NyxData('build')),
    NyxField('build', ARequest.Arguments)]));
end;

procedure TNyxStudioAgentBridge.BuildStatus(const AJob: TNyxBuildJobRef; AOffset: Integer);
begin

  if not FView.CanBuild or not FView.Connected or FView.Conflict then
  begin
    raise Exception.Create('Compiler status is unavailable on this service');
  end;
  Queue(NyxObject([NyxField('op', NyxData('build')),
    NyxField('build', NyxCompilerStatus(AJob, AOffset))]));
end;

procedure TNyxStudioAgentBridge.PreviewGrant(const AJob: TNyxBuildJobRef;
  ALaunchSequence: Integer);
begin

  if not SourceSynchronized or not FView.CanBuild or not FView.CanReportRuntime then
  begin
    raise ENyxModel.Create('Preview reporting requires the exact connected project');
  end;
  Queue(NyxObject([NyxField('op', NyxData('build')),
    NyxField('build', NyxCompilerPreview(AJob, FView.Revision, ALaunchSequence))]));
end;

procedure TNyxStudioAgentBridge.ReportLaunch(const ALaunch: TNyxCompilerLaunch;
  AHost: TNyxBuildTarget; AResult: TNyxCompilerLaunchResult; const ADetail: TNyxText);
begin

  if not FView.CanBuild or not FView.Connected or FView.Conflict then
  begin
    raise ENyxModel.Create('Launch acknowledgment requires the connected editor');
  end;
  Queue(NyxObject([NyxField('op', NyxData('build')),
    NyxField('build', NyxCompilerLaunchResult(ALaunch, AHost, AResult, ADetail))]));
end;

procedure TNyxStudioAgentBridge.CancelBuild(const AJob: TNyxBuildJobRef;
  const AOperation: TNyxBuildOperationRef);
begin

  if not CanCancelBuild then
  begin
    raise Exception.Create('Compiler cancellation requires a connected exact project');
  end;
  Queue(NyxObject([NyxField('op', NyxData('build')),
    NyxField('build', NyxCompilerCancel(AJob, FView.Revision, AOperation))]));
  Tick;
end;

function TNyxStudioAgentBridge.RouteBuildCancel(ANode: TNyxNode;
  ATrigger: TNyxTrigger): Boolean;
var
  LIdentity: TGUID;
begin
  Result := (ATrigger = ntClick) and (ANode <> nil) and
    ANode.Extensions.Has(NyxStudioCancelBuildKey);

  if Result then
  begin
    CreateGUID(LIdentity);
    CancelBuild(NyxBuildJob(ANode.Extensions.Value(NyxStudioCancelBuildKey).AsText),
      NyxBuildOperation('cancel-' + Copy(GUIDToString(LIdentity), 2, 36)));
  end;
end;

procedure TNyxStudioAgentBridge.CompilerJobs(AFilter: TNyxCompilerJobFilter;
  AOffset, ALimit: Integer);
begin

  if not FView.CanControlBuilds or not FEnabled or not FView.Connected or FView.Conflict then
  begin
    raise Exception.Create('Compiler job discovery is unavailable on this service');
  end;
  Queue(NyxObject([NyxField('op', NyxData('build')),
    NyxField('build', NyxCompilerJobs(AFilter, AOffset, ALimit))]));
end;

end.
