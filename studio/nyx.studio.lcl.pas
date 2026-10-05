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

unit nyx.studio.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls,
  nyx.text, nyx.types, nyx.behavior, nyx.data, nyx.model, nyx.theme, nyx.render.lcl,
  nyx.events, nyx.viewport,
  nyx.studio.session, nyx.studio.view, nyx.studio.projects,
  nyx.studio.projectstore, nyx.studio.outputs, nyx.studio.rootedits,
  nyx.studio.compiler, nyx.studio.agentbridge, nyx.studio.agentview,
  nyx.studio.workspaces, nyx.studio.builds, nyx.studio.editorbuild,
  nyx.studio.exchange, nyx.studio.preview, nyx.studio.preview.lcl,
  nyx.studio.sourcejobs;

type
  TNyxNativeStudio = class;
  TNyxNativeBuildStage = (nbsIdle, nbsProfile, nbsOutputs, nbsRequest, nbsPolling,
    nbsPreviewInspect, nbsPreviewDownload, nbsPreviewActivate, nbsTerminal);

  { Private native context owns one portable mirror and its immutable service
    bridge. Owner is borrowed. Views remain owned by Studio; this record retains
    typed editor state and scalar/scroll positions, never borrowed widget handles.
    Inactive bridges may observe their own project without painting another one. }
  TNyxNativeStudioProject = class
  public
    Owner: TNyxNativeStudio;
    Reference: TNyxWorkspaceRef;
    Session: TNyxStudioSession;
    SourceCommands: TNyxSourceCommands;
    Bridge: TNyxStudioAgentBridge;
    State: TNyxStudioViewState;
    Preview: Boolean;
    SavedPair: TNyxText;
    BoundProject: TNyxText;
    ProjectRevision: TNyxText;
    RemotePair: TNyxText;
    RemoteRevision: TNyxText;
    CodeStart: Integer;
    CodeEnd: Integer;
    CodeFocused: Boolean;
    CanvasTop: Integer;
    CanvasLeft: Integer;
    LeftTop: Integer;
    RightTop: Integer;
    { Compiler work belongs to this immutable project, including while hidden.
      Its captured pair gates result activation; the UI timer never owns Studio. }
    BuildStage: TNyxNativeBuildStage;
    BuildScope: TNyxBuildScope;
    BuildTarget: TNyxBuildTarget;
    BuildPair: TNyxText;
    BuildRoot: TNyxBuildRootRef;
    BuildJob: TNyxBuildJobRef;
    BuildReplySequence: Integer;
    BuildStatusPending: Boolean;
    BuildTimer: TTimer;
    BuildArtifact: TNyxText;
    BuildResult: TNyxDataValue;
    BuildProfileSent: TNyxText;
    BuildRequested: Boolean;
    BuildMessage: TNyxText;
    BuildOutputSnapshot: TNyxText;
    CompiledPreview: TNyxLCLCompiledPreview;
    procedure PreviewPrepared(ASucceeded: Boolean; const AError: TNyxText);
    procedure PollBuild(ASender: TObject);
    procedure Refresh(AContentChanged: Boolean);
    procedure SourceChanged(AState: TNyxSourceCommandState; const AMessage: TNyxText);
    destructor Destroy; override;
  end;

  { Native controller for the shared Nyx Studio composition, not a second set of
    editor widgets. Owns its session, theme, three renderer realizations and local
    paired store. AHost is borrowed and must outlive this controller. No compiler
    or network connection is required to run or design. Destroy before the host.

    Widget callbacks only publish portable commands and enqueue one UI repaint.
    Paint transfers independent canvas/source views to hidden parking hosts before
    replacing chrome, so no callback destroys its originating native control.
    Source remains mounted even when its optional pane is hidden. }
  TNyxNativeStudio = class
  private
    FHost: TWinControl;
    FPreviousResize: TNotifyEvent;
    FSession: TNyxStudioSession;
    FSourceCommands: TNyxSourceCommands;
    FTheme: TNyxTheme;
    FShell: TNyxDocument;
    FCodeDocument: TNyxDocument;
    FShellView: TNyxLCLRenderer;
    { Borrowed receiver registration; cancelled before any controller teardown. }
    FHierarchySubscription: INyxEventSubscription;
    FCanvasView: TNyxLCLRenderer;
    FCodeView: TNyxLCLRenderer;
    FCanvasParking: TPanel;
    FCodeParking: TPanel;
    FState: TNyxStudioViewState;
    FOutputs: TNyxOutputConfiguration;
    FStore: TNyxProjectStore;
    FProjectRevision: TNyxText;
    FBoundProject: TNyxText;
    FSavedPair: TNyxText;
    FRemotePair: TNyxText;
    FRemoteRevision: TNyxText;
    FRootRemoval: INyxRootRemoval;
    FCompilerReport: INyxCompilerReport;
    FCanvasID: TNyxText;
    FPreview: Boolean;
    FRunning: Boolean;
    FPainting: Boolean;
    FQueued: Boolean;
    FReplaceCanvas: Boolean;
    FSourceLine: Integer;
    FSourceColumn: Integer;
    FPaintCount: Integer;
    FServiceURL: TNyxText;
    FPreviewDirectory: TNyxText;
    FProjects: array of TNyxNativeStudioProject;
    FCurrentProject: TNyxNativeStudioProject;
    FPendingProject: TNyxNativeStudioProject;
    FChangingProject: Boolean;
    FRestoreProject: Boolean;
    FAgentCompilerSequence: Integer;
    FInitialPair: TNyxText;
    FOutputLoaded: Boolean;
    FOutputLoading: Boolean;
    { Field-level dirty tracking preserves edits made while profile reads are
      in flight. The accepted configuration object stays stable for every view. }
    FOutputChanged: set of 0..5;
    FOutputIdentity: TNyxBuildOutputRef;
    procedure HostResize(ASender: TObject);
    procedure PaintQueued(AData: PtrInt);
    procedure Paint;
    procedure ShellEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure HierarchyEvent(const AEvent: TNyxEventInfo);
    procedure CanvasEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure SourceEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure SourceCommandChanged(AState: TNyxSourceCommandState;
      const AMessage: TNyxText);
    procedure SaveProject;
    procedure OpenProject;
    procedure AcceptRemote;
    procedure CapturePresentation;
    { Update independent source/status controls without replacing the title field
      that is currently notifying. Guard programmatic source feedback. }
    procedure UpdateTitleAndSource;
    procedure AgentRefresh(AContentChanged: Boolean);
    procedure BeginBuild(AApplication: Boolean);
    procedure BeginCompiledPreview(AProject: TNyxNativeStudioProject);
    function CompiledPreviewCurrent(AProject: TNyxNativeStudioProject): Boolean;
    function GetCompiledPreviewProcessID: Integer;
    procedure ReadOutputs(AProject: TNyxNativeStudioProject; ABuild: Boolean);
    procedure ProjectBuildReply(AProject: TNyxNativeStudioProject);
    procedure RecordLocal;
    procedure CaptureProject;
    procedure AdmitProject(AProject: TNyxNativeStudioProject);
    procedure RestoreProjectControls;
    function CurrentBridge: TNyxStudioAgentBridge;
    function GetAgentState: TNyxStudioAgentView;
    function ComposeShell: TNyxDocument;
  protected
    { Owned adapter factory for embedded hosts with another private transport.
      Default uses asynchronous loopback HTTP. No receiver runs inside Post. }
    function CreateEditorExchange: TNyxStudioEditorExchange; virtual;
  public
    { Local directory is explicit host configuration, never design content. }
    constructor Create(AHost: TWinControl; const AProjectDirectory: TNyxText);
    destructor Destroy; override;
    { First mount runs outside widget callbacks. Subsequent refreshes are queued. }
    procedure Run;
    { Admission retains the existing pair on failure. A successful open starts
      that project's history, matching the ordinary browser paired-open contract. }
    procedure LoadProject(const APair: TNyxProjectPair);
    { Coalesce requests; any request requiring a new design supersedes retention.
      Only application UI-thread callers may access this controller or its views. }
    procedure RequestRefresh(AReplaceCanvas: Boolean = False);
    { Explicit optional machine connection. The first named workspace adopts its
      service pair; unsaved offline work refuses attachment until backed up/opened.
      Existing project contexts are immutable and cannot be silently retargeted. }
    procedure ConnectService(const ABaseURL: TNyxText;
      const AWorkspace: TNyxWorkspaceRef);
    { Switch only after local pair/draft publications are acknowledged. Each
      project keeps its mirror/bridge, history on the service and editor state. }
    procedure JumpWorkspace(const AWorkspace: TNyxWorkspaceRef);
    { Borrowed public contracts for embedding/qualification. Never free them or
      rebuild them from inside a native widget notification. }
    property Session: TNyxStudioSession read FSession;
    { Borrow the current project's command context on the UI thread. A project
      jump may change this identity; callers must never free the borrowed owner. }
    property SourceCommands: TNyxSourceCommands read FSourceCommands;
    property ShellView: TNyxLCLRenderer read FShellView;
    property CanvasView: TNyxLCLRenderer read FCanvasView;
    property CodeView: TNyxLCLRenderer read FCodeView;
    property PaintCount: Integer read FPaintCount;
    property Status: TNyxText read FState.Status;
    property Agents: TNyxStudioAgentView read GetAgentState;
    { Zero for stopped previews or browser windows that this host does not own. }
    property CompiledPreviewProcessID: Integer read GetCompiledPreviewProcessID;
  end;

implementation

uses
  StdCtrls, nyx.editing, nyx.editing.lcl, nyx.contract, nyx.source,
  nyx.studio.commands, nyx.studio.authoring, nyx.studio.inspector,
  nyx.studio.palette, nyx.studio.source, nyx.studio.diagnostics, nyx.studio.rootview,
  nyx.studio.exchange.lcl, nyx.studio.agents, nyx.studio.hierarchy, Math;

type
  TNativeHostAccess = class(TWinControl);

  { Closed native command choices decoded once from shared shell metadata. The
    ASCII transport IDs are not user text and never dictate Pascal authoring. }
  TNativeCommand = (ncUnknown,
    ncTitle,
    ncProjectName,
    ncStateName,
    ncStateValue,
    ncStateInput,
    ncBindingTarget,
    ncUndo,
    ncRedo,
    ncDelete,
    ncDuplicate,
    ncUp,
    ncDown,
    ncAddPage,
    ncComponent,
    ncReviewRoot,
    ncCancelRoot,
    ncRemoveRoot,
    ncCode,
    ncOutputs,
    ncFiles,
    ncAdvanced,
    ncDesignPanel,
    ncProjectPanel,
    ncInspectorPanel,
    ncProperties,
    ncEvents,
    ncState,
    ncBindings,
    ncPhone,
    ncDesktop,
    ncPreview,
    ncOutputNone,
    ncOutputBrowser,
    ncOutputLCL,
    ncSave,
    ncProjectSave,
    ncProjectOpen,
    ncProjectRemote,
    ncProjectCopy,
    ncAgents,
    ncAgentConnect,
    ncAgentPause,
    ncAgentAccept,
    ncAgentDisabled,
    ncAgentReadOnly,
    ncAgentEdit,
    ncWorkspaceCloseCancel,
    ncWorkspaceCloseConfirm,
    ncBuildView,
    ncBuildApplication,
    ncCompiledRun,
    ncCompiledStop);

const
  CNativeCommands: array[TNativeCommand] of TNyxText = ('',
    'project-title',
    'project-file-name',
    NyxStudioNewStateNameID,
    NyxStudioNewStateValueID,
    NyxStudioNewStateInputID,
    NyxStudioBindingTargetID,
    'action-undo',
    'action-redo',
    'action-delete',
    'action-duplicate',
    'action-up',
    'action-down',
    'action-add-page',
    'action-component',
    NyxStudioReviewRootID,
    NyxStudioCancelRootID,
    NyxStudioRemoveRootID,
    'action-code',
    'action-outputs',
    'action-import',
    'action-advanced-properties',
    'action-panel-design',
    'action-panel-project',
    'action-panel-inspector',
    NyxInspectorPropertiesID,
    NyxInspectorEventsID,
    NyxStudioStateToggleID,
    NyxStudioBindingsToggleID,
    'action-phone',
    'action-desktop',
    'action-preview',
    'output-none',
    'output-browser',
    'output-lcl',
    'action-save',
    'action-project-save',
    'action-project-open',
    'action-project-use-remote',
    'action-project-copy',
    'action-agents',
    'action-agent-connect',
    'action-agent-pause',
    'action-agent-accept',
    'action-agent-disabled',
    'action-agent-readOnly',
    'action-agent-edit',
    'action-workspace-close-cancel',
    'action-workspace-close-confirm',
    'action-build-view',
    'action-build-app',
    'action-compiled-run',
    'action-compiled-stop');

function DecodeNativeCommand(const AID: TNyxText): TNativeCommand;
var
  LCommand: TNativeCommand;
begin
  for LCommand := Low(TNativeCommand) to High(TNativeCommand) do
  begin

    if CNativeCommands[LCommand] = AID then
    begin
      Exit(LCommand);
    end;
  end;
  Result := ncUnknown;
end;

{ Native ancestry is confined to this host adapter. No widget handle enters the
  portable presentation/session model; borrowed controls are held only while
  their retained renderer realization survives this single paint. }
function InsideControl(AControl, AAncestor: TControl): Boolean;
begin
  Result := False;
  while AControl <> nil do
  begin

    if AControl = AAncestor then
    begin
      Exit(True);
    end;
    AControl := AControl.Parent;
  end;
end;

procedure TNyxNativeStudioProject.PollBuild(ASender: TObject);
begin

  if (Owner = nil) or not Owner.FRunning or (BuildStage <> nbsPolling) or
    BuildStatusPending or not Bridge.Enabled or not Bridge.State.Connected or
    Bridge.State.Conflict then
  begin
    Exit;
  end;
  { A timer only queues a bounded private operation. Compiler workers remain
    service-owned, and the bridge fixes the project's identity on every packet. }
  BuildStatusPending := True;
  try
    Bridge.BuildStatus(BuildJob);
  except
    BuildStatusPending := False;
    raise;
  end;
end;

procedure TNyxNativeStudioProject.Refresh(AContentChanged: Boolean);
begin

  if Owner <> nil then
  begin
    Owner.ProjectBuildReply(Self);
  end;

  if (Owner <> nil) and (Owner.FPendingProject = Self) then
  begin
    Owner.AdmitProject(Self);
  end;

  if (Owner <> nil) and (Owner.FCurrentProject = Self) then
  begin
    Owner.AgentRefresh(AContentChanged);
  end;
end;

procedure TNyxNativeStudioProject.PreviewPrepared(ASucceeded: Boolean; const AError: TNyxText);
begin

  if Owner = nil then
  begin
    Exit;
  end;
  try

    if not ASucceeded then
    begin
      raise ENyxModel.Create(AError);
    end;
    { A successful download still requires another exact-context currentness
      query. This callback never authorizes execution from an older snapshot. }
    BuildStage := nbsPreviewActivate;
    Bridge.BuildStatus(BuildJob);
  except
    on LException: Exception do
    begin
      BuildStage := nbsTerminal;
      BuildMessage := LException.Message;
      State.Status := BuildMessage;

      if Owner.FCurrentProject = Self then
      begin
        Owner.FState.Status := BuildMessage;
        Owner.RequestRefresh;
      end;
    end;
  end;
end;

procedure TNyxNativeStudioProject.SourceChanged(AState: TNyxSourceCommandState;
  const AMessage: TNyxText);
begin
  State.Status := AMessage;
  State.SourceStatus := AMessage;

  if (AState = nssApplied) and (Bridge <> nil) then
  begin
    Bridge.RecordLocal;
  end;

  if (Owner <> nil) and (Owner.FCurrentProject = Self) then
  begin
    Owner.SourceCommandChanged(AState, AMessage);
  end;
end;

destructor TNyxNativeStudioProject.Destroy;
begin
  Owner := nil;
  SourceCommands.Free;
  FreeAndNil(BuildTimer);
  CompiledPreview.Free;
  Bridge.Free;
  Session.Free;
  inherited Destroy;
end;


constructor TNyxNativeStudio.Create(AHost: TWinControl;
  const AProjectDirectory: TNyxText);
begin
  inherited Create;

  if AHost = nil then
  begin
    raise ENyxModel.Create('Native Studio requires a host');
  end;
  FHost := AHost;
  FPreviousResize := TNativeHostAccess(FHost).OnResize;
  FSession := TNyxStudioSession.Create;
  FSourceCommands := TNyxSourceCommands.Create(FSession, SourceCommandChanged);
  FTheme := TNyxTheme.Create;
  FOutputs := TNyxOutputConfiguration.Create;
  FStore := TNyxProjectStore.Create(AProjectDirectory);
  FPreviewDirectory := IncludeTrailingPathDelimiter(ExpandFileName(AProjectDirectory)) +
    'compiled-previews';
  FState := DefaultNyxStudioViewState;
  FState.CodePresentation := ncpHosted;
  FState.Outputs := FOutputs;
  FShellView := TNyxLCLRenderer.Create(FTheme);
  FCanvasView := TNyxLCLRenderer.Create(FTheme);
  FCodeView := TNyxLCLRenderer.Create(FTheme);
  FShellView.OnEvent := ShellEvent;
  FHierarchySubscription := SubscribeNyxStudioHierarchy(FShellView.Events, HierarchyEvent);
  FCanvasView.OnEvent := CanvasEvent;
  FCodeView.OnEvent := SourceEvent;
  FCanvasParking := TPanel.Create(nil);
  FCanvasParking.Parent := FHost;
  FCanvasParking.Visible := False;
  FCodeParking := TPanel.Create(nil);
  FCodeParking.Parent := FHost;
  FCodeParking.Visible := False;
  TNativeHostAccess(FHost).OnResize := HostResize;
  FSavedPair := EncodeNyxProject(FSession.ProjectSnapshot);
  FInitialPair := FSavedPair;
end;

destructor TNyxNativeStudio.Destroy;
var
  LIndex: Integer;
begin
  FRunning := False;

  if FHierarchySubscription <> nil then
  begin
    FHierarchySubscription.Cancel;
    FHierarchySubscription := nil;
  end;
  { Remove this object's queued callbacks before any view/session lifetime ends. }
  Application.RemoveAsyncCalls(Self);

  if FSourceCommands <> nil then
  begin
    FSourceCommands.Detach;
  end;
  { Detach every context before joining even one worker. Waiting for a native
    thread may dispatch host synchronization; no other context may consult the
    current editor after its views or an earlier context have been released. }
  for LIndex := 0 to High(FProjects) do
  begin
    FProjects[LIndex].Owner := nil;
    FProjects[LIndex].SourceCommands.Detach;
    FreeAndNil(FProjects[LIndex].BuildTimer);

    if FProjects[LIndex].CompiledPreview <> nil then
    begin
      FProjects[LIndex].CompiledPreview.Cancel;
    end;
    FProjects[LIndex].Bridge.Pause;
  end;
  FCurrentProject := nil;
  FPendingProject := nil;

  if FHost <> nil then
  begin
    TNativeHostAccess(FHost).OnResize := FPreviousResize;
  end;
  FCanvasView.Free;
  FCodeView.Free;
  FShellView.Free;
  FCanvasParking.Free;
  FCodeParking.Free;
  FCodeDocument.Free;
  FShell.Free;
  FRootRemoval := nil;
  FCompilerReport := nil;

  if Length(FProjects) = 0 then
  begin
    FSourceCommands.Free;
    FSession.Free;
  end
  else
  begin
    for LIndex := 0 to High(FProjects) do
    begin
      FProjects[LIndex].Free;
    end;
  end;
  FSession := nil;
  FSourceCommands := nil;
  FStore.Free;
  FOutputs.Free;
  FTheme.Free;
  inherited Destroy;
end;

procedure TNyxNativeStudio.Run;
begin

  if FRunning then
  begin
    Exit;
  end;
  FRunning := True;
  FReplaceCanvas := True;
  Paint;
end;

function TNyxNativeStudio.CurrentBridge: TNyxStudioAgentBridge;
begin
  Result := nil;

  if FCurrentProject <> nil then
  begin
    Result := FCurrentProject.Bridge;
  end;
end;

function TNyxNativeStudio.GetAgentState: TNyxStudioAgentView;
begin
  Result := DefaultNyxStudioAgentView;

  if CurrentBridge <> nil then
  begin
    Result := CurrentBridge.State;
  end
  else
  begin
    Result.Status := 'Local project / service connection is optional';
  end;
end;

procedure TNyxNativeStudio.RecordLocal;
begin

  if CurrentBridge <> nil then
  begin
    CurrentBridge.RecordLocal;
  end;
end;

procedure TNyxNativeStudio.AgentRefresh(AContentChanged: Boolean);
var
  LState: TNyxStudioAgentView;
begin
  LState := GetAgentState;

  if FPendingProject <> nil then
  begin
    AdmitProject(FPendingProject);
    LState := GetAgentState;
  end;

  if (LState.Compiler.Kind = ndObject) and
    (LState.Compiler.Field('sequence').AsInteger <> FAgentCompilerSequence) and
    CurrentBridge.SourceSynchronized then
  begin
    FAgentCompilerSequence := LState.Compiler.Field('sequence').AsInteger;

    if LState.Compiler.Field('total').AsInteger = 0 then
    begin
      FCompilerReport := nil;
    end
    else if LState.Compiler.Field('acceptedSource').AsBoolean then
    begin
      FCompilerReport := DecodeNyxCompilerReport(NyxObject([
        NyxField('version', NyxData(1)), NyxField('source', NyxData(FSession.Source)),
        NyxField('items', LState.Compiler.Field('items'))]).ToJSON);
    end;
  end;
  FState.Status := LState.Status;

  if (FCurrentProject <> nil) and (FCurrentProject.BuildMessage <> '') then
  begin
    FState.Status := FCurrentProject.BuildMessage;
  end;

  if LState.Conflict then
  begin
    FState.AgentsVisible := True;
  end;
  RequestRefresh(AContentChanged);
end;

function TNyxNativeStudio.CreateEditorExchange: TNyxStudioEditorExchange;
begin
  Result := TNyxLCLEditorExchange.Create(FServiceURL);
end;

procedure TNyxNativeStudio.ReadOutputs(AProject: TNyxNativeStudioProject; ABuild: Boolean);
begin

  if FOutputLoading then
  begin
    raise ENyxModel.Create('Output configuration is synchronizing; local settings are retained');
  end;
  FOutputLoading := True;
  AProject.BuildRequested := ABuild;
  AProject.BuildStage := nbsProfile;
  AProject.BuildReplySequence := AProject.Bridge.State.BuildReplySequence;
  AProject.BuildProfileSent := '';
  try

    if FOutputLoaded and (FOutputChanged <> []) then
    begin
      AProject.BuildProfileSent := FOutputs.Encode;
      AProject.Bridge.SaveCompilerProfile(FOutputs, FOutputIdentity);
    end
    else
    begin
      AProject.Bridge.CompilerProfile;
    end;
  except
    FOutputLoading := False;
    AProject.BuildStage := nbsIdle;
    raise;
  end;
end;

procedure TNyxNativeStudio.BeginBuild(AApplication: Boolean);
var
  LIndex: Integer;
  LProject: TNyxNativeStudioProject;
begin

  if (CurrentBridge = nil) or not CurrentBridge.State.CanBuild then
  begin
    raise ENyxModel.Create('Native build requests are unavailable');
  end;

  if FState.OutputTarget = '' then
  begin
    FState.OutputVisible := True;
    raise ENyxModel.Create('Choose an output in Target / output before building');
  end;

  if FOutputLoading then
  begin
    raise ENyxModel.Create('Output configuration is synchronizing; build when ready');
  end;

  if not CurrentBridge.SourceSynchronized or FSession.ProjectSnapshot.Pending then
  begin
    raise ENyxModel.Create('Apply or restore Pascal and finish project synchronization before building');
  end;
  LProject := FCurrentProject;

  if LProject.BuildStage in [nbsProfile, nbsOutputs, nbsRequest, nbsPolling,
    nbsPreviewInspect, nbsPreviewDownload, nbsPreviewActivate] then
  begin
    raise ENyxModel.Create('A build is already active for this project');
  end;
  LProject.BuildTarget := ParseNyxBuildTarget(FState.OutputTarget);
  LProject.BuildScope := bsApplication;
  LProject.BuildRoot := Default(TNyxBuildRootRef);

  if not AApplication then
  begin
    LProject.BuildScope := bsView;
    for LIndex := 0 to FSession.Document.ComponentCount - 1 do
    begin

      if FSession.Document.Components[LIndex].ID = FSession.ActiveViewID then
      begin
        LProject.BuildScope := bsReusable;
      end;
    end;
    LProject.BuildRoot := NyxBuildRoot(FSession.ActiveViewID);
  end;
  LProject.BuildPair := EncodeNyxProject(FSession.ProjectSnapshot);
  LProject.BuildResult := NyxNull;
  LProject.BuildArtifact := '';
  LProject.BuildMessage := 'Checking compiler output';
  FState.Status := LProject.BuildMessage;
  ReadOutputs(LProject, True);
end;

function TNyxNativeStudio.CompiledPreviewCurrent(AProject: TNyxNativeStudioProject): Boolean;
begin
  Result := (AProject <> nil) and (AProject.BuildArtifact <> '') and
    (AProject.BuildResult.Kind = ndObject) and AProject.Bridge.SourceSynchronized and
    (EncodeNyxProject(AProject.Session.ProjectSnapshot) = AProject.BuildPair) and
    (FOutputs.Encode = AProject.BuildOutputSnapshot);
end;

function TNyxNativeStudio.GetCompiledPreviewProcessID: Integer;
begin
  Result := 0;

  if (FCurrentProject <> nil) and (FCurrentProject.CompiledPreview <> nil) then
  begin
    Result := FCurrentProject.CompiledPreview.ProcessID;
  end;
end;

procedure TNyxNativeStudio.BeginCompiledPreview(AProject: TNyxNativeStudioProject);
begin

  if not CompiledPreviewCurrent(AProject) then
  begin
    raise ENyxModel.Create('Compile the current accepted project and output before running a preview');
  end;

  if AProject.BuildStage <> nbsTerminal then
  begin
    raise ENyxModel.Create('Compiler or preview preparation is still running');
  end;
  AProject.BuildStage := nbsPreviewInspect;
  AProject.BuildMessage := 'Preparing compiled preview';
  AProject.Bridge.BuildStatus(AProject.BuildJob);
end;

procedure TNyxNativeStudio.ProjectBuildReply(AProject: TNyxNativeStudioProject);
var
  LView: TNyxStudioAgentView;
  LReply: TNyxDataValue;
  LProfile: TNyxOutputConfiguration;
  LIndex: Integer;
  LReady: Boolean;
  LIssue: TNyxText;
  LRequest: INyxCompilerRequest;
  LIdentity: TGUID;
  LCurrent: Boolean;
begin
  LView := AProject.Bridge.State;

  if LView.BuildReplySequence = AProject.BuildReplySequence then
  begin
    Exit;
  end;
  AProject.BuildReplySequence := LView.BuildReplySequence;
  LReply := LView.BuildReply;
  try

    if NyxAgentHas(LReply, 'state') and (LReply.Field('state').AsText = 'rejected') then
    begin
      raise ENyxModel.Create(LReply.Field('error').AsText);
    end;
    case AProject.BuildStage of
      nbsProfile:
        begin
          FOutputLoading := False;
          FOutputIdentity := NyxBuildOutput(LReply.Field('outputID').AsText);
          LProfile := TNyxOutputConfiguration.Decode(LReply.Field('profile').ToJSON);
          try

            if AProject.BuildProfileSent = '' then
            begin
              { A delayed read may fill clean fields; it must not erase any field
                the operator has edited meanwhile, or replace borrowed objects. }
              for LIndex := 0 to High(NyxOutputFields) do
              begin

                if not (LIndex in FOutputChanged) then
                begin
                  FOutputs.SetField(NyxOutputFields[LIndex], LProfile.Field(NyxOutputFields[LIndex]));
                end;
              end;
            end
            else if FOutputs.Encode = AProject.BuildProfileSent then
            begin
              FOutputChanged := [];
            end;
          finally
            LProfile.Free;
          end;
          FOutputLoaded := True;

          if FOutputChanged <> [] then
          begin
            ReadOutputs(AProject, AProject.BuildRequested);
          end
          else if AProject.BuildRequested then
          begin
            AProject.BuildStage := nbsOutputs;
            AProject.BuildOutputSnapshot := FOutputs.Encode;
            AProject.Bridge.CompilerOutputs;
          end
          else
          begin
            AProject.BuildStage := nbsIdle;
            AProject.BuildMessage := 'Output configuration loaded';
          end;
        end;
      nbsOutputs:
        begin
          { Recheck the mirror after preflight. Edits/navigation during any
            asynchronous admission must never silently compile a different pair. }
          if not AProject.Bridge.SourceSynchronized or
            (EncodeNyxProject(AProject.Session.ProjectSnapshot) <> AProject.BuildPair) then
          begin
            raise ENyxModel.Create('Project changed during compiler checks; build again when ready');
          end;

          if FOutputs.Encode <> AProject.BuildOutputSnapshot then
          begin
            raise ENyxModel.Create('Output settings changed during compiler checks; local settings retained');
          end;

          if LReply.Field('outputID').AsText <> FOutputIdentity.ID then
          begin
            FOutputLoaded := False;
            raise ENyxModel.Create('Service output configuration changed; inspect outputs before rebuilding');
          end;
          LReady := False;
          LIssue := 'Requested output is unavailable';
          for LIndex := 0 to LReply.Field('outputs').Count - 1 do
          begin

            if LReply.Field('outputs').Item(LIndex).Field('target').AsText =
              NyxBuildTargetName(AProject.BuildTarget) then
            begin
              LReady := LReply.Field('outputs').Item(LIndex).Field('ready').AsBoolean;
              LIssue := LReply.Field('outputs').Item(LIndex).Field('issue').AsText;
            end;
          end;

          if not LReady then
          begin
            raise ENyxModel.Create(LIssue);
          end;
          CreateGUID(LIdentity);
          LRequest := NewNyxCompilerRequest.Target(AProject.BuildTarget)
            .Scope(AProject.BuildScope).AtRevision(LView.Revision)
            .Output(NyxBuildOutput(LReply.Field('outputID').AsText))
            .Operation(NyxBuildOperation('studio-' + Copy(GUIDToString(LIdentity), 2, 36)));

          if AProject.BuildScope <> bsApplication then
          begin
            LRequest.Root(AProject.BuildRoot);
          end;
          AProject.BuildStage := nbsRequest;
          AProject.Bridge.RequestBuild(LRequest);
        end;
      nbsRequest:
        begin
          AProject.BuildJob := NyxBuildJob(LReply.Field('job').AsText);
          AProject.BuildStage := nbsPolling;
          AProject.BuildMessage := 'Compiling ' + NyxBuildScopeName(AProject.BuildScope);

          if AProject.BuildTimer = nil then
          begin
            AProject.BuildTimer := TTimer.Create(nil);
            AProject.BuildTimer.Enabled := False;
            AProject.BuildTimer.Interval := 300;
            AProject.BuildTimer.OnTimer := AProject.PollBuild;
          end;
          AProject.BuildTimer.Enabled := True;
        end;
      nbsPolling:
        begin
          AProject.BuildStatusPending := False;

          if LReply.Field('job').AsText <> AProject.BuildJob.ID then
          begin
            raise ENyxModel.Create('Compiler reply belongs to another job; project retained');
          end;
          AProject.BuildResult := LReply.Copy;

          if (LReply.Field('state').AsText = 'succeeded') or
            (LReply.Field('state').AsText = 'failed') then
          begin
            AProject.BuildStage := nbsTerminal;
            AProject.BuildTimer.Enabled := False;
            LCurrent := LReply.Field('currentSource').AsBoolean and
              LReply.Field('currentOutput').AsBoolean and AProject.Bridge.SourceSynchronized and
              (EncodeNyxProject(AProject.Session.ProjectSnapshot) = AProject.BuildPair) and
              (FOutputs.Encode = AProject.BuildOutputSnapshot);

            if not LCurrent then
            begin
              AProject.BuildMessage := 'Build finished / project or output changed; result retained for review';
            end
            else if LReply.Field('state').AsText = 'succeeded' then
            begin
              AProject.BuildArtifact := LReply.Field('artifact').AsText;
              AProject.BuildMessage := 'Build complete / compiled artifact available';

              if (AProject.CompiledPreview <> nil) and AProject.CompiledPreview.Running and
                (FCurrentProject = AProject) then
              begin
                BeginCompiledPreview(AProject);
              end;
            end
            else
            begin
              AProject.BuildMessage := LReply.Field('error').AsText;
              AProject.State.CodeVisible := True;

              if FCurrentProject = AProject then
              begin
                FState.CodeVisible := True;
                FState.Panel := nspDesign;
              end;
            end;
          end;
        end;
      nbsPreviewInspect, nbsPreviewActivate:
        begin

          if not LReply.Field('currentSource').AsBoolean or
            not LReply.Field('currentOutput').AsBoolean then
          begin
            { A definitive service refusal retires the stale Run affordance;
              the previously running owned preview remains available to Stop. }
            AProject.BuildArtifact := '';
          end;

          if (LReply.Field('job').AsText <> AProject.BuildJob.ID) or
            not CompiledPreviewCurrent(AProject) then
          begin
            raise ENyxModel.Create('Project or output changed; compiled preview activation refused');
          end;
          { Wire admission checks service currentness and the exact immutable
            artifact manifest; local pair/profile admission is repeated too. }
          AdmitNyxCompiledArtifact(LReply);
          AProject.BuildResult := LReply.Copy;

          if AProject.BuildStage = nbsPreviewInspect then
          begin

            if AProject.CompiledPreview = nil then
            begin
              AProject.CompiledPreview := TNyxLCLCompiledPreview.Create(FServiceURL, FPreviewDirectory);
            end;
            AProject.BuildStage := nbsPreviewDownload;
            AProject.CompiledPreview.Prepare(AdmitNyxCompiledArtifact(LReply), AProject.PreviewPrepared);
          end
          else
          begin
            AProject.BuildStage := nbsTerminal;

            if FCurrentProject = AProject then
            begin
              AProject.CompiledPreview.Launch;
              AProject.BuildMessage := 'Compiled preview running';
            end
            else
            begin
              AProject.BuildMessage := 'Compiled preview ready / return to this project to run';
            end;
          end;
        end;
      nbsIdle, nbsPreviewDownload, nbsTerminal:
        begin
          { No reply is adopted outside its explicitly owned pending stage. }
        end;
    end;
  except
    on LException: Exception do
    begin

      if AProject.BuildStage = nbsProfile then
      begin
        FOutputLoading := False;
        FOutputLoaded := False;
      end;
      AProject.BuildStage := nbsTerminal;
      AProject.BuildMessage := LException.Message;

      if AProject.BuildTimer <> nil then
      begin
        AProject.BuildTimer.Enabled := False;
      end;
    end;
  end;
  AProject.State.Status := AProject.BuildMessage;

  if FCurrentProject = AProject then
  begin
    FState.Status := AProject.BuildMessage;
    RequestRefresh;
  end;
end;

procedure TNyxNativeStudio.ConnectService(const ABaseURL: TNyxText;
  const AWorkspace: TNyxWorkspaceRef);
var
  LProject: TNyxNativeStudioProject;
begin

  if FCurrentProject <> nil then
  begin

    if (ABaseURL <> FServiceURL) or (AWorkspace.ID <> FCurrentProject.Reference.ID) then
    begin
      raise ENyxModel.Create('Use project navigation; an existing editor connection cannot be retargeted');
    end;
    CurrentBridge.Connect;
    Exit;
  end;
  LProject := TNyxNativeStudioProject.Create;
  try
    LProject.Owner := Self;
    LProject.Reference := AWorkspace;
    LProject.State := FState;
    LProject.Session := FSession;
    FServiceURL := ABaseURL;
    LProject.Bridge := TNyxStudioAgentBridge.Create(FSession, LProject.Refresh,
      AWorkspace, CreateEditorExchange);
    SetLength(FProjects, 1);
    FProjects[0] := LProject;
    FCurrentProject := LProject;
    LProject.SourceCommands := FSourceCommands;
    FSourceCommands.OnChanged := LProject.SourceChanged;
    LProject := nil;
    FServiceURL := ABaseURL;
    CurrentBridge.Connect(EncodeNyxProject(FSession.ProjectSnapshot) <> FInitialPair);
    FState.AgentsVisible := True;
    RequestRefresh;
  finally

    if LProject <> nil then
    begin
      { The controller still owns its original offline session on failed attach. }
      LProject.Session := nil;
      LProject.Free;
    end;
  end;
end;

function NativeScrollTop(ARenderer: TNyxLCLRenderer; const AID: TNyxText): Integer;
var
  LControl: TControl;
begin
  Result := 0;
  LControl := ARenderer.ControlFor(AID);

  if LControl is TScrollBox then
  begin
    Result := TScrollBox(LControl).VertScrollBar.Position;
  end;
end;

procedure TNyxNativeStudio.CaptureProject;
var
  LInput: TWinControl;
  LSelection: TNyxTextSelection;
  LViewport: TNyxViewportSnapshot;
begin
  CapturePresentation;
  FCurrentProject.State := FState;
  FCurrentProject.Preview := FPreview;
  FCurrentProject.SavedPair := FSavedPair;
  FCurrentProject.BoundProject := FBoundProject;
  FCurrentProject.ProjectRevision := FProjectRevision;
  FCurrentProject.RemotePair := FRemotePair;
  FCurrentProject.RemoteRevision := FRemoteRevision;

  if FCodeView.Root <> nil then
  begin
    LInput := TWinControl(FCodeView.InputFor('studio-code'));
    LSelection := CaptureNyxLCLSelection(LInput);
    FCurrentProject.CodeStart := LSelection.Start;
    FCurrentProject.CodeEnd := LSelection.Finish;
    FCurrentProject.CodeFocused := Screen.ActiveControl = LInput;
  end;
  FCurrentProject.LeftTop := NativeScrollTop(FShellView, 'studio-left');
  FCurrentProject.RightTop := NativeScrollTop(FShellView, 'studio-right');

  if FCanvasView.Root <> nil then
  begin
    LViewport := FCanvasView.ViewViewport;
    FCurrentProject.CanvasTop := Trunc(LViewport.Y.Position);
    FCurrentProject.CanvasLeft := Trunc(LViewport.X.Position);
  end;
end;

procedure TNyxNativeStudio.RestoreProjectControls;
var
  LInput: TWinControl;
  LLength: Integer;
  LStart: Integer;
  LEnd: Integer;
  LControl: TControl;
begin

  if not FRestoreProject or (FCurrentProject = nil) then
  begin
    Exit;
  end;
  FRestoreProject := False;

  if FCodeView.Root <> nil then
  begin
    LInput := TWinControl(FCodeView.InputFor('studio-code'));
    LLength := NyxTextScalarCount(NyxLCLInputText(LInput));
    LStart := Min(FCurrentProject.CodeStart, LLength);
    LEnd := Min(FCurrentProject.CodeEnd, LLength);
    SelectNyxLCLText(LInput, NyxTextSelection(NyxLCLInputText(LInput), LStart, LEnd));

    if FCurrentProject.CodeFocused and LInput.CanFocus then
    begin
      LInput.SetFocus;
    end;
  end;
  LControl := FShellView.ControlFor('studio-left');

  if LControl is TScrollBox then
  begin
    TScrollBox(LControl).VertScrollBar.Position := FCurrentProject.LeftTop;
  end;
  LControl := FShellView.ControlFor('studio-right');

  if LControl is TScrollBox then
  begin
    TScrollBox(LControl).VertScrollBar.Position := FCurrentProject.RightTop;
  end;

  if FCanvasView.Root <> nil then
  begin
    FCanvasView.ScrollView(FCurrentProject.CanvasLeft, FCurrentProject.CanvasTop);
  end;
end;

procedure TNyxNativeStudio.JumpWorkspace(const AWorkspace: TNyxWorkspaceRef);
var
  LIndex: Integer;
  LProject: TNyxNativeStudioProject;
  LKnown: Boolean;
  LWorkspaces: TNyxDataValue;
begin

  if FCurrentProject = nil then
  begin
    raise ENyxModel.Create('Connect the editor before project navigation');
  end;

  if AWorkspace.ID = FCurrentProject.Reference.ID then
  begin
    Exit;
  end;
  RecordLocal;

  if not CurrentBridge.CanSwitchWorkspace or FPainting then
  begin
    raise ENyxModel.Create('Finish synchronization or resolve the current conflict before switching projects');
  end;
  LKnown := AWorkspace.ID = '';
  LWorkspaces := CurrentBridge.State.Workspaces;
  for LIndex := 0 to LWorkspaces.Count - 1 do
  begin
    LKnown := LKnown or (LWorkspaces.Item(LIndex).Field('workspace').AsText = AWorkspace.ID);
  end;

  if not LKnown then
  begin
    raise ENyxModel.Create('The requested project is not available in this service');
  end;
  LProject := nil;
  for LIndex := 0 to High(FProjects) do
  begin

    if FProjects[LIndex].Reference.ID = AWorkspace.ID then
    begin
      LProject := FProjects[LIndex];
    end;
  end;

  if LProject = nil then
  begin
    LProject := TNyxNativeStudioProject.Create;
    try
      LProject.Owner := Self;
      LProject.Reference := AWorkspace;
      LProject.Session := TNyxStudioSession.Create;
      LProject.SourceCommands := TNyxSourceCommands.Create(LProject.Session,
        LProject.SourceChanged);
      LProject.State := DefaultNyxStudioViewState;
      LProject.State.CodePresentation := ncpHosted;
      LProject.State.Outputs := FOutputs;
      LProject.SavedPair := EncodeNyxProject(LProject.Session.ProjectSnapshot);
      LProject.Bridge := TNyxStudioAgentBridge.Create(LProject.Session,
        LProject.Refresh, AWorkspace, CreateEditorExchange);
      SetLength(FProjects, Length(FProjects) + 1);
      FProjects[High(FProjects)] := LProject;
    except
      LProject.Free;
      raise;
    end;
  end;
  FPendingProject := LProject;

  if not LProject.Bridge.Enabled then
  begin
    LProject.Bridge.Connect;
  end;
  AdmitProject(LProject);
end;

procedure TNyxNativeStudio.AdmitProject(AProject: TNyxNativeStudioProject);
var
  LState: TNyxStudioAgentView;
begin

  if FPendingProject <> AProject then
  begin
    Exit;
  end;
  LState := AProject.Bridge.State;

  if LState.Conflict then
  begin
    FPendingProject := nil;
    FState.Status := 'Project connection refused / current project retained';
    RequestRefresh;
    Exit;
  end;

  if not LState.Connected or not CurrentBridge.CanSwitchWorkspace then
  begin
    Exit;
  end;
  CaptureProject;
  { View retirement is deferred to Paint. The action widget remains alive until
    its callback returns, and every old request keeps its immutable context. }
  FCurrentProject := AProject;
  FPendingProject := nil;
  FChangingProject := True;
  FSession := AProject.Session;
  FSourceCommands := AProject.SourceCommands;
  FState := AProject.State;
  FPreview := AProject.Preview;
  FSavedPair := AProject.SavedPair;
  FBoundProject := AProject.BoundProject;
  FProjectRevision := AProject.ProjectRevision;
  FRemotePair := AProject.RemotePair;
  FRemoteRevision := AProject.RemoteRevision;
  FCanvasID := '';
  FRestoreProject := True;
  FSourceLine := 0;
  FSourceColumn := 0;
  FAgentCompilerSequence := 0;
  FCompilerReport := nil;
  FRootRemoval := nil;

  RequestRefresh(True);
end;

procedure TNyxNativeStudio.LoadProject(const APair: TNyxProjectPair);
begin
  FSession.LoadProject(APair);
  FBoundProject := '';
  FProjectRevision := '';
  FSavedPair := EncodeNyxProject(FSession.ProjectSnapshot);
  FRootRemoval := nil;
  FCompilerReport := nil;
  FState.CallbackRemoval.Pending := False;
  FState.Status := 'Project opened';
  RequestRefresh(True);
end;

procedure TNyxNativeStudio.RequestRefresh(AReplaceCanvas: Boolean);
begin
  FReplaceCanvas := FReplaceCanvas or AReplaceCanvas;

  if not FRunning or FQueued then
  begin
    Exit;
  end;
  FQueued := True;
  Application.QueueAsyncCall(PaintQueued, 0);
end;

procedure TNyxNativeStudio.HostResize(ASender: TObject);
begin

  if Assigned(FPreviousResize) then
  begin
    FPreviousResize(ASender);
  end;
  RequestRefresh;
end;

procedure TNyxNativeStudio.PaintQueued(AData: PtrInt);
begin
  FQueued := False;

  if not FRunning then
  begin
    Exit;
  end;
  try
    Paint;
  except
    on LException: Exception do
    begin
      FState.Status := LException.Message;
      { Report a paint failure through the surviving shell. Never let LCL open
        a modal exception box or retry the same failed candidate indefinitely. }

      if (FShellView.Root <> nil) and (FShellView.Root.Find('studio-status') <> nil) then
      begin
        FShellView.Root.Find('studio-status').Configure.Text(FState.Status).Done;
        FShellView.Sync;
      end;
    end;
  end;
end;

procedure TNyxNativeStudio.CapturePresentation;
var
  LNode: TNyxNode;
begin

  if FShellView.Root = nil then
  begin
    Exit;
  end;
  LNode := FShellView.Root.Find('studio-split');

  if LNode <> nil then
  begin
    FState.CanvasPercent := StrToIntDef(LNode.Prop('split-position'), FState.CanvasPercent);
  end;
  LNode := FShellView.Root.Find(NyxStudioNewStateNameID);

  if LNode <> nil then
  begin
    FState.NewStateName := LNode.Prop('value');
    FState.NewStateValue := FShellView.Root.Find(NyxStudioNewStateValueID).Prop('value');
  end;
end;

function TNyxNativeStudio.ComposeShell: TNyxDocument;
begin
  FState.Compact := FHost.ClientWidth < 900;
  FState.RootRemoval := NyxNull;
  FState.Agents := GetAgentState;
  FState.CompiledPreviewAvailable := CompiledPreviewCurrent(FCurrentProject);
  FState.CompiledPreviewRunning := (FCurrentProject <> nil) and
    (FCurrentProject.CompiledPreview <> nil) and FCurrentProject.CompiledPreview.Running;

  if FRootRemoval <> nil then
  begin
    FState.RootRemoval := FRootRemoval.Inspect;
  end;
  Result := BuildNyxStudioView(FSession, FState, FCompilerReport);
  Result.Pages[0].Configure.Height(FHost.ClientHeight).Done;
end;

procedure TNyxNativeStudio.Paint;
var
  LShell: TNyxDocument;
  LCanvasHost: TWinControl;
  LCodeHost: TWinControl;
  LOldCanvasHost: TWinControl;
  LOldCodeHost: TWinControl;
  LFocus: TWinControl;
  LSelection: TNyxTextSelection;
  LRetainFocus: Boolean;
  LCanvasFocus: Boolean;
  LSameView: Boolean;
begin

  if FPainting then
  begin
    RequestRefresh;
    Exit;
  end;
  FPainting := True;
  LShell := nil;
  try
    CapturePresentation;
    LFocus := Screen.ActiveControl;
    LRetainFocus := (LFocus <> nil) and
      ((FCodeView.Root <> nil) and (LFocus = FCodeView.InputFor('studio-code')));
    LCanvasFocus := (LFocus <> nil) and (FCanvasView.Root <> nil) and
      not FReplaceCanvas and (FCanvasID = FSession.ActiveViewID) and
      InsideControl(LFocus, FCanvasView.ControlFor(FCanvasView.Root.ID));
    LSelection := Default(TNyxTextSelection);

    if LRetainFocus or LCanvasFocus then
    begin
      LSelection := CaptureNyxLCLSelection(LFocus);
    end;
    LSameView := FCanvasID = FSession.ActiveViewID;
    LOldCanvasHost := nil;
    LOldCodeHost := nil;

    if FCanvasView.Root <> nil then
    begin
      LOldCanvasHost := FCanvasView.ControlFor(FCanvasView.Root.ID).Parent.Parent;
      FCanvasView.MoveHost(FCanvasParking);
    end;

    if FCodeView.Root <> nil then
    begin
      LOldCodeHost := FCodeView.ControlFor('studio-code').Parent.Parent;
      FCodeView.MoveHost(FCodeParking);
    end;
    LShell := ComposeShell;
    try
      FShellView.Render(LShell, LShell.Pages[0], FHost);
    except
      { Candidate shell admission retains old chrome. Put its borrowed views
        back before surfacing the refusal; do not leave accepted inputs parked. }

      if LOldCanvasHost <> nil then
      begin
        FCanvasView.MoveHost(LOldCanvasHost);
      end;

      if LOldCodeHost <> nil then
      begin
        FCodeView.MoveHost(LOldCodeHost);
      end;
      raise;
    end;
    FShell.Free;
    FShell := LShell;
    LShell := nil;
    { Compact Project/Design panels do not mount the Inspector hierarchy. Restore
      selection only when this shell actually owns the public tree binding. }

    if FShell.Find(NyxStudioHierarchyID) <> nil then
    begin
      SelectNyxStudioHierarchy(FShellView.CollectionView(NyxStudioHierarchyID), FSession);
    end;
    LCanvasHost := nil;
    LCodeHost := nil;

    if FShell.Find('studio-canvas') <> nil then
    begin
      LCanvasHost := TWinControl(FShellView.ControlFor('studio-canvas'));
    end;

    if (LCanvasHost <> nil) and (FSession.ActiveView <> nil) then
    begin

      if (FCanvasView.Root <> nil) and LSameView and not FReplaceCanvas then
      begin
        FCanvasView.MoveHost(LCanvasHost);
      end
      else
      begin
        FCanvasView.Render(FSession.Document, FSession.ActiveView, LCanvasHost, not FPreview);
      end;
      FCanvasView.Select(FSession.SelectedID);
      FCanvasID := FSession.ActiveViewID;
    end
    else if not LSameView or FReplaceCanvas then
    begin
      { A hidden design changed; retire the old realization instead of claiming
        that stale controls represent the accepted document on the next switch. }
      FCanvasView.Unmount;
    end;

    if LCanvasFocus and (LCanvasHost <> nil) and LFocus.CanFocus then
    begin
      LFocus.SetFocus;

      if LSelection.Defined then
      begin
        SelectNyxLCLText(LFocus, LSelection);
      end;
    end;

    if FShell.Find('studio-code-host') <> nil then
    begin
      LCodeHost := TWinControl(FShellView.ControlFor('studio-code-host'));
    end;

    if LCodeHost <> nil then
    begin

      if FCodeView.Root = nil then
      begin
        FreeAndNil(FCodeDocument);
        FCodeDocument := TNyxDocument.Create;
        FCodeDocument.AddPage(NewNyxStudioCodeEditor(FSession.DraftSource));
        FCodeDocument.Pages[0].Configure.Height(LCodeHost.ClientHeight).Done;
        FCodeView.Render(FCodeDocument, FCodeDocument.Pages[0], LCodeHost);
      end
      else
      begin
        FCodeView.Root.Configure.Value(FSession.DraftSource).Height(LCodeHost.ClientHeight).Done;
        FCodeView.Sync;
        FCodeView.MoveHost(LCodeHost);
      end;

      if FSourceLine > 0 then
      begin
        FCodeView.NavigateCodeLine('studio-code', FSourceLine, FSourceColumn);
      end
      else if LRetainFocus and LFocus.CanFocus then
      begin
        LFocus.SetFocus;

        if LSelection.Defined then
        begin
          SelectNyxLCLText(LFocus, LSelection);
        end;
      end;
    end;
    FSourceLine := 0;
    FSourceColumn := 0;
    FReplaceCanvas := False;
    RestoreProjectControls;
    FChangingProject := False;
    Inc(FPaintCount);
  finally
    LShell.Free;
    FPainting := False;
  end;
end;

procedure TNyxNativeStudio.SourceCommandChanged(AState: TNyxSourceCommandState;
  const AMessage: TNyxText);
begin
  FState.Status := AMessage;
  FState.SourceStatus := AMessage;
  { Every connected project's own callback records its pair. An offline editor
    has no bridge. Completion never consults a newly selected project. }
  RequestRefresh(AState = nssApplied);
end;

procedure TNyxNativeStudio.SourceEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if FPainting or FChangingProject then
  begin
    Exit;
  end;
  try

    if RouteNyxStudioSource(FSession, ANode, AEvent.Trigger) then
    begin
      FState.Status := 'Pascal draft / apply when ready';
      { The session owns text immediately. The bridge's project-owned timer
        captures once for a typing burst; transport never borrows this memo. }

      if CurrentBridge <> nil then
      begin
        CurrentBridge.RecordDraft;
      end;
      { Ordinary typing never replaces chrome, focus, selection or scroll. }
    end;
  except
    on LException: Exception do
    begin
      FState.Status := LException.Message;
    end;
  end;
end;

procedure TNyxNativeStudio.UpdateTitleAndSource;
begin
  FPainting := True;
  try

    if FCodeView.Root <> nil then
    begin
      FCodeView.Root.Configure.Value(FSession.DraftSource).Done;
      FCodeView.Sync;
    end;

    if FShellView.Root <> nil then
    begin
      FShellView.Root.Find('studio-subtitle').Configure
        .Text('STUDIO  /  ' + FSession.Document.Title).Done;
      FShellView.Sync;
    end;
  finally
    FPainting := False;
  end;
end;

procedure TNyxNativeStudio.CanvasEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if FPainting or FChangingProject then
  begin
    Exit;
  end;
  try
    case AEvent.Trigger of
      ntDesignSelect:
        begin
          FSession.Select(ANode.DesignID);
          FCanvasView.Select(FSession.SelectedID);
          RecordLocal;
          RequestRefresh;
        end;
      ntDesignValue:
        begin
          FSession.SetCanvasValue(ANode);
          FState.Status := 'Design updated / Pascal generated';
          RecordLocal;
          RequestRefresh;
        end;
    else
      begin
        { Runtime preview events already belong to its application, not editor
          authoring. They must not become design commands or history entries. }
      end;
    end;
  except
    on LException: Exception do
    begin
      FState.Status := LException.Message;
      RequestRefresh(True);
    end;
  end;
end;

procedure TNyxNativeStudio.SaveProject;
var
  LExpected: TNyxText;
begin
  LExpected := '';

  if FState.ProjectName = FBoundProject then
  begin
    LExpected := FProjectRevision;
  end;

  if not FStore.SaveProject(FState.ProjectName, LExpected,
    FSession.ProjectSnapshot, FRemoteRevision, FRemotePair) then
  begin
    FState.ProjectConflict := True;
    FState.FilesVisible := True;
    FState.Status := 'Saved files changed / current work retained';
    Exit;
  end;
  FProjectRevision := FRemoteRevision;
  FBoundProject := FState.ProjectName;
  FSavedPair := EncodeNyxProject(FSession.ProjectSnapshot);
  FState.ProjectConflict := False;
  FState.Status := 'Paired design, Pascal and draft saved';
end;

procedure TNyxNativeStudio.OpenProject;
begin
  FRemotePair := FStore.ReadProject(FState.ProjectName, FRemoteRevision);

  if FRemotePair = '' then
  begin
    raise ENyxModel.Create('No saved project with that name');
  end;

  if EncodeNyxProject(FSession.ProjectSnapshot) <> FSavedPair then
  begin
    FState.ProjectConflict := True;
    FState.Status := 'Current work is unsaved / review before opening';
    Exit;
  end;
  LoadProject(DecodeNyxProject(FRemotePair));
  FBoundProject := FState.ProjectName;
  FProjectRevision := FRemoteRevision;
  FState.ProjectConflict := False;
end;

procedure TNyxNativeStudio.AcceptRemote;
var
  LIdentity: TGUID;
  LRevision: TNyxText;
  LRemote: TNyxText;
  LName: TNyxText;
begin

  if not FState.ProjectConflict or (FRemotePair = '') then
  begin
    raise ENyxModel.Create('No pending project conflict');
  end;
  { Preserve the current exact pair before the explicit warned-open action. A
    failed backup or candidate admission leaves the current session untouched. }
  CreateGUID(LIdentity);
  LName := 'backup-' + Copy(GUIDToString(LIdentity), 2, 36);

  if not FStore.SaveProject(LName, '', FSession.ProjectSnapshot, LRevision, LRemote) then
  begin
    raise ENyxModel.Create('Cannot create the independent project backup');
  end;
  LoadProject(DecodeNyxProject(FRemotePair));
  FBoundProject := FState.ProjectName;
  FProjectRevision := FRemoteRevision;
  FState.ProjectConflict := False;
end;

procedure TNyxNativeStudio.HierarchyEvent(const AEvent: TNyxEventInfo);
var
  LNode: TNyxNode;
  LSelected: TNyxText;
begin

  if FShellView.Root = nil then
  begin
    Exit;
  end;
  LNode := FShellView.Root.Find(NyxStudioHierarchyID);

  if LNode <> nil then
  begin
    LSelected := FSession.SelectedID;
    ShellEvent(LNode, AEvent);

    if (LSelected <> FSession.SelectedID) and (FCanvasView.Root <> nil) and
      (FCanvasID = FSession.ActiveViewID) then
    begin
      { A deliberate hierarchy navigation reveals the exact authored face.
        Ordinary observing paints remain scroll-neutral; the pending chrome
        refresh retains this same public view and its new viewport offset. }
      FCanvasView.Reveal(FSession.SelectedID, niDesign);
    end;
  end;
end;

procedure TNyxNativeStudio.ShellEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LEffect: TNyxInspectorEffect;
  LRemoval: TNyxCallbackRemoval;
  LDiagnostic: TNyxSourceDiagnostic;
  LLine: Integer;
  LChanged: Boolean;
  LBefore: TNyxText;
  LBackup: TGUID;
  LBackupRevision: TNyxText;
  LBackupRemote: TNyxText;
  LHierarchyChanged: Boolean;
begin

  if FPainting or FChangingProject then
  begin
    Exit;
  end;
  LChanged := False;
  try

    if RouteNyxStudioHierarchy(FSession, ANode, AEvent, LHierarchyChanged) then
    begin

      if LHierarchyChanged then
      begin
        RecordLocal;
        RequestRefresh;
      end;
      Exit;
    end;

    if FSourceCommands.Route(ANode, AEvent.Trigger) then
    begin
      Exit;
    end;
  except
    on LException: Exception do
    begin
      FState.Status := LException.Message;
      RequestRefresh;
      Exit;
    end;
  end;
  LBefore := FSession.Save;
  try

    if (ANode.ID = 'studio-split') and (AEvent.Trigger = ntChange) then
    begin
      FState.CanvasPercent := StrToIntDef(ANode.Prop('split-position'), FState.CanvasPercent);
      Exit;
    end;

    if RouteNyxCompilerDiagnostic(FSession, ANode, AEvent.Trigger,
      FCompilerReport, LDiagnostic) or
      RouteNyxSourceDiagnostic(FSession, ANode, AEvent.Trigger, LDiagnostic) then
    begin
      FSourceLine := LDiagnostic.Line;
      FSourceColumn := LDiagnostic.Column;
      FState.CodeVisible := True;
      FState.Panel := nspDesign;
    end
    else if RouteNyxStudioEvents(FSession, ANode, AEvent.Trigger,
      FState.CallbackRemoval, LEffect, LLine, LRemoval) then
    begin
      case LEffect of
        nieSource:
          begin
            FSourceLine := LLine;
            FSourceColumn := 1;
            FState.CodeVisible := True;
            FState.Panel := nspDesign;
          end;
        nieRequestRemoval:
          begin
            FState.CallbackRemoval := LRemoval;
          end;
        nieCancelRemoval, nieRemoved:
          begin
            FState.CallbackRemoval.Pending := False;
          end;
        nieNone:
          begin
            { Policy changes do not replace the designer projection. }
          end;
      end;
    end
    else if RouteNyxStudioSource(FSession, ANode, AEvent.Trigger) or
      RouteNyxStudioProperty(FSession, ANode, AEvent.Trigger) or
      RouteNyxStudioAuthoring(FSession, ANode, AEvent.Trigger, FShellView.Root) then
    begin
      LChanged := True;
      FState.Status := 'Design / Pascal updated';
    end
    else if RouteNyxStudioPalette(ANode, AEvent.Trigger, FState.Palette) then
    begin
      { Discovery is local presentation and preserves mounted editor views. }
    end
    else if AEvent.Trigger = ntChange then
    begin
      case DecodeNativeCommand(ANode.ID) of
        ncTitle:
          begin
            FSession.SetTitle(ANode.Prop('value'));
            UpdateTitleAndSource;
            RecordLocal;
          end;
        ncProjectName:
          begin
            FState.ProjectName := ANode.Prop('value');
          end;
        ncStateName:
          begin
            FState.NewStateName := ANode.Prop('value');
          end;
        ncStateValue:
          begin
            FState.NewStateValue := ANode.Prop('value');
          end;
        ncStateInput:
          begin

            if not TryNyxStudioStateInput(ANode.Prop('value'), FState.NewStateInput) then
            begin
              raise ENyxModel.Create('Unknown state input type');
            end;
            case FState.NewStateInput of
              ssiText:
                begin
                  FState.NewStateValue := '';
                end;
              ssiEscapedText:
                begin
                  FState.NewStateValue := '""';
                end;
              ssiBoolean:
                begin
                  FState.NewStateValue := 'false';
                end;
              ssiInteger, ssiNumber:
                begin
                  FState.NewStateValue := '0';
                end;
            end;
            RequestRefresh;
          end;
        ncBindingTarget:
          begin

            if not TryNyxStudioBindingTarget(ANode.Prop('value'), FState.BindingTarget) then
            begin
              raise ENyxModel.Create('Unknown binding target');
            end;
          end;
      else
        begin

          if ANode.Prop('output-field') <> '' then
          begin
            FOutputs.SetField(ANode.Prop('output-field'), ANode.Prop('value'));
            for LLine := 0 to High(NyxOutputFields) do
            begin

              if ANode.Prop('output-field') = NyxOutputFields[LLine] then
              begin
                Include(FOutputChanged, LLine);
              end;
            end;
          end
          else
          begin
            Exit;
          end;
        end;
      end;
      { Pending text fields must survive a click's blur without replacing the
        pressed action. These drafts require no paint until another UI action. }
      Exit;
    end
    else if AEvent.Trigger = ntClick then
    begin

      if ANode.Extensions.Has(NyxStudioWorkspaceJumpKey) then
      begin

        if ANode.Extensions.Value(NyxStudioWorkspaceJumpKey).AsText = '' then
        begin
          JumpWorkspace(NyxPrimaryWorkspace);
        end
        else
        begin
          JumpWorkspace(NyxWorkspace(ANode.Extensions.Value(NyxStudioWorkspaceJumpKey).AsText));
        end;
        Exit;
      end
      else if ANode.Extensions.Has(NyxStudioWorkspaceCloseKey) then
      begin
        CurrentBridge.RequestWorkspaceClose(NyxWorkspace(
          ANode.Extensions.Value(NyxStudioWorkspaceCloseKey).AsText));
        Exit;
      end
      else if ANode.Prop('add-kind') <> '' then
      begin
        FSession.AddKind(ANode.Prop('add-kind'));
        FState.Panel := nspDesign;
        LChanged := True;
      end
      else if ANode.Prop('view-id') <> '' then
      begin
        FSession.Activate(ANode.Prop('view-id'));
        FState.Panel := nspDesign;
        LChanged := True;
      end
      else if ANode.Prop('select-id') <> '' then
      begin
        FSession.Select(ANode.Prop('select-id'));
      end
      else if ANode.Prop('component-id') <> '' then
      begin
        FSession.AddComponentInstance(ANode.Prop('component-id'));
        FState.Panel := nspDesign;
        LChanged := True;
      end
      else if ANode.Prop('override-path') <> '' then
      begin
        FSession.CustomizePart(ANode.Prop('override-path'));
        LChanged := True;
      end
      else
      begin
        case DecodeNativeCommand(ANode.ID) of
          ncUndo, ncRedo, ncDelete, ncDuplicate,
          ncUp, ncDown, ncAddPage, ncComponent:
            begin
              case DecodeNativeCommand(ANode.ID) of
                ncUndo:
                  begin

                    if (CurrentBridge <> nil) and CurrentBridge.Enabled then
                    begin
                      CurrentBridge.History(nehUndo);
                    end
                    else
                    begin
                      FSession.Undo;
                    end;
                  end;
                ncRedo:
                  begin

                    if (CurrentBridge <> nil) and CurrentBridge.Enabled then
                    begin
                      CurrentBridge.History(nehRedo);
                    end
                    else
                    begin
                      FSession.Redo;
                    end;
                  end;
                ncDelete:
                  begin
                    FSession.DeleteSelected;
                  end;
                ncDuplicate:
                  begin
                    FSession.DuplicateSelected;
                  end;
                ncUp:
                  begin
                    FSession.MoveSelected(-1);
                  end;
                ncDown:
                  begin
                    FSession.MoveSelected(1);
                  end;
                ncAddPage:
                  begin
                    FSession.AddPage;
                  end;
                ncComponent:
                  begin
                    FSession.CreateComponent;
                  end;
              else
                begin
                  raise ENyxModel.Create('Unsupported native document command');
                end;
              end;
              LChanged := True;
            end;
          ncReviewRoot, ncCancelRoot, ncRemoveRoot:
            begin
              LChanged := RouteNyxRootRemoval(FSession, ANode.ID,
                AEvent.Trigger, FRootRemoval) = nreRemoved;
            end;
          ncCode:
            begin
              FState.CodeVisible := not FState.CodeVisible;
            end;
          ncOutputs:
            begin
              FState.OutputVisible := not FState.OutputVisible;

              if FState.OutputVisible and (CurrentBridge <> nil) and
                CurrentBridge.State.CanBuild and not FOutputLoaded and not FOutputLoading then
              begin
                ReadOutputs(FCurrentProject, False);
              end;
            end;
          ncFiles:
            begin
              FState.FilesVisible := not FState.FilesVisible;
            end;
          ncAdvanced:
            begin
              FState.AdvancedProperties := not FState.AdvancedProperties;
            end;
          ncDesignPanel:
            begin
              FState.Panel := nspDesign;
            end;
          ncProjectPanel:
            begin
              FState.Panel := nspProject;
            end;
          ncInspectorPanel:
            begin
              FState.Panel := nspInspector;
            end;
          ncProperties:
            begin
              FState.InspectorTab := nitProperties;
            end;
          ncEvents:
            begin
              FState.InspectorTab := nitEvents;
            end;
          ncState:
            begin
              FState.StateVisible := not FState.StateVisible;
            end;
          ncBindings:
            begin
              FState.BindingsVisible := not FState.BindingsVisible;
            end;
          ncPhone:
            begin
              FState.Phone := True;
            end;
          ncDesktop:
            begin
              FState.Phone := False;
            end;
          ncPreview:
            begin
              FPreview := not FPreview;
              LChanged := True;
            end;
          ncOutputNone, ncOutputBrowser, ncOutputLCL:
            begin
              FState.OutputTarget := ANode.Prop('output-target');
              ValidateNyxOutputTarget(FState.OutputTarget);
            end;
          ncSave, ncProjectSave:
            begin
              SaveProject;
            end;
          ncProjectOpen:
            begin
              OpenProject;
            end;
          ncProjectRemote:
            begin
              AcceptRemote;
            end;
          ncProjectCopy:
            begin
              FState.ProjectName := FState.ProjectName + '-copy';
              FState.ProjectConflict := False;
            end;
          ncAgents:
            begin
              FState.AgentsVisible := not FState.AgentsVisible;
            end;
          ncAgentConnect:
            begin

              if CurrentBridge = nil then
              begin
                raise ENyxModel.Create('Choose an explicit local service connection when launching native Studio');
              end;
              CurrentBridge.Connect;
            end;
          ncAgentPause:
            begin
              CurrentBridge.Pause;
            end;
          ncAgentAccept:
            begin
              CreateGUID(LBackup);

              if not FStore.SaveProject('backup-' + Copy(GUIDToString(LBackup), 2, 36),
                '', FSession.ProjectSnapshot, LBackupRevision, LBackupRemote) then
              begin
                raise ENyxModel.Create('Cannot save the exact local backup; current work is retained');
              end;
              CurrentBridge.AcceptRemote;
            end;
          ncAgentDisabled, ncAgentReadOnly, ncAgentEdit:
            begin
              case DecodeNativeCommand(ANode.ID) of
                ncAgentDisabled:
                  begin
                    CurrentBridge.Configure(apDisabled);
                  end;
                ncAgentReadOnly:
                  begin
                    CurrentBridge.Configure(apReadOnly);
                  end;
                ncAgentEdit:
                  begin
                    CurrentBridge.Configure(apEdit);
                  end;
              else
                begin
                  raise ENyxModel.Create('Unsupported native permission command');
                end;
              end;
            end;
          ncWorkspaceCloseCancel:
            begin
              CurrentBridge.CancelWorkspaceClose;
            end;
          ncWorkspaceCloseConfirm:
            begin
              CurrentBridge.ConfirmWorkspaceClose;
            end;
          ncBuildView, ncBuildApplication:
            begin
              BeginBuild(DecodeNativeCommand(ANode.ID) = ncBuildApplication);
            end;
          ncCompiledRun:
            begin
              BeginCompiledPreview(FCurrentProject);
            end;
          ncCompiledStop:
            begin

              if (FCurrentProject = nil) or (FCurrentProject.CompiledPreview = nil) then
              begin
                raise ENyxModel.Create('This project has no owned compiled preview');
              end;
              FCurrentProject.CompiledPreview.Cancel;
              FCurrentProject.CompiledPreview.Stop;

              if FCurrentProject.BuildStage in [nbsProfile, nbsOutputs, nbsRequest, nbsPolling] then
              begin
                { Stop owns preview execution, not an independently admitted
                  compiler job. Keep its pending receipt/status observation. }
                FCurrentProject.BuildMessage := 'Compiled preview stopped / build continues';
              end
              else
              begin
                FCurrentProject.BuildStage := nbsTerminal;
                FCurrentProject.BuildMessage := 'Compiled preview stopped';
              end;
              FState.Status := FCurrentProject.BuildMessage;
            end;
        else
          begin
            raise ENyxModel.Create('This action requires the native service connection');
          end;
        end;
      end;
    end
    else
    begin
      Exit;
    end;
    RecordLocal;
    RequestRefresh(LChanged and (FSession.Save <> LBefore) or
      (FCanvasID <> FSession.ActiveViewID) or
      ((ANode.ID = 'action-preview') and (AEvent.Trigger = ntClick)));
  except
    on LException: Exception do
    begin
      FState.Status := LException.Message;
      RequestRefresh;
    end;
  end;
end;

end.
