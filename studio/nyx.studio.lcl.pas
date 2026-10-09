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
  nyx.studio.sections, nyx.studio.section.views,
  Classes, SysUtils, Forms, Controls, ExtCtrls,
  nyx.text, nyx.types, nyx.behavior, nyx.data, nyx.model, nyx.theme, nyx.render.lcl,
  nyx.events, nyx.viewport, nyx.projection.refresh, nyx.callbacks, nyx.studio.collections,
  nyx.hostspace, nyx.hostspace.lcl,
  nyx.content.editor, nyx.content.mount, nyx.view.recovery,
  nyx.theme.editor, nyx.studio.theme,
  nyx.images, nyx.image.editor, nyx.image.import, nyx.image.import.lcl,
  nyx.resources, nyx.resources.editor, nyx.resources.rows.editor,
  nyx.resources.browser, nyx.resources.workspace, nyx.studio.resource.browser, nyx.publication,
  nyx.resource.context,
  nyx.resources.import, nyx.resources.import.lcl,
  nyx.studio.help, nyx.component.help, nyx.root.types,
  nyx.popover, nyx.popover.lcl,
  nyx.menu, nyx.menu.lcl, nyx.menu.button, nyx.controls, nyx.studio.menu,
  nyx.studio.session, nyx.studio.view, nyx.studio.projects,
  nyx.studio.projectstore, nyx.studio.outputs, nyx.studio.rootedits,
  nyx.studio.compiler, nyx.studio.agentbridge, nyx.studio.agentview,
  nyx.studio.workspaces, nyx.studio.builds, nyx.studio.editorbuild,
  nyx.studio.exchange, nyx.studio.preview, nyx.studio.preview.lcl,
  nyx.studio.sourcejobs, nyx.modal, nyx.modal.lcl,
  nyx.designer.input, nyx.gestures, nyx.studio.edits, nyx.studio.drag,
  nyx.designer.placement,
  nyx.designer.resize, nyx.designer.guides, nyx.studio.resize, nyx.presentations, nyx.studio.move;

type
  TNyxNativeStudio = class;
  TNyxNativeBuildStage = (nbsIdle, nbsProfile, nbsOutputs, nbsRequest, nbsPolling,
    nbsPreviewInspect, nbsPreviewDownload, nbsPreviewActivate, nbsTerminal, nbsLaunchInspect);

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
    { Last successful artifact owns an independent acceptance frame. Starting
      or cancelling another job must not erase this still-current preview. }
    AcceptedBuildJob: TNyxBuildJobRef;
    AcceptedBuildPair: TNyxText;
    AcceptedBuildOutput: TNyxText;
    BuildProfileSent: TNyxText;
    BuildRequested: Boolean;
    BuildMessage: TNyxText;
    BuildOutputSnapshot: TNyxText;
    CompiledPreview: TNyxLCLCompiledPreview;
    { Each observer consumes a transient semantic sequence once. Hidden projects
      retain intent but never start an executable until they are observed. }
    ConsumedLaunchSequence: Integer;
    { One admitted backend endpoint owns the sequence domain; reconnect cannot
      reuse the previous backend's high-water mark. Contains no credential. }
    ConsumedLaunchEndpoint: TNyxText;
    LaunchPending: TNyxCompilerLaunch;
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
    FHostSpace: INyxHostSpace;
    FImagePicker: INyxImagePicker;
    FResourcePicker: INyxResourcePicker;
    FResourcePickContext: TNyxStudioCommandContext;
    FResourcePickDraft: TNyxDataValue;
    FImagePickContext: TNyxStudioCommandContext;
    FImagePickOwner: TNyxText;
    FImagePickBaseline: TNyxText;
    FSession: TNyxStudioSession;
    FSourceCommands: TNyxSourceCommands;
    FTheme: TNyxTheme;
    FShell: TNyxDocument;
    FCodeDocument: TNyxDocument;
    FSourcePaneDocument: TNyxDocument;
    FSourcePaneView: TNyxLCLRenderer;
    FSourcePaneParking: TPanel;
    FSourceModal: INyxLCLModalHost;
    FSourceFocusPending: Boolean;
    FShellView: TNyxStudioSectionViews;
    FResourceBrowser: TNyxStudioResourceBrowser;
    { Public managed Nyx presentation owns cloned component help content. }
    FComponentHelp: INyxLCLPopover;
    FActionMenu: INyxLCLMenu;
    FActionButton: INyxMenuButton;
    FCanvasMenus: INyxMenuBindings;
    { Borrowed receiver registration; cancelled before any controller teardown. }
    FHierarchySubscription: INyxEventSubscription;
    FCanvasView: TNyxLCLRenderer;
    FDesignerDrag: TNyxStudioDrag;
    FDesignerResize: TNyxStudioResize;
    FDesignerMove: TNyxStudioMove;
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
    { Deferred field resets are value-only and bound to this exact session/load.
      Pending proposals overlay them without changing accepted defaults/history. }
    FCanvasRestores: TNyxProjectionValueRestores;
    FCanvasRestoreContext: TNyxStudioCommandContext;
    FCanvasCommandContext: TNyxStudioCommandContext;
    { Shell controls remain mounted until deferred painting after a project load.
      Their old identities must not submit intent into the newly accepted load. }
    FShellCommandContext: TNyxStudioCommandContext;
    { A surviving retry belongs to the failed current load, even if an earlier
      shell is still mounted. It cannot redirect recovery into another project. }
    FDisplayRecoveryContext: TNyxStudioCommandContext;
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
    procedure HostSpaceChanged(const AExtent: TNyxHostExtent);
    procedure ImagePicked(AStatus: TNyxImagePickStatus;
      const ASource: TNyxImageSource; const AError: TNyxText);
    procedure ResourcePicked(AStatus: TNyxResourcePickStatus;
      const ADefinition: INyxResourceDefinition; const AError: TNyxText);
    procedure PaintQueued(AData: PtrInt);
    procedure Paint;
    procedure ShellEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure MenuAction(AAction: TNyxStudioMenuAction);
    procedure PrepareActionMenu;
    procedure ShowComponentHelp(const AAnchor: TNyxControlRef);
    { Borrow current owners synchronously; hover never publishes a design pair. }
    function DesignerDragContext: TNyxStudioDragContext;
    function DesignerResizeMeasure(const AControl: TNyxControlRef): TNyxResizeSize;
    function DesignerMovePoint(const APointer: TNyxPointerSnapshot): TNyxResizePoint;
    function DesignerResizeGuides(const AControl: TNyxControlRef): TNyxAlignmentContext;
    procedure DesignerResizeStatus(const AMessage: TNyxText);
    procedure DesignerResizePresentation(const APreview: TNyxResizePreview);
    procedure DesignerDragFeedback(const ATarget: TNyxControlRef);
    procedure DesignerPlacementFeedback(const APreview: TNyxDropPreview);
    procedure DesignerGesture(const ATarget: TNyxDesignerTarget;
      const AEvent: TNyxEventInfo; const ADecision: INyxGestureDecision);
    procedure HierarchyEvent(const AEvent: TNyxEventInfo);
    procedure CanvasEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure RememberCanvasRestore(const ARestore: TNyxProjectionValueRestore);
    procedure SourceEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure SourceCommandChanged(AState: TNyxSourceCommandState;
      const AMessage: TNyxText);
    { Projection refusal is separate from accepted source admission. These
      presentation operations retain exact files/history and retry ownership. }
    procedure DisplayFailed(const AMessage: TNyxText);
    procedure SyncDisplayRecovery;
    procedure SaveProject;
    procedure OpenProject;
    procedure AcceptRemote;
    procedure CapturePresentation;
    { Target widgets remain borrowed; only visible pane positions enter copied
      presentation. Compact switching never owns a second resource proposal. }
    procedure CaptureResourcePaneScroll;
    procedure RestoreResourcePaneScroll;
    procedure SelectResourcePane(APane: TNyxResourceWorkspacePane);
    procedure SourceModalDismiss;
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
    procedure ConsumeCompilerLaunch(AProject: TNyxNativeStudioProject);
    procedure RecordLocal;
    procedure CaptureProject;
    procedure AdmitProject(AProject: TNyxNativeStudioProject);
    procedure RestoreProjectControls;
    function CurrentBridge: TNyxStudioAgentBridge;
    function GetAgentState: TNyxStudioAgentView;
    function ComposeShell: TNyxDocument;
    function GetPresentationPending: Boolean;
  protected
    { Ordinary Studio consumes the public local-file picker contract. }
    function CreateImagePicker: INyxImagePicker; virtual;
    { Own the replaceable byte-only picker; cancel its borrowed reply before
      retirement. The shared resource form remains independent of LCL dialogs. }
    function CreateResourcePicker: INyxResourcePicker; virtual;
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
    { UI-thread observation shared with the browser Studio. Readiness includes
      active and queued source work, independently of mounted status controls.
      False means quiescent, not successful admission; inspect results separately. }
    function SourceBusy: Boolean;
    { Borrow the current project's command context on the UI thread. A project
      jump may change this identity; callers must never free the borrowed owner. }
    property SourceCommands: TNyxSourceCommands read FSourceCommands;
    property ShellView: TNyxStudioSectionViews read FShellView;
    property CanvasView: TNyxLCLRenderer read FCanvasView;
    property CodeView: TNyxLCLRenderer read FCodeView;
    { Borrowed source workspace/window contracts; destroy neither. }
    property SourceView: TNyxLCLRenderer read FSourcePaneView;
    property SourceModal: INyxLCLModalHost read FSourceModal;
    property PaintCount: Integer read FPaintCount;
    { UI-thread observation of queued/active presentation work. Source completion
      can precede its visible paint. False means no paint is pending, not that a
      renderer error or visual-quality gate has been resolved. }
    property PresentationPending: Boolean read GetPresentationPending;
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

function TNyxNativeStudio.SourceBusy: Boolean;
begin
  Result := (FSourceCommands <> nil) and FSourceCommands.Busy;
end;

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
    ncSourceTab,
    ncMessagesTab,
    ncExpandSource,
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
    ncBuilds,
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
    'action-source-tab',
    'action-messages-tab',
    'action-expand-source',
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
    'action-builds',
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
    Owner.ConsumeCompilerLaunch(Self);
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

    if Bridge.State.CanReportRuntime then
    begin
      Bridge.PreviewGrant(AcceptedBuildJob, LaunchPending.Sequence);
    end
    else
    begin
      Bridge.BuildStatus(AcceptedBuildJob);
    end;
  except
    on LException: Exception do
    begin
      BuildStage := nbsTerminal;
      BuildMessage := LException.Message;
      State.Status := BuildMessage;

      if LaunchPending.Sequence > 0 then
      begin
        Bridge.ReportLaunch(LaunchPending, btNativeLCL, clrRefused,
          'Native artifact preparation failed');
        ConsumedLaunchSequence := LaunchPending.Sequence;
        LaunchPending := Default(TNyxCompilerLaunch);
      end;

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
  FHostSpace := NewNyxLCLHostSpace(FHost, NyxHostSizing.Fit(nhfAvailableHeight));
  FSession := TNyxStudioSession.Create;
  FSourceCommands := TNyxSourceCommands.Create(FSession, SourceCommandChanged);
  FTheme := TNyxTheme.Create;
  FOutputs := TNyxOutputConfiguration.Create;
  FStore := TNyxProjectStore.Create(AProjectDirectory);
  FPreviewDirectory := IncludeTrailingPathDelimiter(ExpandFileName(AProjectDirectory)) +
    'compiled-previews';
  FState := DefaultNyxStudioViewState;
  FState.CodePresentation := ncpPaneHosted;
  FState.Outputs := FOutputs;
  FShellView := TNyxStudioSectionViews.Create(FTheme, nscEditorOwnedHierarchy);
  FResourceBrowser := TNyxStudioResourceBrowser.Create;
  FCanvasView := TNyxLCLRenderer.Create(FTheme);
  FCanvasView.DesignerInput := NyxDesignerInput.Drops(True);
  FCanvasView.OnDesignerGesture := DesignerGesture;
  FDesignerDrag := TNyxStudioDrag.Create(DesignerDragContext, DesignerDragFeedback,
    DesignerPlacementFeedback);
  FDesignerResize := TNyxStudioResize.Create(DesignerDragContext,
    DesignerResizeMeasure, DesignerResizeStatus, DesignerResizePresentation, DesignerResizeGuides);
  FDesignerMove := TNyxStudioMove.Create(DesignerDragContext, DesignerResizeGuides,
    DesignerResizeStatus, DesignerResizePresentation, DesignerMovePoint);
  FCodeView := TNyxLCLRenderer.Create(FTheme);
  FSourcePaneView := TNyxLCLRenderer.Create(FTheme);
  FSourcePaneView.OnEvent := ShellEvent;
  FSourceModal := NewNyxLCLModalHost(FHost);
  FSourceModal.OnDismiss := SourceModalDismiss;
  FShellView.OnEvent := ShellEvent;
  FCanvasView.OnEvent := CanvasEvent;
  FCodeView.OnEvent := SourceEvent;
  FCanvasParking := TPanel.Create(nil);
  FCanvasParking.Parent := FHost;
  FCanvasParking.Visible := False;
  FCodeParking := TPanel.Create(nil);
  FCodeParking.Parent := FHost;
  FCodeParking.Visible := False;
  FSourcePaneParking := TPanel.Create(nil);
  FSourcePaneParking.Parent := FHost;
  FSourcePaneParking.Visible := False;
  FHostSpace.OnChange := HostSpaceChanged;
  FSavedPair := EncodeNyxProject(FSession.ProjectSnapshot);
  FInitialPair := FSavedPair;
end;

destructor TNyxNativeStudio.Destroy;
var
  LIndex: Integer;
begin
  FRunning := False;

  if FImagePicker <> nil then
  begin
    FImagePicker.Cancel;
    FImagePicker := nil;
  end;

  if FResourcePicker <> nil then
  begin
    FResourcePicker.Cancel;
    FResourcePicker := nil;
  end;

  if FCanvasView <> nil then
  begin
    FCanvasView.OnDesignerGesture := nil;
  end;
  FreeAndNil(FDesignerDrag);
  FreeAndNil(FDesignerResize);
  FreeAndNil(FDesignerMove);

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

  if FHostSpace <> nil then
  begin
    FHostSpace.Disconnect;
    FHostSpace := nil;
  end;
  FCanvasView.Free;
  if FSourceModal <> nil then
  begin
    FSourceModal.OnDismiss := nil;
    FSourceModal.Hide;
  end;
  FCodeView.Free;
  FSourcePaneView.Free;

  if FCodeParking <> nil then
  begin
    { A refused modal projection may still have borrowed this parking panel.
      Retire it explicitly with the controller, after its code view unmounts. }
    FCodeParking.Parent := FHost;
  end;
  FSourceModal := nil;
  FActionButton := nil;
  FActionMenu := nil;
  FCanvasMenus := nil;
  FComponentHelp := nil;
  FResourceBrowser.Free;
  FShellView.Free;
  FCanvasParking.Free;
  FCodeParking.Free;
  FSourcePaneParking.Free;
  FCodeDocument.Free;
  FSourcePaneDocument.Free;
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

  if FSourceCommands.Busy then
  begin
    raise ENyxModel.Create('Wait for pending editor changes before building');
  end;

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
    nbsPreviewInspect, nbsPreviewDownload, nbsPreviewActivate, nbsLaunchInspect] then
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
  LProject.BuildMessage := 'Checking compiler output';
  FState.Status := LProject.BuildMessage;
  ReadOutputs(LProject, True);
end;

function TNyxNativeStudio.CompiledPreviewCurrent(AProject: TNyxNativeStudioProject): Boolean;
begin
  Result := (AProject <> nil) and (AProject.BuildArtifact <> '') and
    (AProject.AcceptedBuildJob.ID <> '') and AProject.Bridge.SourceSynchronized and
    (EncodeNyxProject(AProject.Session.ProjectSnapshot) = AProject.AcceptedBuildPair) and
    (FOutputs.Encode = AProject.AcceptedBuildOutput);
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
  AProject.Bridge.BuildStatus(AProject.AcceptedBuildJob);
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

  if LView.BuildReplyKind = coCancel then
  begin
    { Independent panel actions do not advance a pending compiler or preview
      stage. Cancellation remains active until an ordinary joined status. }
    if LReply.Field('state').AsText = 'rejected' then
    begin
      AProject.State.Status := LReply.Field('error').AsText;
    end
    else
    begin
      AProject.State.Status := 'Cancellation requested / accepted source and preview retained';
    end;

    if FCurrentProject = AProject then
    begin
      FState.Status := AProject.State.Status;
      RequestRefresh;
    end;
    Exit;
  end;

  if LView.BuildReplyKind in [coJobs, coLaunchResult] then
  begin
    Exit;
  end;
  try

    if NyxAgentHas(LReply, 'state') and (LReply.Field('state').AsText = 'rejected') then
    begin
      raise ENyxModel.Create(LReply.Field('error').AsText);
    end;
    case AProject.BuildStage of
      nbsLaunchInspect:
        begin

          if (LReply.Field('job').AsText <> AProject.LaunchPending.Job.ID) or
            (LView.CompilerLaunch.Sequence <> AProject.LaunchPending.Sequence) or
            not LReply.Field('currentSource').AsBoolean or
            not LReply.Field('currentOutput').AsBoolean or
            not AProject.Bridge.SourceSynchronized or
            (EncodeNyxProject(AProject.Session.ProjectSnapshot) <> AProject.BuildPair) or
            (FOutputs.Encode <> AProject.BuildOutputSnapshot) then
          begin
            raise ENyxModel.Create('Semantic launch changed before native preparation');
          end;
          AdmitNyxCompiledArtifact(LReply);
          AProject.BuildArtifact := LReply.Field('artifact').AsText;
          AProject.AcceptedBuildJob := AProject.LaunchPending.Job;
          AProject.AcceptedBuildPair := AProject.BuildPair;
          AProject.AcceptedBuildOutput := AProject.BuildOutputSnapshot;
          AProject.BuildStage := nbsTerminal;
          BeginCompiledPreview(AProject);
        end;
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

          if NyxBuildJobTerminal(ParseNyxBuildJobState(LReply.Field('state').AsText)) then
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
              AProject.AcceptedBuildJob := AProject.BuildJob;
              AProject.AcceptedBuildPair := AProject.BuildPair;
              AProject.AcceptedBuildOutput := AProject.BuildOutputSnapshot;
              AProject.BuildMessage := 'Build complete / compiled artifact available';

              if (AProject.CompiledPreview <> nil) and AProject.CompiledPreview.Running and
                (FCurrentProject = AProject) then
              begin
                BeginCompiledPreview(AProject);
              end;
            end
            else if LReply.Field('state').AsText = 'cancelled' then
            begin
              AProject.BuildMessage := 'Build cancelled / accepted source and preview retained';
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

          if (LReply.Field('job').AsText <> AProject.AcceptedBuildJob.ID) or
            not CompiledPreviewCurrent(AProject) then
          begin
            raise ENyxModel.Create('Project or output changed; compiled preview activation refused');
          end;
          { Wire admission checks service currentness and the exact immutable
            artifact manifest; local pair/profile admission is repeated too. }
          AdmitNyxCompiledArtifact(LReply);
          AProject.BuildResult := LReply.Copy;

          if (AProject.LaunchPending.Sequence > 0) and
            (LView.CompilerLaunch.Sequence <> AProject.LaunchPending.Sequence) then
          begin
            raise ENyxModel.Create('Native semantic launch was replaced before activation');
          end;

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
              if NyxAgentHas(LReply, 'runtime') then
              begin
                AProject.CompiledPreview.Launch(LReply.Field('runtime'));
              end
              else
              begin
                AProject.CompiledPreview.Launch;
              end;
              AProject.BuildMessage := 'Compiled preview running';

              if AProject.LaunchPending.Sequence > 0 then
              begin
                AProject.Bridge.ReportLaunch(AProject.LaunchPending, btNativeLCL, clrMounted);
                AProject.LaunchPending := Default(TNyxCompilerLaunch);
              end;
            end
            else
            begin
              AProject.BuildMessage := 'Compiled preview ready / return to this project to run';

              if AProject.LaunchPending.Sequence > 0 then
              begin
                AProject.Bridge.ReportLaunch(AProject.LaunchPending, btNativeLCL, clrRefused,
                  'Observer left the project before native activation');
                AProject.LaunchPending := Default(TNyxCompilerLaunch);
              end;
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

      if AProject.LaunchPending.Sequence > 0 then
      begin
        AProject.ConsumedLaunchSequence := AProject.LaunchPending.Sequence;
        AProject.Bridge.ReportLaunch(AProject.LaunchPending, btNativeLCL, clrRefused,
          'Native preview preparation or exact-context admission failed');
        AProject.LaunchPending := Default(TNyxCompilerLaunch);
      end;

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

procedure TNyxNativeStudio.ConsumeCompilerLaunch(AProject: TNyxNativeStudioProject);
var
  LLaunch: TNyxCompilerLaunch;
begin

  if AProject.Bridge.State.Endpoint <> AProject.ConsumedLaunchEndpoint then
  begin
    AProject.ConsumedLaunchEndpoint := AProject.Bridge.State.Endpoint;
    AProject.ConsumedLaunchSequence := 0;
  end;
  LLaunch := AProject.Bridge.State.CompilerLaunch;

  if (FCurrentProject <> AProject) or
    (LLaunch.Sequence <= AProject.ConsumedLaunchSequence) or
    not (AProject.BuildStage in [nbsIdle, nbsTerminal]) or
    AProject.SourceCommands.Busy or (FOutputChanged <> []) or
    not AProject.Bridge.SourceSynchronized then
  begin
    Exit;
  end;

  if LLaunch.Target <> btNativeLCL then
  begin
    AProject.ConsumedLaunchSequence := LLaunch.Sequence;
    AProject.Bridge.ReportLaunch(LLaunch, btNativeLCL, clrUnavailable,
      'A browser compiled artifact requires a browser Studio observer');
    Exit;
  end;

  if not FOutputLoaded then
  begin
    { Agent execution is not contingent on the operator first opening Outputs.
      Load the private profile through the same guarded asynchronous UI seam.
      Do not retain execution intent during this preliminary read: it may be
      retired or replaced meanwhile. The next observation reacquires the
      current intent after the profile arrives, before owning any artifact. }
    ReadOutputs(AProject, False);
    Exit;
  end;
  AProject.ConsumedLaunchSequence := LLaunch.Sequence;

  if (LLaunch.Revision <> AProject.Bridge.State.Revision) or
    ((LLaunch.Scope <> bsApplication) and (LLaunch.Root.ID <> AProject.Session.ActiveViewID)) then
  begin
    AProject.Bridge.ReportLaunch(LLaunch, btNativeLCL, clrRefused,
      'The observing editor has a different revision or active view');
    Exit;
  end;
  AProject.LaunchPending := LLaunch;
  AProject.BuildPair := EncodeNyxProject(AProject.Session.ProjectSnapshot);
  AProject.BuildOutputSnapshot := FOutputs.Encode;
  AProject.BuildScope := LLaunch.Scope;
  AProject.BuildTarget := LLaunch.Target;
  AProject.BuildRoot := LLaunch.Root;
  AProject.BuildJob := LLaunch.Job;
  AProject.BuildStage := nbsLaunchInspect;
  AProject.BuildMessage := LLaunch.Actor + ' requested a compiled preview';
  AProject.Bridge.BuildStatus(LLaunch.Job);
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

  if ARenderer = nil then
  begin
    Exit;
  end;
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
  FCurrentProject.LeftTop := NativeScrollTop(FShellView.SectionView(nssProject), 'studio-left');
  FCurrentProject.RightTop := NativeScrollTop(FShellView.SectionView(nssInspector), 'studio-right');

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
      LProject.State.CodePresentation := ncpPaneHosted;
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
  FState.MenuEditorDraft.Clear;
  FState.MenuBarEditorDraft.Clear;
  FState.QueryEditorDraft.Clear;
  FState.TimeDomainEditorDraft.Clear;
  FState.ContentEditorDraft.Clear;
  FState.ThemeEditorDraft.Clear;
  FState.ImageEditorDraft.Clear;
  FState.ResourceEditorDraft.Clear;
  FState.ResourceRowsDraft.Clear;
  FState.ResourceSelection := NyxNewResourceSelection;
  FBoundProject := '';
  FProjectRevision := '';
  FSavedPair := EncodeNyxProject(FSession.ProjectSnapshot);
  FRootRemoval := nil;
  FCompilerReport := nil;
  FState.CallbackRemoval.Pending := False;
  FState.Status := 'Project opened';
  RequestRefresh(True);
end;

function TNyxNativeStudio.GetPresentationPending: Boolean;
begin
  Result := FQueued or FPainting;
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

procedure TNyxNativeStudio.CaptureResourcePaneScroll;
var
  LPane: TNyxResourceWorkspacePane;
  LControl: TControl;
  LPosition: Integer;
begin
  for LPane := Low(TNyxResourceWorkspacePane) to High(TNyxResourceWorkspacePane) do
  begin

    if FShellView.Root.Find(NyxResourceWorkspaceScrollID(
      NyxStudioResourceWorkspaceID, LPane)) = nil then
    begin
      Continue;
    end;
    LControl := FShellView.ControlFor(NyxResourceWorkspaceScrollID(
      NyxStudioResourceWorkspaceID, LPane));

    if not (LControl is TScrollBox) or not LControl.Visible then
    begin
      Continue;
    end;
    LPosition := TScrollBox(LControl).VertScrollBar.Position;

    if LPane = rwpFiles then
    begin
      FState.ResourceCatalogScroll := LPosition;
    end
    else
    begin
      FState.ResourceEditorScroll := LPosition;
    end;
  end;
end;

procedure TNyxNativeStudio.RestoreResourcePaneScroll;
var
  LPane: TNyxResourceWorkspacePane;
  LControl: TControl;
  LPosition: Integer;
begin
  for LPane := Low(TNyxResourceWorkspacePane) to High(TNyxResourceWorkspacePane) do
  begin

    if FShellView.Root.Find(NyxResourceWorkspaceScrollID(
      NyxStudioResourceWorkspaceID, LPane)) = nil then
    begin
      Continue;
    end;
    LControl := FShellView.ControlFor(NyxResourceWorkspaceScrollID(
      NyxStudioResourceWorkspaceID, LPane));
    LPosition := FState.ResourceEditorScroll;

    if LPane = rwpFiles then
    begin
      LPosition := FState.ResourceCatalogScroll;
    end;

    if (LControl is TScrollBox) and LControl.Visible then
    begin
      TScrollBox(LControl).VertScrollBar.Position := LPosition;
    end;
  end;
end;

procedure TNyxNativeStudio.SelectResourcePane(APane: TNyxResourceWorkspacePane);
var
  LWorkspace: TNyxNode;
begin
  CaptureResourcePaneScroll;
  FState.ResourcePane := APane;
  LWorkspace := FShellView.Root.Find(NyxStudioResourceWorkspaceID);

  if LWorkspace <> nil then
  begin
    RestoreNyxResourceWorkspace(LWorkspace, NyxPresentation('compact'), APane);
    FShellView.Sync;
    RestoreResourcePaneScroll;
  end;
end;

function TNyxNativeStudio.CreateResourcePicker: INyxResourcePicker;
begin
  Result := NewNyxLCLResourcePicker;
end;

procedure TNyxNativeStudio.ResourcePicked(AStatus: TNyxResourcePickStatus;
  const ADefinition: INyxResourceDefinition; const AError: TNyxText);
var
  LEditor: TNyxNode;
  LCurrent: TNyxResourceEditorDraft;
  LProjection: TNyxNode;
begin

  if AStatus = rpsCancelled then
  begin
    Exit;
  end;
  try

    if not FSession.MatchesCommandContext(FResourcePickContext) then
    begin
      raise ENyxResource.Create('Resource import belongs to an earlier project');
    end;
    LEditor := FShellView.Root.Find('studio-resource-editor');

    if LEditor = nil then
    begin
      raise ENyxResource.Create('Resource import form has closed');
    end;
    LProjection := FSession.SelectedProjection;
    try

      if not NyxResourceEditorContextMatches(LEditor, FSession.Document.Resources,
        FSession.Selected, LProjection) then
      begin
        raise ENyxResource.Create('Resource catalog or selected control changed while importing');
      end;
    finally
      LProjection.Free;
    end;
    LCurrent := Default(TNyxResourceEditorDraft);
    LCurrent.Capture('studio-resource-editor', FShellView.RootFor('studio-resource-editor'));

    if LCurrent.ToData.ToJSON <> FResourcePickDraft.ToJSON then
    begin
      raise ENyxResource.Create('Resource proposal changed while the file picker was open');
    end;

    if AStatus = rpsFailed then
    begin
      raise ENyxResource.Create(AError);
    end;
    ProposeNyxResourceEditor(LEditor, ADefinition);
    FState.ResourceEditorDraft.Capture('studio-resource-editor', FShellView.RootFor('studio-resource-editor'));
    FShellView.Sync;

  except
    on LException: Exception do
    begin
      FState.Status := LException.Message;
      RequestRefresh;
    end;
  end;
end;

function TNyxNativeStudio.CreateImagePicker: INyxImagePicker;
begin
  Result := NewNyxLCLImagePicker;
end;

procedure TNyxNativeStudio.ImagePicked(AStatus: TNyxImagePickStatus;
  const ASource: TNyxImageSource; const AError: TNyxText);
var
  LProjection: TNyxNode;
begin
  { An import may finish after project navigation, selection or another edit.
    Check the exact captured owner and effective baseline before touching a
    proposal. No accepted document is changed by a picker notification. }

  if AStatus = ipsCancelled then
  begin
    Exit;
  end;
  try

    if not FSession.MatchesCommandContext(FImagePickContext) or
      (FSession.SelectedID <> FImagePickOwner) then
    begin
      raise ENyxModel.Create('Image import belongs to an earlier project or selection');
    end;
    LProjection := FSession.SelectedProjection;
    try

      if NyxImageEditorBaseline(FSession.Selected, LProjection) <> FImagePickBaseline then
      begin
        raise ENyxModel.Create('Image changed while its import was open');
      end;
    finally
      LProjection.Free;
    end;

    if AStatus = ipsFailed then
    begin
      raise ENyxImage.Create(AError);
    end;
    { Refresh the mounted proposal before delivery. A user may change validation
      without changing the accepted owner baseline; retain that later choice
      instead of letting an earlier file request replace it. }
    FState.ImageEditorDraft.Capture('inspector-image', FShellView.RootFor('inspector-image'));

    if FState.ImageEditorDraft.Validation.ChecksumsRequired <> ASource.Validation.ChecksumsRequired then
    begin
      raise ENyxImage.Create('Image validation changed while its import was open');
    end;
    FState.ImageEditorDraft.Propose(ASource);
    FState.ImageEditorDraft.Restore(FShellView.RootFor('inspector-image'));
    FShellView.Sync;
  except
    on LException: Exception do
    begin
      FState.Status := TNyxText(LException.Message);
      RequestRefresh;
    end;
  end;
end;

procedure TNyxNativeStudio.HostSpaceChanged(const AExtent: TNyxHostExtent);
begin
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
      DisplayFailed(LException.Message);
    end;
  end;
end;

procedure TNyxNativeStudio.SyncDisplayRecovery;
var
  LNotice: TNyxNode;
  LCanvas: TControl;
begin

  if FCanvasView.Root <> nil then
  begin
    LCanvas := FCanvasView.ControlFor(FCanvasView.Root.ID);

    if LCanvas.Parent <> nil then
    begin
      { The renderer's enclosing panel is a target host, not an authored control.
        Suspend its old faces without overwriting any design Enabled property. }
      LCanvas.Parent.Enabled := not FState.DisplayRecovery.BlocksInput;
    end;
  end;

  if FShellView.Root = nil then
  begin
    Exit;
  end;
  LNotice := FShellView.Root.Find(NyxStudioDisplayRecoveryID);

  if FShellView.Root.Find('studio-status') <> nil then
  begin
    FShellView.Root.Find('studio-status').Configure.Text(FState.Status).Done;
    FShellView.ViewFor('studio-status').Sync;
  end;

  if LNotice <> nil then
  begin
    RestoreNyxViewRecovery(LNotice, FState.DisplayRecovery);
    { Chrome alone contains this stable notice. Synchronizing another section
      could re-enter the refused adapter while merely reporting its failure. }
    FShellView.ViewFor(LNotice.ID).Sync;
  end;

  if (FSourcePaneView.Root <> nil) and
    (FSourcePaneView.Root.Find('studio-source-status') <> nil) then
  begin
    { Source admission and target display have independent outcomes. Refresh
      only this owned status, leaving the nested editor and its input intact. }
    FSourcePaneView.Root.Find('studio-source-status').Configure
      .Text(FSourceCommands.Message).Visible(FSourceCommands.Message <> '').Done;
    FSourcePaneView.Sync;
  end;
end;

procedure TNyxNativeStudio.DisplayFailed(const AMessage: TNyxText);
begin
  FState.DisplayRecovery := TNyxViewRecovery.Failed(AMessage);
  FDisplayRecoveryContext := FSession.CommandContext;
  FState.Status := 'Display needs attention / ' + AMessage;
  try
    FDesignerDrag.Cancel;
  except
    on LException: Exception do
    begin
      FState.Status := FState.Status + ' / Gesture retirement: ' + LException.Message;
    end;
  end;
  try
    SyncDisplayRecovery;
  except
    on LException: Exception do
    begin
      { Retain the exact original refusal and report a failed notice too. Never
        queue the same failed proposal repeatedly or open a target error modal. }
      FState.Status := FState.Status + ' / Recovery notice: ' + LException.Message;
    end;
  end;
  try

    if (FActionButton = nil) and (FShellView.Root <> nil) and
      (FShellView.Root.Find(NyxStudioActionMenuID) <> nil) and
      FSession.MatchesCommandContext(FShellCommandContext) then
    begin
      { The current Chrome remains usable independently of a refused canvas.
        Its former menu was retired at the start of complete preparation. }
      PrepareActionMenu;
    end;
  except
    on LException: Exception do
    begin
      FState.Status := FState.Status + ' / Editor actions: ' + LException.Message;
    end;
  end;
end;

procedure TNyxNativeStudio.SourceModalDismiss;
begin
  FState.SourceExpanded := False;
  FSourceFocusPending := True;
  RequestRefresh;
end;

procedure TNyxNativeStudio.CapturePresentation;
var
  LNode: TNyxNode;
begin

  if FShellView.Root = nil then
  begin
    Exit;
  end;

  if FSession.MatchesCommandContext(FShellCommandContext) then
  begin
    FState.MenuEditorDraft.Capture('inspector-menu', FShellView.RootFor('inspector-menu'));
    FState.MenuBarEditorDraft.Capture('inspector-menu-bar', FShellView.RootFor('inspector-menu-bar'));
    FState.QueryEditorDraft.Capture('inspector-collection-query', FShellView.RootFor('inspector-collection-query'));
    FState.TimeDomainEditorDraft.Capture('inspector-time-domain', FShellView.RootFor('inspector-time-domain'));
    FState.ContentEditorDraft.Capture('inspector-content', FShellView.RootFor('inspector-content'));
    FState.ThemeEditorDraft.Capture('studio-theme-editor', FShellView.RootFor('studio-theme-editor'));
    FState.ImageEditorDraft.Capture('inspector-image', FShellView.RootFor('inspector-image'));
    FState.ResourceEditorDraft.Capture('studio-resource-editor', FShellView.RootFor('studio-resource-editor'));
    FState.ResourceRowsDraft.Capture('studio-resource-rows', FShellView.RootFor('studio-resource-rows'));
    FResourceBrowser.Capture(FShellView, FState.ResourceBrowser);
    CaptureResourcePaneScroll;

    if FShellView.Root.Find('studio-resources') <> nil then
    begin
      FState.ResourcesScroll := NativeScrollTop(FShellView.SectionView(nssResources),
        'studio-resources');
    end;
  end;
  LNode := FShellView.Root.Find('studio-split');

  if LNode <> nil then
  begin
    FState.CanvasPercent := StrToIntDef(LNode.Prop('split-position'), FState.CanvasPercent);
  end;
  LNode := FShellView.Root.Find('studio-details-split');

  if LNode <> nil then
  begin
    FState.DetailsPercent := StrToIntDef(LNode.Prop('split-position'), FState.DetailsPercent);
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

  if FSourceLine > 0 then
  begin
    FState.SourceTab := nstSource;
  end;
  FState.Compact := NyxStudioCompactHost(FHostSpace.Extent.Width, FHostSpace.Extent.Height);
  FState.RootRemoval := NyxNull;

  if CurrentBridge <> nil then
  begin
    CurrentBridge.ObserveResource(FState.ResourceSelection.Reference,
      FState.ResourceSelection.Locale, FState.ResourcesVisible);
  end;
  FState.Agents := GetAgentState;
  FState.BuildControlReady := (CurrentBridge <> nil) and
    CurrentBridge.CanCancelBuild;
  FActionButton := nil;
  FActionMenu := nil;
  FComponentHelp := nil;
  FState.PendingDesign := FSourceCommands.PendingDesign;
  FState.CompiledPreviewAvailable := CompiledPreviewCurrent(FCurrentProject);
  FState.CompiledPreviewRunning := (FCurrentProject <> nil) and
    (FCurrentProject.CompiledPreview <> nil) and FCurrentProject.CompiledPreview.Running;

  if FRootRemoval <> nil then
  begin
    FState.RootRemoval := FRootRemoval.Inspect;
  end;
  Result := BuildNyxStudioView(FSession, FState, FCompilerReport);
  FState.MenuEditorDraft.Restore(Result.Pages[0]);
  FState.MenuBarEditorDraft.Restore(Result.Pages[0]);
  FState.QueryEditorDraft.Restore(Result.Pages[0]);
  FState.TimeDomainEditorDraft.Restore(Result.Pages[0]);
  FState.ContentEditorDraft.Restore(Result.Pages[0]);
  FState.ThemeEditorDraft.Restore(Result.Pages[0]);
  FState.ImageEditorDraft.Restore(Result.Pages[0]);

  if not FState.ResourceRowsDraft.Restore(Result.Pages[0]) and
    (Result.Pages[0].Find('studio-resource-rows') <> nil) then
  begin
    FState.ResourceRowsDraft.Clear;
  end;

  if not FState.ResourceEditorDraft.Restore(Result.Pages[0]) and
    not (FSession.MatchesCommandContext(FShellCommandContext) and
      FState.ResourceEditorDraft.RestoreForOwnerSelection(Result.Pages[0])) and
    (Result.Pages[0].Find('studio-resource-editor') <> nil) then
  begin
    FState.ResourceEditorDraft.Clear;
  end;
  Result.Pages[0].Configure.Height(FHost.ClientHeight).Done;
end;

procedure TNyxNativeStudio.Paint;
var
  LShell: TNyxDocument;
  LCanvasHost: TWinControl;
  LCodeHost: TWinControl;
  LSourceHost: TWinControl;
  LSourceDocument: TNyxDocument;
  LOldCanvasHost: TWinControl;
  LOldSourceHost: TWinControl;
  LCanvasInputs: TNyxContentFaceStates;
  LSourceInputs: TNyxContentFaceStates;
  LCodeInputs: TNyxContentFaceStates;
  LCanvasCaptured: Boolean;
  LCodeCaptured: Boolean;
  LRecoveryFailure: TNyxText;
  LFocus: TWinControl;
  LSelection: TNyxTextSelection;
  LRetainFocus: Boolean;
  LCanvasFocus: Boolean;
  LSameView: Boolean;
  LChromeID: TNyxText;
  LSection: TNyxStudioSection;
  LCanvasFocusID: TNyxText;
  LChromeStateKey: TNyxText;
  LChromeEventOwner: TNyxText;
  LChromeEventTrigger: TNyxText;
  LChromeEventName: TNyxText;
  LChromeCollection: TNyxStudioCollectionChromeIdentity;
  LResourcePrepared: INyxPreparedPublication;
  LResourceCompatible: Boolean;
  {$ifdef NYX_STUDIO_PROFILE}
  LPhaseStarted: QWord;
  {$endif}

  procedure CaptureBorrowedInput(AView: TNyxLCLRenderer;
    out AInputs: TNyxContentFaceStates; const AName: TNyxText);
  begin

    if not AView.CaptureInteraction(AInputs) then
    begin
      raise ENyxModel.Create('Studio ' + AName + ' input boundary became busy');
    end;
  end;

  procedure RecoverBorrowedView(AView: TNyxLCLRenderer; AHost: TWinControl;
    const AInputs: TNyxContentFaceStates; const AName: TNyxText;
    AReplayInput: Boolean = True);
  begin
    { Hosts are borrowed from the still-admitted shell. Restore each owner once,
      even when another owner refuses recovery. CodeView is independently owned
      inside SourceView: returning the outer host cannot recover focus lost while
      its children were hidden. Copies contain no widget or session references. }

    if AHost <> nil then
    begin
      try
        AView.MoveHost(AHost);
      except
        on LException: Exception do
        begin
          LRecoveryFailure := LRecoveryFailure + ' / ' + AName + ' host: ' +
            LException.Message;
        end;
      end;
    end;

    if not AReplayInput then
    begin
      Exit;
    end;
    try

      if not AView.RestoreInteraction(AInputs) then
      begin
        raise ENyxModel.Create('Prior input boundary became busy');
      end;
    except
      on LException: Exception do
      begin
        LRecoveryFailure := LRecoveryFailure + ' / ' + AName + ' input: ' +
          LException.Message;
      end;
    end;
  end;
  {$ifdef NYX_STUDIO_PROFILE}
  procedure RecordPhase(const AName: TNyxText);
  var
    LNow: QWord;
  begin
    { Opt-in UI-thread evidence only. Emit static phase names and elapsed
      milliseconds; production contains no clock/output or authored values. }
    LNow := GetTickCount64;
    WriteLn('studio-ui,', AName, ',', LNow - LPhaseStarted);
    LPhaseStarted := LNow;
  end;
  {$endif}

  function EditingChromeIdentity(ANode: TNyxNode): TNyxText;
  var
    LIndex: Integer;
  begin
    Result := '';

    if (ANode.ID = 'project-title') or (ANode.Prop('prop-key') <> '') or
      (ANode.Prop(NyxStudioStateCommandKey) <> '') or
      ((ANode.Kind = NyxKindName(nkSelect)) and
        (ANode.Prop(NyxStudioEventCommandKey) <> '')) or
      { Collection buttons do not own an editing input. }
      (((ANode.Kind = NyxKindName(nkInput)) or
        (ANode.Kind = NyxKindName(nkMemo)) or (ANode.Kind = NyxKindName(nkSelect))) and
        (ANode.Prop(NyxStudioCollectionCommandKey) <> '')) or
      (ANode.ID = NyxStudioNewStateNameID) or (ANode.ID = NyxStudioNewStateValueID) then
    begin

      if FShellView.InputFor(ANode.ID) = LFocus then
      begin
        Exit(ANode.ID);
      end;
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Result := EditingChromeIdentity(ANode.Children[LIndex]);

      if Result <> '' then
      begin
        Exit;
      end;
    end;
  end;
begin

  if FPainting then
  begin
    RequestRefresh;
    Exit;
  end;
  FPainting := True;
  LShell := nil;
  {$ifdef NYX_STUDIO_PROFILE}LPhaseStarted := GetTickCount64;{$endif}
  try
    CapturePresentation;
    LFocus := Screen.ActiveControl;
    LRetainFocus := (LFocus <> nil) and
      ((FCodeView.Root <> nil) and (LFocus = FCodeView.InputFor('studio-code')));
    LCanvasFocus := (LFocus <> nil) and (FCanvasView.Root <> nil) and
      (FCanvasID = FSession.ActiveViewID) and
      InsideControl(LFocus, FCanvasView.ControlFor(FCanvasView.Root.ID));
    LCanvasFocusID := '';

    if LCanvasFocus then
    begin
      LCanvasFocusID := FCanvasView.InputIdentity(LFocus);
      LCanvasFocus := LCanvasFocusID <> '';
    end;
    LSelection := Default(TNyxTextSelection);
    LChromeID := '';
    LChromeStateKey := '';
    LChromeEventOwner := '';
    LChromeEventTrigger := '';
    LChromeEventName := '';
    LChromeCollection := Default(TNyxStudioCollectionChromeIdentity);

    if (LFocus <> nil) and (FShellView.Root <> nil) and
      InsideControl(LFocus, FShellView.ControlFor(FShellView.Root.ID)) then
    begin
      for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
      begin

        if FShellView.SectionRoot(LSection) <> nil then
        begin
          LChromeID := EditingChromeIdentity(FShellView.SectionRoot(LSection));
        end;

        if LChromeID <> '' then
        begin
          Break;
        end;
      end;

      if LChromeID <> '' then
      begin
        LChromeStateKey := FShellView.Root.Find(LChromeID).Prop(NyxStudioStateKey);
        LChromeEventOwner := FShellView.Root.Find(LChromeID).Prop(NyxStudioEventOwnerKey);
        LChromeEventTrigger := FShellView.Root.Find(LChromeID).Prop(NyxStudioEventTriggerKey);
        LChromeEventName := FShellView.Root.Find(LChromeID).Prop(NyxStudioEventNameKey);
        LChromeCollection := TNyxStudioCollectionChromeIdentity.FromNode(
          FShellView.Root.Find(LChromeID));
      end;
    end;

    if LRetainFocus or LCanvasFocus or (LChromeID <> '') then
    begin
      LSelection := CaptureNyxLCLSelection(LFocus);
    end;
    LSameView := FCanvasID = FSession.ActiveViewID;

    if not FSession.MatchesCommandContext(FCanvasRestoreContext) then
    begin
      FCanvasRestores := nil;
    end;
    LOldCanvasHost := nil;
    LOldSourceHost := nil;
    LCanvasCaptured := False;
    LCodeCaptured := False;

    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('paint-capture');{$endif}
    LShell := ComposeShell;
    LResourceCompatible := FResourceBrowser.Compatible(FSession);
    LResourcePrepared := FResourceBrowser.Prepare(FSession);
    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('paint-compose');{$endif}
    try

      if not LResourceCompatible or
        not FShellView.TryRefresh(LShell, LShell.Pages[0], False) then
      begin
        { A compatible shell owns the same borrowed hosts. Keep independent
          canvas/source views mounted there: parking would reparent and lay out
          every large-view control twice without changing accepted meaning.
          Only a full shell replacement needs parking before old hosts retire.
          Its failure path still restores those exact live hosts below. }

        { Capture before any hidden reparenting changes focus. In particular,
          SourceView cannot observe the separately routed CodeView's fields.
          A refused capture leaves all three independently owned views mounted. }

        { Design canvas controls are selection faces, with bindings deliberately
          inactive. Only an interacting preview admits runtime input snapshots;
          design faces still return to their exact host on failure. }

        if (FCanvasView.Root <> nil) and FPreview then
        begin
          CaptureBorrowedInput(FCanvasView, LCanvasInputs, 'canvas');
          LCanvasCaptured := True;
        end;

        if (FSourcePaneView.Root <> nil) and not FSourceModal.IsOpen then
        begin
          CaptureBorrowedInput(FSourcePaneView, LSourceInputs, 'source');

          if FCodeView.Root <> nil then
          begin
            CaptureBorrowedInput(FCodeView, LCodeInputs, 'code');
            LCodeCaptured := True;
          end;
        end;

        if FCanvasView.Root <> nil then
        begin
          LOldCanvasHost := FCanvasView.ControlFor(FCanvasView.Root.ID).Parent.Parent;
          FCanvasView.MoveHost(FCanvasParking);
        end;

        { Retained Chrome still owns the exact source port; moving it would
          recreate native handles without changing meaning. Only a full frame
          retirement parks this independently owned workspace. Modal content
          already has its own host and does not need parking. }

        if (FSourcePaneView.Root <> nil) and not FSourceModal.IsOpen then
        begin
          LOldSourceHost := FSourcePaneView.ControlFor(FSourcePaneView.Root.ID).Parent.Parent;
          FSourcePaneView.MoveHost(FSourcePaneParking);
        end;

        { Source/code already belong to their independent workspace, parked
          above or mounted in the modal. Retiring chrome cannot free them. }
        {$ifdef NYX_STUDIO_PROFILE}RecordPhase('paint-park');{$endif}
        FShellView.Render(LShell, LShell.Pages[0], FHost, False, nil,
          NyxStudioResourceContinuity(FSession.MatchesCommandContext(FShellCommandContext),
            FShellView.SectionRoot(nssResources), LShell.Pages[0]));
      end;
    except
      on LException: Exception do
      begin
        { Candidate shell admission retains old chrome. Return exact borrowed
          hosts, then replay their copied input through adapters that suppress
          callbacks. Recovery must not author a draft/history step or mask a
          second owner's refusal behind the first failed host operation. }
        LRecoveryFailure := '';

        if LOldCanvasHost <> nil then
        begin
          RecoverBorrowedView(FCanvasView, LOldCanvasHost, LCanvasInputs, 'canvas',
            LCanvasCaptured);
        end;

        if LOldSourceHost <> nil then
        begin
          RecoverBorrowedView(FSourcePaneView, LOldSourceHost, LSourceInputs, 'source');

          if LCodeCaptured then
          begin
            RecoverBorrowedView(FCodeView, nil, LCodeInputs, 'code');
          end;
        end;

        if LRecoveryFailure <> '' then
        begin
          raise ENyxModel.Create(LException.Message +
            ' / Studio borrowed view recovery failed' + LRecoveryFailure);
        end;
        raise;
      end;
    end;
    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('paint-shell-render');{$endif}
    FShell.Free;
    FShell := LShell;
    LShell := nil;
    FResourceBrowser.Mount(FShellView, LResourcePrepared,
      FState.ResourceBrowser, FState.ResourceSelection);

    if FShellView.Root.Find('studio-resources') <> nil then
    begin
      TScrollBox(FShellView.ControlFor('studio-resources')).VertScrollBar.Position :=
        FState.ResourcesScroll;
      RestoreResourcePaneScroll;
    end;
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

    if not LSameView or FReplaceCanvas then
    begin
      FCanvasMenus := nil;
    end;

    if (LCanvasHost <> nil) and (FSession.ActiveView <> nil) then
    begin

      if (FCanvasView.Root <> nil) and LSameView then
      begin
        FCanvasView.MoveHost(LCanvasHost);

        if FReplaceCanvas and
          not FCanvasView.TryRefresh(FSession.Document, FSession.ActiveView,
            not FPreview, FCanvasRestores) then
        begin
          FCanvasView.Render(FSession.Document, FSession.ActiveView, LCanvasHost,
            not FPreview, nil, nil, nrmAuthoredDefaults);
        end;
      end
      else
      begin
        FCanvasView.Render(FSession.Document, FSession.ActiveView, LCanvasHost,
          not FPreview, nil, nil, nrmAuthoredDefaults);
      end;

      if FPreview and (FCanvasMenus = nil) then
      begin
        FCanvasMenus := BindNyxLCLMenus(FSession.Document, FCanvasView);
      end;
      FCanvasView.Select(FSession.SelectedID);
      FState.PresentationSelection := FState.PresentationSelection.Reconciled(
        FCanvasView.Root.PresentationSnapshot);
      FCanvasView.PresentationSelection := FState.PresentationSelection;

      if not LSameView or FReplaceCanvas then
      begin
        FCanvasCommandContext := FSession.CommandContext;
      end;
      FCanvasID := FSession.ActiveViewID;

      if not FPreview and
        FState.PendingDesign.ApplyCanvasValues(FCanvasView.Root, FCanvasID) then
      begin
        FCanvasView.Sync;
      end;
    end
    else if not LSameView or FReplaceCanvas then
    begin
      { A hidden design changed; retire the old realization instead of claiming
        that stale controls represent the accepted document on the next switch. }
      FCanvasView.Unmount;
    end;
    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('paint-canvas');{$endif}

    { Never dereference the pre-refresh input after a possible full replacement.
      Resolve its exact runtime identity again; retained fields keep the same
      control and caret, while compatible replacements restore the text range. }

    if LCanvasFocus and (LCanvasHost <> nil) and (FCanvasView.Root <> nil) and
      (FCanvasView.Root.Find(LCanvasFocusID) <> nil) then
    begin
      LFocus := TWinControl(FCanvasView.InputFor(LCanvasFocusID, niRuntime));

      if (LFocus <> nil) and LFocus.CanFocus then
      begin
        LFocus.SetFocus;

        if LSelection.Defined then
        begin
          SelectNyxLCLText(LFocus, LSelection);
        end;
      end;
    end;

    if FShell.Find('studio-source-mount') <> nil then
    begin
      LSourceHost := TWinControl(FShellView.ControlFor('studio-source-mount'));

      if FState.SourceExpanded then
      begin
        FSourceModal.Show(NyxModal('Pascal source').Sizing(nhfAvailableHeight));
        LSourceHost := FSourceModal.Control;
      end
      else
      begin
        { Restore the exact owner's input state before reparenting focused
          native controls. LCL may focus the new form during SetParent; moving
          into a still-disabled owner can raise before the return completes.
          Hide retains this host and its children until the ordinary view moves. }
        FSourceModal.Hide;
      end;
      LSourceDocument := TNyxDocument.Create;
      try
        LSourceDocument.AddPage(BuildNyxStudioSourcePane(FSession, FState, FCompilerReport));
        { The retained renderer follows its current host, including native
          modal resizing. A captured pixel height would freeze this viewport. }
        LSourceDocument.Pages[0].Configure.HeightSizing(nsFill).Done;

        if not FSourcePaneView.TryRefresh(LSourceDocument, LSourceDocument.Pages[0], False) then
        begin

          if FCodeView.Root <> nil then
          begin
            { A diagnostics/status structure change can rebuild source chrome
              while expanded. Park inside the same enabled modal window, rather
              than moving focused controls into its disabled background owner. }
            FCodeParking.Parent := LSourceHost;
            FCodeView.MoveHost(FCodeParking);
          end;
          FSourcePaneView.Render(LSourceDocument, LSourceDocument.Pages[0], LSourceHost);
        end
        else
        begin
          FSourcePaneView.MoveHost(LSourceHost);
        end;
        FSourcePaneDocument.Free;
        FSourcePaneDocument := LSourceDocument;
        LSourceDocument := nil;
      finally
        LSourceDocument.Free;
      end;

      LCodeHost := TWinControl(FSourcePaneView.ControlFor('studio-code-host'));
    end
    else
    begin
      FSourceModal.Hide;

      if FSourcePaneView.Root <> nil then
      begin
        FSourcePaneView.MoveHost(FSourcePaneParking);
      end;
    end;

    if LCodeHost <> nil then
    begin

      if FCodeView.Root = nil then
      begin
        FreeAndNil(FCodeDocument);
        FCodeDocument := NewNyxStudioCodeDocument(FSession.DraftSource);
        FCodeDocument.Pages[0].Configure.HeightSizing(nsFill).Done;
        FCodeView.Render(FCodeDocument, FCodeDocument.Pages[0], LCodeHost);
      end
      else
      begin
        FCodeView.Root.Configure.Value(FSession.DraftSource)
          .Clear(atHeight).HeightSizing(nsFill).Done;
        FCodeView.Sync;
        FCodeView.MoveHost(LCodeHost);
      end;
      { This temporary empty parking host must leave the modal before a later
        MoveHost admission requires its target to contain no other controls. }
      FCodeParking.Parent := FHost;

      if FSourceFocusPending and (FState.SourceTab = nstSource) then
      begin
        TWinControl(FCodeView.InputFor('studio-code')).SetFocus;
        FSourceFocusPending := False;
      end;

      if FSourceLine > 0 then
      begin
        FCodeView.NavigateCodeLine('studio-code', FSourceLine, FSourceColumn);
        { A deliberate admitted-handler/diagnostic navigation owns focus. An
          earlier policy field must not reclaim it after the source caret moves. }
        LChromeID := '';
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
    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('paint-source');{$endif}
    { Chrome is independently regenerated while a worker prepares edits. Restore
      the same inspector/title field by identity and Unicode selection, never
      retain its destroyed widget pointer across shell replacement. Pending
      values are already present in ComposeShell's immutable presentation. }

    if (LChromeID <> '') and (LChromeStateKey <> '') and
      ((FShell.Find(LChromeID) = nil) or
        (FShell.Find(LChromeID).Prop(NyxStudioStateKey) <> LChromeStateKey)) then
    begin
      { Removing a state can reuse its positional chrome ID for a different
        row. Restore only the original exact identity, never that replacement. }
      LChromeID := '';
    end;

    if (LChromeID <> '') and (FShell.Find(LChromeID) <> nil) then
    begin
      { Removed rows/columns can reuse their positional control ID. Retain
        focus only for the full original collection/field/item/owner tuple. }

      if LChromeCollection.Defined and
        not LChromeCollection.Matches(FShell.Find(LChromeID)) then
      begin
        LChromeID := '';
      end;
    end;

    if (LChromeID <> '') and (FShell.Find(LChromeID) <> nil) then
    begin

      if (LChromeEventOwner <> '') and
        ((FShell.Find(LChromeID).Prop(NyxStudioEventOwnerKey) <> LChromeEventOwner) or
        (FShell.Find(LChromeID).Prop(NyxStudioEventTriggerKey) <> LChromeEventTrigger) or
        (FShell.Find(LChromeID).Prop(NyxStudioEventNameKey) <> LChromeEventName)) then
      begin
        { Named event metadata can reuse a positional card ID. Owner, trigger
          and exact open event name must all match before restoring focus. }
        LChromeID := '';
      end;
    end;

    if (LChromeID <> '') and (FShell.Find(LChromeID) <> nil) then
    begin
      LFocus := FShellView.FocusFor(LChromeID);

      if (LFocus <> nil) and LFocus.CanFocus then
      begin
        LFocus.SetFocus;

        if LSelection.Defined then
        begin
          SelectNyxLCLText(LFocus, LSelection);
        end;
      end;
    end;
    FReplaceCanvas := False;
    FCanvasRestores := nil;
    RestoreProjectControls;
    FShellCommandContext := FSession.CommandContext;
    { The hierarchy belongs to the independent Inspector scope. Retire the
      borrowed receiver before binding the current mounted view. }

    if FHierarchySubscription <> nil then
    begin
      FHierarchySubscription.Cancel;
      FHierarchySubscription := nil;
    end;

    if FShellView.RootFor(NyxStudioHierarchyID) <> nil then
    begin
      FHierarchySubscription := SubscribeNyxStudioHierarchy(
        FShellView.ViewFor(NyxStudioHierarchyID).Events, HierarchyEvent);
    end;
    PrepareActionMenu;
    FShellView.ConnectSources(FDesignerDrag, FShellCommandContext);
    FDesignerResize.Connect(FShellView.SectionEvents(nssInspector),
      FShellView.SectionRoot(nssInspector), FShellCommandContext);
    FDesignerMove.Connect(FShellView.SectionEvents(nssInspector),
      FShellView.SectionRoot(nssInspector), FShellCommandContext);

    { A parked canvas retains its former face. Reconnect guides only after the
      visible mount above has synchronized the current authored selection. }

    if FCanvasView.DesignMode and (LCanvasHost <> nil) then
    begin
      FCanvasView.AttachResizeGrips(FDesignerResize.CanvasGrips);
      FCanvasView.AttachMoveGrip(FDesignerMove.CanvasGrip);
    end
    else
    begin
      FCanvasView.AttachResizeGrips(nil);
      FCanvasView.AttachMoveGrip(nil);
    end;
    FChangingProject := False;

    if FState.DisplayRecovery.BlocksInput then
    begin
      FState.DisplayRecovery := TNyxViewRecovery.Ready;
      FState.Status := 'Display refreshed / accepted files retained';
      SyncDisplayRecovery;
    end;
    Inc(FPaintCount);
    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('paint-finish');{$endif}
  finally
    LShell.Free;
    FPainting := False;
  end;
end;

procedure TNyxNativeStudio.SourceCommandChanged(AState: TNyxSourceCommandState;
  const AMessage: TNyxText);
var
  LRestore: TNyxProjectionValueRestore;
  LRestoreCanvas: Boolean;
  LCreatedName: TNyxText;
  LNameField: TNyxNode;
  LEvent: TNyxStudioEventIntent;
  LOwner: TNyxText;
  LView: TNyxText;
  LHandler: TNyxHandlerRef;
begin
  FState.Status := AMessage;
  FState.SourceStatus := AMessage;

  if FSourceCommands.CompletedEvent(LEvent, LOwner, LView, LHandler) then
  begin

    if (LEvent.Action = seaAdd) and (LHandler.Name <> '') and
      (FSession.SelectedID = LOwner) and (FSession.ActiveViewID = LView) then
    begin
      FSourceLine := FSession.CallbackLine(LHandler);
      FSourceColumn := 1;
      FState.CodeVisible := True;
      FState.Panel := nspDesign;
    end;

    if (LEvent.Action = seaRemove) and FState.CallbackRemoval.Pending and
      (FState.CallbackRemoval.OwnerID = LOwner) and
      (FState.CallbackRemoval.Trigger = LEvent.Trigger) and
      (FState.CallbackRemoval.Name.Name = LEvent.Name.Name) and
      (FState.CallbackRemoval.ID.Name = LEvent.ID.Name) and
      (FState.CallbackRemoval.Handler.Name = LEvent.Handler.Name) then
    begin
      FState.CallbackRemoval.Pending := False;
    end;
  end;

  if FSourceCommands.NewDefaultCreated(LCreatedName) and (FShellView.Root <> nil) then
  begin
    LNameField := FShellView.Root.Find(NyxStudioNewStateNameID);

    if (LNameField <> nil) and (LNameField.Prop('value') = LCreatedName) then
    begin
      { CapturePresentation reads this owned node before the deferred paint.
        Clear only the exact submitted name; a later value remains untouched. }
      LNameField.Configure.Value('').Done;
      FState.NewStateName := '';
    end;
  end;
  { Palette/page completion follows its owning project's structural result.
    Status-only and inspector callbacks retain the user's chosen panel. }

  if FSourceCommands.PublishedDesign and
    (FSourceCommands.PublishedAction in [sdaAddKind, sdaAddInstance, sdaAddPage,
      sdaCreateComponent]) then
  begin
    FState.Panel := nspDesign;
  end;
  { Every connected project's own callback records its pair. An offline editor
    has no bridge. Completion never consults a newly selected project. }
  LRestoreCanvas := FSourceCommands.CanvasRestore(LRestore);

  if LRestoreCanvas then
  begin
    RememberCanvasRestore(LRestore);
  end;
  RequestRefresh((AState = nssApplied) or LRestoreCanvas);
end;

procedure TNyxNativeStudio.RememberCanvasRestore(const ARestore: TNyxProjectionValueRestore);
var
  LIndex: Integer;
  LCount: Integer;
begin

  if not FSession.MatchesCommandContext(FCanvasRestoreContext) then
  begin
    FCanvasRestores := nil;
    FCanvasRestoreContext := FSession.CommandContext;
  end;
  for LIndex := 0 to High(FCanvasRestores) do
  begin

    if (FCanvasRestores[LIndex].RuntimeID = ARestore.RuntimeID) and
      (FCanvasRestores[LIndex].DesignID = ARestore.DesignID) then
    begin
      Exit;
    end;
  end;
  LCount := Length(FCanvasRestores);
  SetLength(FCanvasRestores, LCount + 1);
  FCanvasRestores[LCount] := ARestore;
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
var
  LProposal: TNyxStudioDesignEdit;
begin

  if FPainting or FChangingProject or FState.DisplayRecovery.BlocksInput then
  begin
    Exit;
  end;
  LProposal := Default(TNyxStudioDesignEdit);
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
          LProposal := FSession.CaptureCanvasValue(ANode, npfNativeLCL, FCanvasCommandContext);
          FSourceCommands.Edit(LProposal);
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

      if LProposal.Action = sdaCanvasValue then
      begin
        RememberCanvasRestore(TNyxProjectionValueRestore.ForField(LProposal.Name,
          LProposal.Selection));
      end;
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

function TNyxNativeStudio.DesignerDragContext: TNyxStudioDragContext;
begin
  Result := Default(TNyxStudioDragContext);
  Result.Session := FSession;
  Result.Commands := FSourceCommands;
  Result.SourceMount := FShellCommandContext;
  Result.CanvasMount := FCanvasCommandContext;
  Result.Designing := not FPreview and not FState.DisplayRecovery.BlocksInput;
  Result.Placement := FState.DesignerPlacement;
  Result.AutomaticPlacement := FState.DesignerAutomaticPlacement;
end;

function TNyxNativeStudio.DesignerMovePoint(const APointer: TNyxPointerSnapshot): TNyxResizePoint;
begin
  Result := FCanvasView.LogicalPointFor(FSession.SelectedID,
    FShellView.ScreenPointFor(NyxStudioMoveGripID, APointer), niDesign);
end;

function TNyxNativeStudio.DesignerResizeMeasure(const AControl: TNyxControlRef): TNyxResizeSize;
begin
  Result := FCanvasView.SizeFor(AControl.ID, niDesign);
end;

function TNyxNativeStudio.DesignerResizeGuides(const AControl: TNyxControlRef): TNyxAlignmentContext;
begin
  Result := FCanvasView.AlignmentFor(AControl.ID, niDesign);
end;

procedure TNyxNativeStudio.DesignerResizePresentation(const APreview: TNyxResizePreview);
begin

  if FCanvasView <> nil then
  begin
    FCanvasView.PreviewResize(APreview);
  end;
end;

procedure TNyxNativeStudio.DesignerResizeStatus(const AMessage: TNyxText);
var
  LCaption: TNyxNode;
begin
  FState.Status := AMessage;

  if FShellView.Root <> nil then
  begin
    LCaption := FShellView.Root.Find('studio-status');

    if LCaption <> nil then
    begin
      LCaption.Configure.Text(AMessage).Done;
      FShellView.Sync;
    end;
  end;
end;

procedure TNyxNativeStudio.DesignerDragFeedback(const ATarget: TNyxControlRef);
begin

  if (FCanvasView.Root = nil) or FPreview then
  begin
    Exit;
  end;

  if ATarget.ID = '' then
  begin
    FCanvasView.Select(FSession.SelectedID);
  end
  else
  begin
    FCanvasView.Select(ATarget.ID);
  end;
end;

procedure TNyxNativeStudio.DesignerPlacementFeedback(const APreview: TNyxDropPreview);
begin

  if (FCanvasView <> nil) and (FCanvasView.Root <> nil) and not FPreview then
  begin
    FCanvasView.PreviewDrop(APreview);

    if APreview.Active then
    begin
      DesignerResizeStatus(APreview.Caption);
    end
    else
    begin
      DesignerResizeStatus('Ready to design');
    end;
  end;
end;

procedure TNyxNativeStudio.DesignerGesture(const ATarget: TNyxDesignerTarget;
  const AEvent: TNyxEventInfo; const ADecision: INyxGestureDecision);
begin
  FDesignerDrag.Gesture(ATarget, AEvent, ADecision);
end;

procedure TNyxNativeStudio.MenuAction(AAction: TNyxStudioMenuAction);
var
  LNode: TNyxNode;
  LEvent: TNyxEventInfo;
begin

  if AAction = smaHelp then
  begin
    ShowComponentHelp(NyxControl(NyxStudioActionMenuID));
    Exit;
  end;

  if AAction in [smaProperties, smaEvents] then
  begin
    FState.Panel := nspInspector;
    FState.InspectorTab := nitProperties;
    FState.CanvasExpanded := False;

    if AAction = smaEvents then
    begin
      FState.InspectorTab := nitEvents;
    end;
    RequestRefresh;
    Exit;
  end;
  LNode := FShellView.Root.Find(NyxStudioMenuActionTarget(AAction));

  if LNode <> nil then
  begin
    LEvent := Default(TNyxEventInfo);
    LEvent.Value := NyxNull;
    LEvent.Trigger := ntClick;
    ShellEvent(LNode, LEvent);
  end;

  if LNode = nil then
  begin
    LNode := TNyxNode.Create(nkButton, NyxStudioMenuActionTarget(AAction));
    try
      LEvent := Default(TNyxEventInfo);
      LEvent.Value := NyxNull;
      LEvent.Trigger := ntClick;
      ShellEvent(LNode, LEvent);
    finally
      LNode.Free;
    end;
  end;
end;

procedure TNyxNativeStudio.PrepareActionMenu;
var
  LContent: TNyxDocument;
  LItems: TNyxMenuItems;
begin
  LContent := BuildNyxStudioActionMenu(FSession, LItems, True,
    FState.Agents.CanControlBuilds);
  try
    FActionMenu := NewNyxLCLMenu(FShellView.FocusFor(NyxStudioActionMenuID),
      LContent, NyxPageRoot(NyxStudioActionMenuRoot), LItems, FTheme);
    FActionMenu.OnInvoke.Subscribe(NewNyxStudioMenuCallback(MenuAction));
    FActionButton := NewNyxMenuButton(RetainNyxControl(
      FShellView.Root.Find(NyxStudioActionMenuID)) as INyxButton,
      FShellView.ViewFor(NyxStudioActionMenuID).Events, FActionMenu, NyxMenu('Component actions'));
  finally
    LContent.Free;
  end;
end;

procedure TNyxNativeStudio.ShowComponentHelp(const AAnchor: TNyxControlRef);
var
  LHelp: TNyxDocument;
begin
  LHelp := BuildNyxStudioComponentHelp(FSession);
  try

    if LHelp <> nil then
    begin
      FComponentHelp := nil;
      FComponentHelp := NewNyxLCLPopover(FShellView.FocusFor(AAnchor.ID), LHelp,
        NyxPageRoot(NyxComponentHelpRootID), FTheme);
      FComponentHelp.Open(NyxPopover('About this component')
        .Size(380, 300).Focus(NyxPart('close')));
    end;
  finally
    LHelp.Free;
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
  LContentDraft: TNyxContentEditorDraft;
  LThemeDraft: TNyxThemeEditorDraft;
  LImageEditor: TNyxNode;
  LImageAction: TNyxImageEditorAction;
  LImageSource: TNyxImageSource;
  LResourceEditor: TNyxNode;
  LResourceAction: TNyxResourceEditorAction;
  LResourceSelection: TNyxResourceEditorSelection;
  LResourceProjection: TNyxNode;
  LRetainedResourceSelection: Boolean;
  LResourceIntent: TNyxStudioResourceBrowseIntent;
  LResourcePane: TNyxResourceWorkspacePane;
  LContentFocus: TWinControl;
begin

  if not FPainting and not FChangingProject and
    (AEvent.Trigger = ntClick) and NyxViewRecoveryAction(ANode, NyxStudioDisplayRecoveryID) then
  begin

    if (FState.DisplayRecovery.Phase = nvrFailed) and
      FSession.MatchesCommandContext(FDisplayRecoveryContext) and
      (FShellView.Root.Find(ANode.ID) = ANode) then
    begin
      FState.DisplayRecovery := TNyxViewRecovery.Retrying(FState.DisplayRecovery.Diagnostic);
      FState.Status := 'Refreshing the accepted display';
      RequestRefresh(True);
    end;
    Exit;
  end;

  if FPainting or FChangingProject or
    not FSession.MatchesCommandContext(FShellCommandContext) then
  begin
    Exit;
  end;

  if (ANode.ID = NyxStudioActionMenuID) and (AEvent.Trigger = ntClick) then
  begin
    FComponentHelp := nil;
    Exit;
  end;

  if (ANode.ID = NyxStudioComponentHelpID) and (AEvent.Trigger = ntClick) then
  begin
    ShowComponentHelp(NyxControl(NyxStudioComponentHelpID));
    Exit;
  end;
  LChanged := False;
  try

    if (CurrentBridge <> nil) and CurrentBridge.RouteBuildCancel(ANode, AEvent.Trigger) then
    begin
      FState.Status := 'Cancellation requested / waiting for compiler retirement';
      RequestRefresh;
      Exit;
    end;

    if (ANode.ID = NyxStudioDropPositionID) and (AEvent.Trigger = ntChange) then
    begin
      FDesignerDrag.Cancel;
      ReadNyxStudioPlacementChoice(ANode.Prop('value'), FState.DesignerPlacement,
        FState.DesignerAutomaticPlacement);
      Exit;
    end;

    if (ANode.ID = NyxStudioPresentationPreviewID) and (AEvent.Trigger = ntChange) then
    begin
      FDesignerDrag.Cancel;
      FDesignerResize.Cancel;
      FDesignerMove.Cancel;
      FState.PresentationSelection := ReadNyxStudioPresentationChoice(ANode.Prop('value'),
        FSession.Document.Presentations);
      FCanvasView.PresentationSelection := FState.PresentationSelection;
      FState.Status := NyxStudioPresentationChoice(FState.PresentationSelection);
      Exit;
    end;

    if RouteNyxStudioHierarchy(FSession, ANode, AEvent, LHierarchyChanged) then
    begin

      if LHierarchyChanged then
      begin
        RecordLocal;
        RequestRefresh;
      end;
      Exit;
    end;

    if (ANode.ID = NyxStudioBindingFlowID) and (AEvent.Trigger = ntChange) then
    begin
      { An unbound flow choice is presentation only. Retain it even when the
        queue consumes that event; bound choices additionally submit intent. }

      if not TryNyxStudioBindingDirection(ANode.Prop('value'), FState.BindingDirection) then
      begin
        raise ENyxModel.Create('Unknown binding flow');
      end;
    end;

    if (AEvent.Trigger = ntClick) and CaptureNyxContentRuleInspector(FSession,
      ANode, FShellView.RootFor(ANode.ID), LContentDraft) then
    begin
      FState.ContentEditorDraft := LContentDraft;

      if not FState.ContentEditorDraft.Restore(FShellView.RootFor('inspector-content')) then
      begin
        raise ENyxModel.Create('Recipe form changed before the selected choice could be loaded');
      end;
      FShellView.Sync;
      LContentFocus := FShellView.FocusFor(
        NyxContentEditorFieldID('inspector-content', ncfScope));

      if (LContentFocus <> nil) and LContentFocus.CanFocus then
      begin
        LContentFocus.SetFocus;
      end;
      Exit;
    end;

    if FResourceBrowser.Handle(ANode, AEvent, FShellView, FSession,
      FState.ResourceBrowser, LResourceIntent, LResourceSelection) then
    begin
      case LResourceIntent of
        rbiWorkspace:
          begin
            FState.ResourcesVisible := True;
            FState.ResourcePane := rwpFiles;
            FState.CanvasExpanded := False;
            RequestRefresh;
          end;
        rbiOpen:
          begin
            { Catalog Open has the same proposal lifetime as the public form's
              New/Open commands. Cancel the previous import before replacing
              fields; native picker replies retain their exact guards too. }

            if FResourcePicker <> nil then
            begin
              FResourcePicker.Cancel;
            end;
            LResourceEditor := FShellView.Root.Find('studio-resource-editor');
            LResourceProjection := FSession.SelectedProjection;
            try
              LRetainedResourceSelection := TrySelectNyxResourceEditor(LResourceEditor,
                FSession.Document.Resources, LResourceSelection, FSession.Selected,
                LResourceProjection, FShellView.Sync);
            finally
              LResourceProjection.Free;
            end;
            FState.ResourceEditorDraft.Clear;
            FState.ResourceSelection := LResourceSelection;
            { A retained form does not require a new shell composition. Keep
              the observation selector current at this successful handoff. }

            if CurrentBridge <> nil then
            begin
              CurrentBridge.ObserveResource(LResourceSelection.Reference,
                LResourceSelection.Locale, FState.ResourcesVisible);
            end;
            SelectResourcePane(rwpEditor);

            if LRetainedResourceSelection then
            begin
              FState.ResourceEditorDraft.Capture('studio-resource-editor',
                FShellView.RootFor('studio-resource-editor'));
              { Clear the previous variant's observation through an ordinary
                retained paint, preserving these proposal controls. }
              RequestRefresh;
            end
            else
            begin
              RequestRefresh;
            end;
          end;
        rbiNone:
          begin
            { Runtime filtering/selection already synchronized its owned views. }
          end;
      end;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and NyxResourceWorkspaceAction(ANode,
      NyxStudioResourceWorkspaceID, LResourcePane) then
    begin
      SelectResourcePane(LResourcePane);
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and (ANode.ID = 'action-resources-close') then
    begin
      FState.ResourcesVisible := False;
      FState.Panel := nspDesign;
      RequestRefresh;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and (ANode.ID = 'action-resources-toggle') then
    begin
      FState.ResourcesVisible := not FState.ResourcesVisible;
      FState.CanvasExpanded := False;
      RequestRefresh;
      Exit;
    end;

    if ((AEvent.Trigger = ntChange) and
      NyxResourceRowsInput(ANode, FShellView.RootFor(ANode.ID), LResourceEditor)) or
      ((AEvent.Trigger = ntClick) and HandleNyxResourceRowsEditor(ANode, FShellView.RootFor(ANode.ID))) then
    begin
      FState.ResourceRowsDraft.Capture('studio-resource-rows', FShellView.RootFor('studio-resource-rows'));
      FShellView.Sync;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and HandleNyxResourceEditorLabels(ANode,
      FShellView.RootFor(ANode.ID), LResourceEditor) then
    begin
      FState.ResourceEditorDraft.Capture('studio-resource-editor',
        FShellView.RootFor('studio-resource-editor'));
      FShellView.Sync;
      Exit;
    end;

    if (AEvent.Trigger = ntChange) and
      NyxResourceEditorInput(ANode, FShellView.RootFor(ANode.ID), LResourceEditor) then
    begin
      RefreshNyxResourceEditor(LResourceEditor, False);
      FState.ResourceEditorDraft.Capture('studio-resource-editor', FShellView.RootFor('studio-resource-editor'));
      FShellView.Sync;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and
      NyxResourceEditorAction(ANode, FShellView.RootFor(ANode.ID), LResourceEditor,
        LResourceAction, LResourceSelection) and
      (LResourceAction in [reaNew, reaOpen, reaImport, reaPreview]) then
    begin

      if FResourcePicker <> nil then
      begin
        FResourcePicker.Cancel;
      end;
      case LResourceAction of
        reaNew, reaOpen:
          begin
            { Proposal navigation retains the public form and native controls.
              A synchronization exception keeps the former draft; changed
              context still uses ordinary staged shell replacement. }
            LResourceProjection := FSession.SelectedProjection;
            try
              LRetainedResourceSelection := TrySelectNyxResourceEditor(LResourceEditor,
                FSession.Document.Resources, LResourceSelection, FSession.Selected,
                LResourceProjection, FShellView.Sync);
            finally
              LResourceProjection.Free;
            end;
            FState.ResourceEditorDraft.Clear;
            FState.ResourceSelection := LResourceSelection;

            if CurrentBridge <> nil then
            begin
              CurrentBridge.ObserveResource(LResourceSelection.Reference,
                LResourceSelection.Locale, FState.ResourcesVisible);
            end;

            if LRetainedResourceSelection then
            begin
              FState.ResourceEditorDraft.Capture('studio-resource-editor', FShellView.RootFor('studio-resource-editor'));
              RequestRefresh;
            end
            else
            begin
              RequestRefresh;
            end;
          end;
        reaPreview:
          begin
            RefreshNyxResourceEditor(LResourceEditor, True);
            FState.ResourceEditorDraft.Capture('studio-resource-editor', FShellView.RootFor('studio-resource-editor'));
            FShellView.Sync;
          end;
        reaImport:
          begin

            if FResourcePicker = nil then
            begin
              FResourcePicker := CreateResourcePicker;
            end;
            FState.ResourceEditorDraft.Capture('studio-resource-editor', FShellView.RootFor('studio-resource-editor'));
            FResourcePickDraft := FState.ResourceEditorDraft.ToData;
            FResourcePickContext := FSession.CommandContext;
            FResourcePicker.Pick(NyxResourceEditorKind(LResourceEditor), ResourcePicked);
          end;
        reaApply, reaRemove:
          begin
            { The ordinary isolated source command owns these admissions. }
          end;
      end;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and (ANode.ID = 'action-theme-toggle') then
    begin
      FState.ThemeVisible := not FState.ThemeVisible;
      RequestRefresh;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and PrepareNyxStudioThemePreset(FSession,
      ANode, FShellView.RootFor(ANode.ID), LThemeDraft) then
    begin
      FState.ThemeEditorDraft := LThemeDraft;

      if not FState.ThemeEditorDraft.Restore(FShellView.RootFor('studio-theme-editor')) then
      begin
        raise ENyxModel.Create('Theme form changed before the palette could be loaded');
      end;
      FShellView.Sync;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and
      NyxImageEditorAction(ANode, FShellView.RootFor(ANode.ID), LImageEditor, LImageAction) and
      (LImageAction in [ieaImport, ieaInline, ieaClear]) then
    begin
      FState.ImageEditorDraft.Capture('inspector-image', FShellView.RootFor('inspector-image'));

      if FImagePicker = nil then
      begin
        FImagePicker := CreateImagePicker;
      end;
      FImagePicker.Cancel;

      if LImageAction = ieaClear then
      begin
        FState.ImageEditorDraft.Propose(NyxNoImage);
        FState.ImageEditorDraft.Restore(FShellView.RootFor('inspector-image'));
        FShellView.Sync;
      end
      else if LImageAction = ieaInline then
      begin
        LImageSource := ReadNyxImageEditorInline(LImageEditor);
        ValidateNyxLCLImageSource(LImageSource);
        FState.ImageEditorDraft.Propose(LImageSource);
        FState.ImageEditorDraft.Restore(FShellView.RootFor('inspector-image'));
        FShellView.Sync;
      end
      else
      begin
        FImagePickContext := FSession.CommandContext;
        FImagePickOwner := FSession.SelectedID;
        FImagePickBaseline := NyxImageEditorContext(LImageEditor);
        FImagePicker.Pick(ImagePicked, ReadNyxImageEditorValidation(LImageEditor));
      end;
      Exit;
    end;

    if FSourceCommands.Route(ANode, AEvent, FShellView.RootFor(ANode.ID)) then
    begin
      Exit;
    end;
    { Event presentation is immediate; only copied add/policy/confirmed-removal
      intent enters independent preparation. No full pair snapshot is needed
      merely to show a warning or navigate an existing implementation. }

    if FSourceCommands.RouteEvents(ANode, AEvent.Trigger,
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
            { Mutation completion owns any later source-navigation effect. }
          end;
      end;
      RequestRefresh;
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

    if (ANode.ID = 'studio-details-split') and (AEvent.Trigger = ntChange) then
    begin
      FState.DetailsPercent := StrToIntDef(ANode.Prop('split-position'), FState.DetailsPercent);
      Exit;
    end;

    if RouteNyxMenuInspectorChoice(FSession, ANode, FShellView.RootFor(ANode.ID),
      AEvent.Trigger, FState.MenuEditorReference) then
    begin
      RequestRefresh;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and RouteNyxStudioWorkspace(FState, ANode.ID) then
    begin
      RequestRefresh;
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
    else if RouteNyxStudioSource(FSession, ANode, AEvent.Trigger) or
      RouteNyxStudioProperty(FSession, ANode, AEvent.Trigger) or
      RouteNyxStudioAuthoring(FSession, ANode, AEvent.Trigger, FShellView.RootFor(ANode.ID)) then
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
          ncSourceTab:
            begin
              FState.SourceTab := nstSource;
              FSourceFocusPending := True;
            end;
          ncMessagesTab:
            begin
              FState.SourceTab := nstMessages;
            end;
          ncExpandSource:
            begin
              FState.SourceExpanded := not FState.SourceExpanded;
              FState.SourceTab := nstSource;
              FSourceFocusPending := True;
            end;
          ncCode:
            begin
              FState.CodeVisible := not FState.CodeVisible;
              FState.CanvasExpanded := False;
              FState.Panel := nspDesign;
            end;
          ncOutputs:
            begin
              FState.OutputVisible := not FState.OutputVisible;
              FState.DetailsExpanded := FState.OutputVisible;
              FState.CanvasExpanded := False;
              FState.Panel := nspDesign;

              if FState.OutputVisible and (CurrentBridge <> nil) and
                CurrentBridge.State.CanBuild and not FOutputLoaded and not FOutputLoading then
              begin
                ReadOutputs(FCurrentProject, False);
              end;
            end;
          ncFiles:
            begin
              FState.FilesVisible := not FState.FilesVisible;
              FState.CanvasExpanded := False;
              FState.Panel := nspProject;
            end;
          ncAdvanced:
            begin
              FState.AdvancedProperties := not FState.AdvancedProperties;
            end;
          ncDesignPanel:
            begin
              FState.ResourcesVisible := False;
              FState.Panel := nspDesign;
            end;
          ncProjectPanel:
            begin
              FState.ResourcesVisible := False;
              FState.Panel := nspProject;
              FState.CanvasExpanded := False;
            end;
          ncInspectorPanel:
            begin
              FState.ResourcesVisible := False;
              FState.Panel := nspInspector;
              FState.CanvasExpanded := False;
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
              FState.DetailsExpanded := FState.AgentsVisible;
              FState.CanvasExpanded := False;
              FState.Panel := nspDesign;
            end;
          ncBuilds:
            begin
              FState.BuildsVisible := not FState.BuildsVisible;
              FState.DetailsExpanded := FState.BuildsVisible;
              FState.CanvasExpanded := False;
              FState.Panel := nspDesign;
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
