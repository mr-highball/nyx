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
  nyx.studio.sourcecompilation.shared,
  nyx.studio.sections, nyx.studio.section.views,
  Classes, SysUtils, Forms, Controls, ExtCtrls,
  nyx.text, nyx.types, nyx.behavior, nyx.data, nyx.model, nyx.disclosure, nyx.theme, nyx.render.lcl,
  nyx.events, nyx.viewport, nyx.projection.refresh, nyx.callbacks, nyx.studio.collections,
  nyx.hostspace, nyx.hostspace.lcl,
  nyx.content.editor, nyx.content.mount, nyx.view.recovery,
  nyx.theme.editor, nyx.studio.theme, nyx.studio.chrome,
  nyx.images, nyx.image.editor, nyx.image.import, nyx.image.import.lcl,
  nyx.resources, nyx.resources.editor, nyx.resources.rows.editor, nyx.logic.editor,
  nyx.programs.editor, nyx.programs.declarations, nyx.diagrams.palette,
  nyx.diagram.editor, nyx.diagram.editor.lcl, nyx.diagram.declarations,
  nyx.diagram, nyx.diagram.adoption,
  nyx.flow.rows.editor, nyx.flow.controls.editor,
  nyx.resources.browser, nyx.resources.workspace, nyx.studio.resource.browser, nyx.publication,
  nyx.resource.context, nyx.studio.inspector.groups, nyx.event.discovery,
  nyx.reusable.editor, nyx.expressions.editor,
  nyx.resources.import, nyx.resources.import.lcl,
  nyx.studio.help, nyx.component.help, nyx.root.types,
  nyx.popover, nyx.popover.lcl,
  nyx.menu, nyx.menu.lcl, nyx.menu.button, nyx.controls, nyx.studio.menu,
  nyx.studio.session, nyx.studio.view, nyx.studio.projects, nyx.studio.files,
  nyx.files, nyx.files.lcl,
  nyx.studio.projectstore, nyx.studio.outputs, nyx.studio.sourceconfiguration,
  nyx.studio.rootedits,
  nyx.studio.compiler, nyx.studio.agentbridge, nyx.studio.agentview,
  nyx.studio.projectnavigation,
  nyx.studio.sourceinput, nyx.editing, nyx.editing.history, nyx.studio.completion,
  nyx.pascal.completion, nyx.pascal.drafts,
  nyx.studio.workspaces, nyx.studio.builds, nyx.studio.editorbuild,
  nyx.studio.exchange, nyx.studio.preview, nyx.studio.preview.lcl,
  nyx.studio.sourcejobs, nyx.studio.sourcecompilation, nyx.modal, nyx.modal.lcl,
  nyx.studio.sourceoutline,
  nyx.studio.previewruntime, nyx.canvas.palette,
  nyx.designer.input, nyx.gestures, nyx.studio.edits, nyx.studio.drag,
  nyx.designer.placement,
  nyx.designer.resize, nyx.designer.guides, nyx.studio.resize, nyx.presentations, nyx.studio.move;

type
  { Forward owner for project contexts that borrow the native editor. The full
    declaration below owns its projections, sessions and configuration state. }
  TNyxNativeStudio = class;
  { Native compiler/profile/preview phases, separate from source publication.
    Profile synchronization can run without selecting or compiling an output. }
  TNyxNativeBuildStage = (nbsIdle, nbsProfile, nbsOutputs, nbsRequest, nbsPolling,
    nbsPreviewInspect, nbsPreviewDownload, nbsPreviewActivate, nbsTerminal, nbsLaunchInspect);

  { Private native context owns one portable mirror and its immutable service
    bridge. Owner is borrowed. Views remain owned by Studio; this context retains
    typed editor state and scalar/scroll positions, never borrowed widget handles.
    Recovery reviews belong to their project rather than whichever tab is visible.
    Inactive bridges may observe their own project without painting another one. }
  TNyxNativeStudioProject = class
  public
    Owner: TNyxNativeStudio;
    Reference: TNyxWorkspaceRef;
    Session: TNyxStudioSession;
    SourceCommands: TNyxSourceCommands;
    Bridge: TNyxStudioAgentBridge;
    RecoveryChoice: TNyxStudioRecoveryChoice;
    RecoveryReview: TNyxStudioRecoveryReview;
    RecoveryFocus: TNyxStudioRecoveryFocus;
    State: TNyxStudioViewState;
    Preview: Boolean;
    SavedPair: TNyxText;
    BoundProject: TNyxText;
    ProjectRevision: TNyxText;
    RemotePair: TNyxText;
    RemoteRevision: TNyxText;
    ImportPacket: TNyxText;
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
    { An initial read may precede explicit Apply or Build. A plain panel read
      must never turn newer local edits into an unsolicited profile write. }
    ProfileCommitRequested: Boolean;
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
    FProjectFiles: INyxTextFileExchange;
    FImportPacket: TNyxText;
    FImportContext: TNyxStudioCommandContext;
    FImportSnapshot: TNyxText;
    FImportOpening: Boolean;
    FOpeningBoundProject: TNyxText;
    FOpeningRevision: TNyxText;
    FImagePickContext: TNyxStudioCommandContext;
    FImagePickOwner: TNyxText;
    FImagePickBaseline: TNyxText;
    FSession: TNyxStudioSession;
    FSourceCommands: TNyxSourceCommands;
    FSourceCompiler: INyxSourceCompiler;
    FProjectSourceCompiler: INyxSourceCompiler;
    FSharedSourceFactory: INyxSharedSourceCompilerFactory;
    { Late local execution is owned by this host, independent of selected output
      and every project's pair/history. Injected strategies keep their authority
      and do not expose a competing local configuration UI. Saved hints never
      install a compiler on startup. All accessed on the application UI thread. }
    FLocalSourceAvailable: Boolean;
    FLocalSourceEnabled: Boolean;
    FLocalSourceSettings: TNyxLocalSourceSettings;
    FConfiguredLocalSourceSettings: TNyxLocalSourceSettings;
    FLocalSourceSettingsFile: TNyxText;
    FLocalSourcePrevious: TNyxText;
    FLocalSourcePreviouslyPresent: Boolean;
    FLocalSourceMessage: TNyxText;
    FTheme: TNyxTheme;
    FChromeTheme: TNyxTheme;
    FShell: TNyxDocument;
    FCodeDocument: TNyxDocument;
    FSourcePaneDocument: TNyxDocument;
    FSourcePaneView: TNyxLCLRenderer;
    FSourcePaneParking: TPanel;
    FSourceModal: INyxLCLModalHost;
    { Independent proposals are retained per session/load and name. Dismissal
      hides these owned windows; publication never remounts their local input. }
    FDiagramWindows: array of INyxDiagramEditorWindow;
    FDiagramContexts: array of TNyxStudioCommandContext;
    FDiagramNames: array of TNyxDiagramRef;
    FLastDiagramWindow: Integer;
    FDiagramSerial: Integer;
    { Independent report presentation; view unmounts before document retirement. }
    FQueryPreviewModal: INyxLCLModalHost;
    FQueryPreviewView: TNyxLCLRenderer;
    FQueryPreviewDocument: TNyxDocument;
    FSourceFocusPending: Boolean;
    FShellView: TNyxStudioSectionViews;
    FResourceBrowser: TNyxStudioResourceBrowser;
    { Public managed Nyx presentation owns cloned component help content. }
    FComponentHelp: INyxLCLPopover;
    FActionMenu: INyxLCLMenu;
    FActionMenuState: TNyxStudioActionMenuState;
    FActionButton: INyxMenuButton;
    FCanvasMenus: INyxMenuBindings;
    { Borrowed receiver registration; cancelled before any controller teardown. }
    FHierarchySubscription: INyxEventSubscription;
    FCanvasView: TNyxLCLRenderer;
    { Managed Interact ownership adopts inside the renderer's pure publication
      boundary. This slot owns no controls; its weak source port revokes first
      during Studio disposal, and stores release after the canvas retires. }
    FPreviewSlot: TNyxStudioPreviewSlot;
    FDesignerDrag: TNyxStudioDrag;
    FDesignerResize: TNyxStudioResize;
    FDesignerMove: TNyxStudioMove;
    FCodeView: TNyxLCLRenderer;
    { Owns source timelines/callback leases; detach before the retained view and
      project sessions retire. The controller never owns native controls. }
    FSourceInput: TNyxStudioSourceInput;
    { Programmatic frame restoration must not be observed as physical typing. }
    FSourceInputWriting: Boolean;
    { A retained memo is not necessarily the current source presentation. These
      copied source facts and borrowed identities certify only the successfully
      written frame; parking or a failed write revokes admission first. Neither
      identity owns a control, and old identities are only compared, not read. }
    FSourceInputAvailable: Boolean;
    FSourceInputContext: TNyxStudioCommandContext;
    FSourceInputFile: TNyxPascalSourceRef;
    FSourceInputText: TNyxText;
    FSourceInputRoot: TNyxNode;
    FSourceInputHost: TWinControl;
    { Independent public Nyx popover. Rebuild only after callback return; the
      source memo and its text timeline remain mounted and keep keyboard focus. }
    FCompletionPopover: INyxLCLPopover;
    FCompletionRevision: Integer;
    FCompletionQueued: Boolean;
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
    { Copied identity of the last admitted event frame. The temporary root and
      footer are borrowed only during the synchronous pending presentation. }
    FEventFrameOwner, FEventFrameView: TNyxText;
    FEventFrameSchema: Integer;
    FPendingEventRoot, FPendingEventFooter: TNyxNode;
    FPendingEventPresentation: Boolean;
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
    { Managed outline owns detached discovery only. Copied project view records
      borrow its presenter; load switches retire the previous navigation scope. }
    FSourceOutline: INyxStudioSourceOutline;
    FPendingProject: TNyxNativeStudioProject;
    FChangingProject: Boolean;
    { Compact panels may be absent during project admission. Retain each lane's
      restoration until that project's corresponding panel is actually mounted. }
    FRestoreProjectLeft: Boolean;
    FRestoreProjectRight: Boolean;
    FRestoreProjectCode: Boolean;
    FRestoreProjectCanvas: Boolean;
    FAgentCompilerSequence: Integer;
    FInitialPair: TNyxText;
    FOutputLoaded: Boolean;
    FOutputLoading: Boolean;
    { Shared UI-thread reconciliation retains independent field intent through
      delayed Apply/Reload replies. Views borrow the stable FOutputs object. }
    FOutputEdits: TNyxOutputEditState;
    FOutputIdentity: TNyxBuildOutputRef;
    procedure LoadLocalSourceHints;
    procedure RequireSourceConfigurationIdle;
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
    function ReadSourceInput(out ASession: TNyxStudioSession): TNyxEditingSnapshot;
    function MatchesSourceInputFrame: Boolean;
    procedure PublishSourceInputFrame(const AContext: TNyxStudioCommandContext;
      const AFile: TNyxPascalSourceRef; const AText: TNyxText;
      ARoot: TNyxNode; AHost: TWinControl);
    procedure WriteSourceInput(const AFrame: TNyxTextEditorFrame; AChanged: Boolean);
    function ReadCompletionContext: INyxStudioCompletionContext;
    function ReadCompletionOwner: TNyxPascalSourceRef;
    procedure CompletionChanged;
    procedure UpdateCompletionPopup(AData: PtrInt);
    procedure CompletionEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure RenderStudioCanvas(AHost: TWinControl);
    procedure SourceCommandChanged(AState: TNyxSourceCommandState;
      const AMessage: TNyxText);
    function PendingEventFrameReady: Boolean;
    function TryPendingEventPresentation: Boolean;
    { Reset project-local presentation only after admitted opening; input and
      file binding stay available through failed or stale compiler completion. }
    procedure ProjectOpened;
    { Projection refusal is separate from accepted source admission. These
      presentation operations retain exact files/history and retry ownership. }
    procedure DisplayFailed(const AMessage: TNyxText);
    procedure SyncDisplayRecovery;
    procedure SaveProject;
    procedure OpenProject;
    { File selection is guarded by the exact project/load and current pair.
      Conflicting input remains independently retained until explicit resolution. }
    procedure ImportProject;
    procedure ProjectFilesPicked(AStatus: TNyxFilePickStatus;
      const AFiles: TNyxTextFiles; const AError: TNyxText);
    procedure AcceptImport(AResolution: TNyxProjectResolution);
    procedure ExportProject(ACompanion: Boolean; AInput: Boolean = False);
    procedure AcceptRemote;
    procedure CapturePresentation;
    { Capture only the current published source frame. Parked controls may still
      contain another project's text until the optional source pane is revealed. }
    procedure CaptureProjectSource;
    { Target widgets remain borrowed; only visible pane positions enter copied
      presentation. Compact switching never owns a second resource proposal. }
    procedure CaptureResourcePaneScroll;
    procedure RestoreResourcePaneScroll;
    procedure SelectResourcePane(APane: TNyxResourceWorkspacePane);
    procedure SourceModalDismiss;
    procedure ClearDiagramWindows;
    procedure ShowQueryPreview;
    procedure QueryPreviewEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure RetireStaleQueryPreview;
    { Update independent source/status controls without replacing the title field
      that is currently notifying. Guard programmatic source feedback. }
    procedure UpdateTitleAndSource;
    procedure AgentRefresh(AContentChanged: Boolean);
    procedure BeginBuild(AApplication: Boolean);
    procedure BeginCompiledPreview(AProject: TNyxNativeStudioProject);
    function CompiledPreviewCurrent(AProject: TNyxNativeStudioProject): Boolean;
    function GetCompiledPreviewProcessID: Integer;
    procedure ReadOutputs(AProject: TNyxNativeStudioProject; ABuild: Boolean;
      AIntent: TNyxOutputSyncIntent = osiObserve);
    procedure ProjectBuildReply(AProject: TNyxNativeStudioProject);
    procedure ConsumeCompilerLaunch(AProject: TNyxNativeStudioProject);
    procedure RecordLocal;
    procedure CaptureProject;
    procedure AdmitProject(AProject: TNyxNativeStudioProject);
    procedure RestoreProjectControls;
    function CurrentBridge: TNyxStudioAgentBridge;
    function GetAgentState: TNyxStudioAgentView;
    { One explicit destination is consumed after painting its project's controls.
      Background bridge observations cannot create or replay keyboard navigation. }
    procedure CompleteRecoveryFocus;
    function ComposeShell: TNyxDocument;
    function GetPresentationPending: Boolean;
  protected
    { Ordinary Studio consumes the public local-file picker contract. }
    function CreateImagePicker: INyxImagePicker; virtual;
    { Own the replaceable byte-only picker; cancel its borrowed reply before
      retirement. The shared resource form remains independent of LCL dialogs. }
    function CreateResourcePicker: INyxResourcePicker; virtual;
    { Own one replaceable public file exchange. Cancel its borrowed callback
      before controller/project retirement. No picker or path enters the model. }
    function CreateProjectFiles: INyxTextFileExchange; virtual;
    { Owned adapter factory for embedded hosts with another private transport.
      Default uses asynchronous loopback HTTP. No receiver runs inside Post. }
    function CreateEditorExchange: TNyxStudioEditorExchange; virtual;
  public
    { Local directory is explicit host configuration, never design content. }
    { Explicit optional trusted compiler strategy. It is copied into each
      project command context and owns no accepted tree or host widget.
      ASharedFactory instead creates providers from each project's acknowledged
      server context. Supplying both strategies raises ENyxModel; default
      construction requires no compiler and starts no shared publication.
      AProjectCompiler optionally supplies separate compile-only file execution;
      otherwise a local ASourceCompiler serves both Apply and Open workflows. }
    constructor Create(AHost: TWinControl; const AProjectDirectory: TNyxText;
      const ASourceCompiler: INyxSourceCompiler = nil;
      const ASharedFactory: INyxSharedSourceCompilerFactory = nil;
      const AProjectCompiler: INyxSourceCompiler = nil);
    destructor Destroy; override;
    { First mount runs outside widget callbacks. Subsequent refreshes are queued. }
    procedure Run;
    { Admission retains the existing pair on failure. A successful open starts
      that project's history, matching the ordinary browser paired-open contract. }
    procedure LoadProject(const APair: TNyxProjectPair;
      AResolution: TNyxProjectResolution = nprRequireMatch);
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
    { Owned UI-thread pair/draft snapshot, independent of mounted source fields.
      No tree, renderer or command authority is returned. }
    function ProjectSnapshot: TNyxProjectPair;
    { Explicit UI-thread host operation: validate copied machine paths, persist
      hints and install one local compiler for Apply/Open on every idle context.
      Failed validation/persistence and busy contexts retain the old strategy and
      exact files/history. Injected/shared strategies refuse this local override.
      Configuration starts no compiler process and never changes output choice. }
    procedure ConfigureLocalSource(const ASettings: TNyxLocalSourceSettings);
    { Idle-only retirement, preserving settings, accepted pairs and pending text.
      Future source commands use the strict compiler-independent path. }
    procedure DisableLocalSource;
    { Copied machine hints, never a mutable compiler/profile owner. }
    property LocalSourceSettings: TNyxLocalSourceSettings read FLocalSourceSettings;
    property LocalSourceEnabled: Boolean read FLocalSourceEnabled;
    { Borrow the current project's command context on the UI thread. A project
      jump may change this identity; callers must never free the borrowed owner. }
    property SourceCommands: TNyxSourceCommands read FSourceCommands;
    property ShellView: TNyxStudioSectionViews read FShellView;
    property CanvasView: TNyxLCLRenderer read FCanvasView;
    { UI-owned floating authoring. Absence starts an independent English
      proposal; explicit Save uses this project's ordinary source queue.
      Closing retains exact input; Studio retirement disconnects the window. }
    function ShowDiagram(const AReference: TNyxDiagramRef): INyxDiagramEditorWindow;
    procedure ResumeDiagram;
    property CodeView: TNyxLCLRenderer read FCodeView;
    { Borrowed source workspace/window contracts; destroy neither. }
    property SourceView: TNyxLCLRenderer read FSourcePaneView;
    property SourceModal: INyxLCLModalHost read FSourceModal;
    { Borrowed target observations for ordinary input/accessibility consumers. }
    property CompletionPopover: INyxLCLPopover read FCompletionPopover;
    { Borrowed action family for embedding and real native input qualification.
      Nil before composition; its registrations retire with this Studio. Do not
      hold menu callbacks or widget observations beyond the owning editor. }
    property ActionMenu: INyxLCLMenu read FActionMenu;
    { Borrowed hosting seam for embedded viewers/input qualification, not edits. }
    property QueryPreviewModal: INyxLCLModalHost read FQueryPreviewModal;
    property QueryPreviewView: TNyxLCLRenderer read FQueryPreviewView;
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
  StdCtrls, nyx.editing.lcl, nyx.interaction, nyx.contract, nyx.source, nyx.schema,
  nyx.collections.query.editor, nyx.parts.editor,
  nyx.studio.commands, nyx.studio.authoring, nyx.studio.inspector,
  nyx.studio.palette, nyx.studio.source, nyx.studio.sourcefiles,
  nyx.pascal.filechanges, nyx.studio.diagnostics, nyx.studio.rootview,
  nyx.studio.builddiagnostics,
  nyx.studio.exchange.lcl, nyx.studio.agents, nyx.studio.hierarchy,
  nyx.studio.sourceconfiguration.native, nyx.studio.sourcecompilation.native,
  nyx.studio.directories, nyx.studio.buildexecutor, Math;

function TNyxNativeStudio.ProjectSnapshot: TNyxProjectPair;
begin

  if FSession = nil then
  begin
    raise ENyxModel.Create('Studio project observation requires its owning session');
  end;
  Result := FSession.ProjectSnapshot;
end;

function TNyxNativeStudio.SourceBusy: Boolean;
begin
  Result := (FSourceCommands <> nil) and FSourceCommands.Busy;
end;

procedure TNyxNativeStudio.LoadLocalSourceHints;
begin
  try
    FLocalSourcePreviouslyPresent := FileExists(FLocalSourceSettingsFile);
    FLocalSourcePrevious := ReadNyxLocalSourceSettings(FLocalSourceSettingsFile);

    if FLocalSourcePreviouslyPresent then
    begin
      FLocalSourceSettings := TNyxLocalSourceSettings.Decode(FLocalSourcePrevious);
      FLocalSourceMessage := 'Saved paths loaded. Choose Use for Pascal source to enable execution.';
    end;
  except
    on LException: Exception do
    begin
      { Missing tools or damaged machine hints must not block the uncompiled
        editor. Keep the diagnostic visible in Outputs; never install a strategy
        merely because a file or a prior enabled setting existed. }
      FLocalSourceMessage := 'Saved source settings could not be loaded: ' + LException.Message;
    end;
  end;
end;

procedure TNyxNativeStudio.RequireSourceConfigurationIdle;
var
  LIndex: Integer;
begin

  if GetCurrentThreadID <> MainThreadID then
  begin
    raise ENyxModel.Create('Configure source compilation on the application UI thread');
  end;

  if not FLocalSourceAvailable then
  begin
    raise ENyxModel.Create('This editor owns an injected compiler strategy; configure its host instead');
  end;

  if FChangingProject or FImportOpening or not FSourceCommands.CanOpenProject then
  begin
    raise ENyxModel.Create('Wait for pending editor changes before configuring source compilation');
  end;
  for LIndex := 0 to High(FProjects) do
  begin

    if not FProjects[LIndex].SourceCommands.CanOpenProject then
    begin
      raise ENyxModel.Create('Wait for every project source operation before configuring compilation');
    end;
  end;
end;

procedure TNyxNativeStudio.ConfigureLocalSource(const ASettings: TNyxLocalSourceSettings);
var
  LSettings: TNyxLocalSourceSettings;
  LDirectories: TNyxStudioDirectories;
  LProfile: TNyxOutputConfiguration;
  LCompiler: INyxSourceCompiler;
  LIndex: Integer;
begin
  RequireSourceConfigurationIdle;

  if (Trim(ASettings.LibraryRoot) = '') or (Trim(ASettings.RuntimeRoot) = '') or
    (Trim(ASettings.CompilerPath) = '') then
  begin
    raise ENyxModel.Create('Choose an FPC compiler, Nyx library and private compiler workspace');
  end;
  LSettings := ASettings.WithLibrary(ExpandFileName(ASettings.LibraryRoot))
    .WithRuntime(ExpandFileName(ASettings.RuntimeRoot))
    .WithCompiler(ExpandFileName(ASettings.CompilerPath));

  if not FileExists(LSettings.CompilerPath) then
  begin
    raise ENyxModel.Create('The configured FPC executable does not exist; current strategy retained');
  end;

  if not FileExists(IncludeTrailingPathDelimiter(LSettings.LibraryRoot) +
    'src' + PathDelim + 'nyx.model.pas') or
    not DirectoryExists(IncludeTrailingPathDelimiter(LSettings.LibraryRoot) + 'studio') then
  begin
    raise ENyxModel.Create('Choose a Nyx library folder containing src and studio');
  end;
  LDirectories := TNyxStudioDirectories.ForRepository(LSettings.LibraryRoot)
    .RunningIn(LSettings.RuntimeRoot);
  LProfile := TNyxOutputConfiguration.Create;
  try
    LProfile.SetField('fpc', LSettings.CompilerPath);
    LCompiler := NewNyxNativeSourceCompiler(LDirectories, LProfile.Encode,
      TNyxCompilerLimits.Default);
  finally
    LProfile.Free;
  end;
  { All context checks and independent host preparation precede publication.
    Save hints before replacing any strategy; a failed/conflicting settings write
    leaves every project on its prior compiler. These idle setter calls have no
    user callbacks and create neither a job nor document/history changes. }
  SaveNyxLocalSourceSettings(FLocalSourceSettingsFile, LSettings,
    FLocalSourcePreviouslyPresent, FLocalSourcePrevious);
  for LIndex := 0 to High(FProjects) do
  begin

    if FProjects[LIndex].SourceCommands <> FSourceCommands then
    begin
      FProjects[LIndex].SourceCommands.UseCompiler(LCompiler);
    end;
  end;
  FSourceCommands.UseCompiler(LCompiler);
  FSourceCompiler := LCompiler;
  FProjectSourceCompiler := LCompiler;
  FConfiguredLocalSourceSettings := LSettings;
  FLocalSourceSettings := LSettings;
  FLocalSourcePrevious := LSettings.Encode;
  FLocalSourcePreviouslyPresent := True;
  FLocalSourceEnabled := True;
  FLocalSourceMessage := 'Settings applied. Complete Pascal runs when you choose Apply or Open.';
  FState.Status := 'Local source compiler configured';
  RequestRefresh;
end;

procedure TNyxNativeStudio.DisableLocalSource;
var
  LIndex: Integer;
begin
  RequireSourceConfigurationIdle;
  for LIndex := 0 to High(FProjects) do
  begin

    if FProjects[LIndex].SourceCommands <> FSourceCommands then
    begin
      FProjects[LIndex].SourceCommands.UseCompiler(nil);
    end;
  end;
  FSourceCommands.UseCompiler(nil);
  FSourceCompiler := nil;
  FProjectSourceCompiler := nil;
  FLocalSourceEnabled := False;
  FLocalSourceMessage := 'Source compilation is off. Saved paths and your current project are retained.';
  FState.Status := 'Local source compilation turned off';
  RequestRefresh;
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
    ncSourceFilesTab,
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
    ncSaveOutputs,
    ncReloadOutputs,
    ncSave,
    ncProjectSave,
    ncProjectOpen,
    ncProjectRemote,
    ncProjectCopy,
    ncProjectImport,
    ncProjectExport,
    ncProjectExportFiles,
    ncImportBackup,
    ncImportPascal,
    ncImportDesign,
    ncImportCancel,
    ncAgents,
    ncBuilds,
    ncAgentConnect,
    ncAgentPause,
    ncAgentAccept,
    ncAgentConfirm,
    ncAgentCancelReview,
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
    'action-source-files-tab',
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
    'action-save-outputs',
    'action-reload-outputs',
    'action-save',
    'action-project-save',
    'action-project-open',
    'action-project-use-remote',
    'action-project-copy',
    'action-project-import',
    'action-project-export',
    'action-project-export-files',
    'action-project-input-backup',
    'action-project-use-pascal',
    'action-project-use-design',
    'action-project-cancel-import',
    'action-agents',
    'action-builds',
    'action-agent-connect',
    'action-agent-pause',
    'action-agent-accept',
    NyxStudioRecoveryConfirmID,
    NyxStudioRecoveryCancelID,
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

  { Explicit draft restoration changes the acknowledged frame even though no
    compiler/design result was applied. Publish through this owning project's
    bridge, including when it is not the currently displayed project. }

  if ((AState = nssApplied) or SourceCommands.RestoredDraft) and (Bridge <> nil) then
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
  const AProjectDirectory: TNyxText; const ASourceCompiler: INyxSourceCompiler;
  const ASharedFactory: INyxSharedSourceCompilerFactory;
  const AProjectCompiler: INyxSourceCompiler);
begin
  inherited Create;

  if (ASourceCompiler <> nil) and (ASharedFactory <> nil) then
  begin
    raise ENyxModel.Create('Choose one local or shared source compilation strategy');
  end;

  if AHost = nil then
  begin
    raise ENyxModel.Create('Native Studio requires a host');
  end;
  FHost := AHost;
  FHostSpace := NewNyxLCLHostSpace(FHost, NyxHostSizing.Fit(nhfAvailableHeight));
  FSession := TNyxStudioSession.Create;
  FSourceCompiler := ASourceCompiler;
  FProjectSourceCompiler := AProjectCompiler;

  if FProjectSourceCompiler = nil then
  begin
    FProjectSourceCompiler := ASourceCompiler;
  end;
  FSharedSourceFactory := ASharedFactory;
  FSourceCommands := TNyxSourceCommands.Create(FSession, SourceCommandChanged);
  FSourceCommands.UseCompiler(FSourceCompiler);
  FSourceCommands.UseProjectCompiler(FProjectSourceCompiler);
  FTheme := TNyxTheme.Create;
  FChromeTheme := NewNyxStudioChromeTheme;

  if SameText(FHost.Font.Name, 'default') then
  begin
    { Use the platform UI family when the embedding host has not chosen one. }
    FHost.Font.Name := Screen.MenuFont.Name;
  end;
  FOutputs := TNyxOutputConfiguration.Create;
  FOutputEdits := TNyxOutputEditState.Create;
  FStore := TNyxProjectStore.Create(AProjectDirectory);
  FPreviewDirectory := IncludeTrailingPathDelimiter(ExpandFileName(AProjectDirectory)) +
    'compiled-previews';
  FState := DefaultNyxStudioViewState;
  FSourceOutline := NewNyxStudioSourceOutline;
  FState.SourceOutline := FSourceOutline.Presenter;
  FLocalSourceAvailable := (ASourceCompiler = nil) and (ASharedFactory = nil) and
    (AProjectCompiler = nil);
  FLocalSourceSettingsFile := IncludeTrailingPathDelimiter(ExpandFileName(AProjectDirectory)) +
    '.local' + PathDelim + 'source-settings.json';
  FLocalSourceSettings := TNyxLocalSourceSettings.Empty.WithRuntime(
    IncludeTrailingPathDelimiter(ExpandFileName(AProjectDirectory)) + 'source-runtime');

  if FLocalSourceAvailable then
  begin
    LoadLocalSourceHints;
  end;
  FState.CodePresentation := ncpPaneHosted;
  FState.Outputs := FOutputs;
  FShellView := TNyxStudioSectionViews.Create(FChromeTheme, nscEditorOwnedHierarchy);
  FResourceBrowser := TNyxStudioResourceBrowser.Create;
  FCanvasView := TNyxLCLRenderer.Create(FTheme);
  FPreviewSlot := TNyxStudioPreviewSlot.Create(FCanvasView.Events.Scheduler);
  FCanvasView.DesignerInput := NyxDesignerInput.Drops(True);
  FCanvasView.OnDesignerGesture := DesignerGesture;
  FDesignerDrag := TNyxStudioDrag.Create(DesignerDragContext, DesignerDragFeedback,
    DesignerPlacementFeedback);
  FDesignerResize := TNyxStudioResize.Create(DesignerDragContext,
    DesignerResizeMeasure, DesignerResizeStatus, DesignerResizePresentation, DesignerResizeGuides);
  FDesignerMove := TNyxStudioMove.Create(DesignerDragContext, DesignerResizeGuides,
    DesignerResizeStatus, DesignerResizePresentation, DesignerMovePoint);
  FCodeView := TNyxLCLRenderer.Create(FChromeTheme);
  FSourcePaneView := TNyxLCLRenderer.Create(FChromeTheme);
  FSourcePaneView.OnEvent := ShellEvent;
  FSourceModal := NewNyxLCLModalHost(FHost);
  FSourceModal.OnDismiss := SourceModalDismiss;
  FShellView.OnEvent := ShellEvent;
  FCanvasView.OnEvent := CanvasEvent;
  FCodeView.OnEvent := SourceEvent;

  if NyxLCLTextHistorySupported then
  begin
    FSourceInput := TNyxStudioSourceInput.Create(FCodeView.Events, ReadSourceInput, WriteSourceInput);
    FSourceInput.ConfigureCompletion(ReadCompletionContext, ReadCompletionOwner, CompletionChanged);
  end;
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

{$I nyx.studio.diagrams.inc}

destructor TNyxNativeStudio.Destroy;
var
  LIndex: Integer;
begin
  FSourceInputAvailable := False;
  { Revocation is local and cannot enter application cleanup while any Studio
    receiver is being destroyed. Actual store cleanup follows canvas teardown. }

  if FPreviewSlot <> nil then
  begin
    FPreviewSlot.Revoke;
  end;
  ClearDiagramWindows;
  FRunning := False;

  if FProjectFiles <> nil then
  begin
    FProjectFiles.Cancel;
    FProjectFiles := nil;
  end;

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
  try
    FreeAndNil(FCanvasView);
  finally
    FreeAndNil(FPreviewSlot);
  end;

  if FQueryPreviewModal <> nil then
  begin
    FQueryPreviewModal.Hide;
  end;
  FQueryPreviewView.Free;
  FQueryPreviewDocument.Free;
  FQueryPreviewModal := nil;

  if FSourceModal <> nil then
  begin
    FSourceModal.OnDismiss := nil;
    FSourceModal.Hide;
  end;
  FCompletionPopover := nil;
  FreeAndNil(FSourceInput);
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

  if FActionMenu <> nil then
  begin
    FActionMenu.Close;
  end;
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
  FOutputEdits.Free;
  FOutputs.Free;
  FTheme.Free;
  FChromeTheme.Free;
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
    Result.RecoveryReview := FCurrentProject.RecoveryReview;
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
  LCreated: TNyxWorkspaceRef;
begin
  LState := GetAgentState;

  if (CurrentBridge <> nil) and not SourceBusy and not FPainting and
    CurrentBridge.TakeCreatedWorkspace(LCreated) then
  begin
    JumpWorkspace(LCreated);
    Exit;
  end;

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

procedure TNyxNativeStudio.ReadOutputs(AProject: TNyxNativeStudioProject; ABuild: Boolean;
  AIntent: TNyxOutputSyncIntent);
begin

  if (AProject = nil) or (AProject.Bridge = nil) then
  begin
    raise ENyxModel.Create('Connect a compiler service to apply or reload settings; local edits are retained');
  end;

  if FOutputLoading then
  begin
    raise ENyxModel.Create('Output configuration is synchronizing; local settings are retained');
  end;

  if AProject.BuildStage in [nbsOutputs, nbsRequest, nbsPolling, nbsPreviewInspect,
    nbsPreviewDownload, nbsPreviewActivate, nbsLaunchInspect] then
  begin
    raise ENyxModel.Create('Compiler work is active; output edits are retained');
  end;

  AProject.ProfileCommitRequested := AIntent = osiSave;

  if (AIntent = osiSave) and not FOutputLoaded then
  begin
    AIntent := osiObserve;
  end
  else if (AIntent = osiObserve) and ABuild and FOutputLoaded and FOutputEdits.Dirty then
  begin
    AIntent := osiSave;
  end;
  FOutputEdits.BeginRequest(AIntent, FOutputs);
  FOutputLoading := True;
  AProject.BuildRequested := ABuild;
  AProject.BuildStage := nbsProfile;
  AProject.BuildReplySequence := AProject.Bridge.State.BuildReplySequence;
  AProject.BuildMessage := 'Loading saved output configuration';

  if AIntent = osiSave then
  begin
    AProject.BuildMessage := 'Saving local output configuration';
  end;
  FState.Status := AProject.BuildMessage;
  try

    if AIntent = osiSave then
    begin
      AProject.Bridge.SaveCompilerProfile(FOutputs, FOutputIdentity);
    end
    else
    begin
      AProject.Bridge.CompilerProfile;
    end;
  except
    FOutputEdits.AbortRequest;
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
  LProfileIntent: TNyxOutputSyncIntent;
  LProfileIdentity: TNyxBuildOutputRef;
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
          LProfileIdentity := NyxBuildOutput(LReply.Field('outputID').AsText);
          LProfile := TNyxOutputConfiguration.Decode(LReply.Field('profile').ToJSON);
          try
            LProfileIntent := FOutputEdits.Intent;
            FOutputEdits.Accept(FOutputs, LProfile);
            FOutputIdentity := LProfileIdentity;
          finally
            LProfile.Free;
          end;
          FOutputLoaded := True;

          if ((LProfileIntent = osiObserve) and AProject.ProfileCommitRequested) or
            (FOutputEdits.Dirty and AProject.BuildRequested) then
          begin
            ReadOutputs(AProject, AProject.BuildRequested, osiSave);
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

            if LProfileIntent = osiSave then
            begin
              AProject.BuildMessage := 'Local output configuration saved';

              if FOutputEdits.Dirty then
              begin
                AProject.BuildMessage := AProject.BuildMessage + ' / local edits retained for Apply';
              end;
            end
            else if FOutputEdits.Dirty then
            begin
              AProject.BuildMessage := 'Output configuration loaded / local edits retained for Apply';
            end;
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
        FOutputEdits.AbortRequest;
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
    AProject.SourceCommands.Busy or FOutputEdits.Dirty or
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
    LProject.Bridge.UseCompletionExchange(CreateEditorExchange);
    SetLength(FProjects, 1);
    FProjects[0] := LProject;
    FCurrentProject := LProject;
    LProject.SourceCommands := FSourceCommands;

    if FSharedSourceFactory <> nil then
    begin
      LProject.Bridge.UseSharedSourceFactory(FSharedSourceFactory);
      LProject.SourceCommands.UseSharedCompiler(LProject.Bridge.SharedSourceHost);
    end;
    FSourceCommands.OnChanged := LProject.SourceChanged;
    LProject := nil;
    FServiceURL := ABaseURL;
    CurrentBridge.Connect(EncodeNyxProject(FSession.ProjectSnapshot) <> FInitialPair);
    { Primary connection preserves the caller's current pane choice. An explicit
      named-session arrival exposes navigation like browser Studio; a conflict
      still opens its resolution through AgentRefresh. Connection alone should
      not consume the design canvas with optional transport/configuration UI. }

    if AWorkspace.ID <> '' then
    begin
      FState.AgentsVisible := True;
    end;
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
  FCurrentProject.ImportPacket := FImportPacket;

  if FShellView.SectionView(nssProject) <> nil then
  begin
    FCurrentProject.LeftTop := NativeScrollTop(FShellView.SectionView(nssProject), 'studio-left');
  end;

  if FShellView.SectionView(nssInspector) <> nil then
  begin
    FCurrentProject.RightTop := NativeScrollTop(FShellView.SectionView(nssInspector), 'studio-right');
  end;

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

  if FCurrentProject = nil then
  begin
    Exit;
  end;

  if FRestoreProjectCode and MatchesSourceInputFrame then
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
    FRestoreProjectCode := False;
  end;

  if FRestoreProjectCanvas and (FCanvasView.Root <> nil) and
    FSession.MatchesCommandContext(FCanvasCommandContext) then
  begin
    FCanvasView.ScrollView(FCurrentProject.CanvasLeft, FCurrentProject.CanvasTop);
    FRestoreProjectCanvas := False;
  end;
  { ViewFor deliberately refuses absent controls. In compact mode a missing
    lane is ordinary navigation, not a display failure; apply its saved position
    once when the user next reveals it, without resetting later user scrolling. }

  if FRestoreProjectLeft and (FShellView.RootFor('studio-left') <> nil) then
  begin
    LControl := FShellView.ControlFor('studio-left');

    if LControl is TScrollBox then
    begin
      TScrollBox(LControl).VertScrollBar.Position := FCurrentProject.LeftTop;
    end;
    FRestoreProjectLeft := False;
  end;

  if FRestoreProjectRight and (FShellView.RootFor('studio-right') <> nil) then
  begin
    LControl := FShellView.ControlFor('studio-right');

    if LControl is TScrollBox then
    begin
      TScrollBox(LControl).VertScrollBar.Position := FCurrentProject.RightTop;
    end;
    FRestoreProjectRight := False;
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

  if SourceBusy or not CurrentBridge.CanSwitchWorkspace or FPainting then
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
      LProject.SourceCommands.UseCompiler(FSourceCompiler);
      LProject.SourceCommands.UseProjectCompiler(FProjectSourceCompiler);
      LProject.State := DefaultNyxStudioViewState;
      LProject.State.SourceOutline := FSourceOutline.Presenter;
      LProject.State.CodePresentation := ncpPaneHosted;
      LProject.State.Outputs := FOutputs;
      LProject.SavedPair := EncodeNyxProject(LProject.Session.ProjectSnapshot);
      LProject.Bridge := TNyxStudioAgentBridge.Create(LProject.Session,
        LProject.Refresh, AWorkspace, CreateEditorExchange);
      LProject.Bridge.UseCompletionExchange(CreateEditorExchange);

      if FSharedSourceFactory <> nil then
      begin
        LProject.Bridge.UseSharedSourceFactory(FSharedSourceFactory);
        LProject.SourceCommands.UseSharedCompiler(LProject.Bridge.SharedSourceHost);
      end;
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

  if SourceBusy or not LState.Connected or not CurrentBridge.CanSwitchWorkspace then
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
  FImportPacket := AProject.ImportPacket;

  if FProjectFiles <> nil then
  begin
    FProjectFiles.Cancel;
  end;
  FCanvasID := '';
  FRestoreProjectCode := True;
  FRestoreProjectCanvas := True;
  FRestoreProjectLeft := True;
  FRestoreProjectRight := True;
  FSourceLine := 0;
  FSourceColumn := 0;
  FAgentCompilerSequence := 0;
  FCompilerReport := nil;
  FRootRemoval := nil;

  RequestRefresh(True);
end;

procedure TNyxNativeStudio.LoadProject(const APair: TNyxProjectPair;
  AResolution: TNyxProjectResolution);
begin

  if not FSourceCommands.CanOpenProject then
  begin
    raise ENyxModel.Create('Wait for pending editor changes before opening a project');
  end;
  FImportPacket := EncodeNyxProject(APair);
  FOpeningBoundProject := '';
  FOpeningRevision := '';
  FImportOpening := True;
  try
    FSourceCommands.OpenProject(APair, AResolution);
  except
    FImportOpening := False;
    raise;
  end;
end;

procedure TNyxNativeStudio.ProjectOpened;
var
  LRevision: TNyxText;
  LWarning: TNyxText;
begin
  LWarning := '';

  if FProjectFiles <> nil then
  begin
    FProjectFiles.Cancel;
  end;

  if FImportOpening then
  begin
    FBoundProject := FOpeningBoundProject;
    FProjectRevision := FOpeningRevision;

    try

      if FBoundProject <> '' then
      begin
        { Reading a journal granted no execution authority. The now-admitted
          live checkpoint alone can complete its unchanged disk recovery. }

        if not FStore.CompleteCapturedRecovery(FBoundProject, FProjectRevision,
          FSession.ProjectSnapshot, FSession.AcceptedSourceCheckpoint, LRevision) then
        begin
          raise ENyxProjectConflict.Create('Saved files changed during checking');
        end;
        FProjectRevision := LRevision;
      end;
      FState.ProjectName := FBoundProject;
      FImportPacket := '';
      FState.ProjectConflict := False;
    except
      on LException: Exception do
      begin
        { The project is already admitted. A separate disk refusal cannot label
          that compilation failed or prevent remounting its new owned roots. }
        FState.ProjectConflict := True;
        LWarning := ' / ' + LException.Message + '; original input retained';
      end;
    end;
    FImportOpening := False;
  end;
  FState.ImportConflict := FImportPacket <> '';
  FState.MenuEditorDraft.Clear;
  FState.MenuBarEditorDraft.Clear;
  FState.TabsEditorDraft.Clear;
  FState.KanbanEditorDraft.Clear;
  FState.MasterDetailEditorDraft.Clear;
  FState.DataToolbarEditorDraft.Clear;
  FState.SelectionEditorDraft.Clear;
  FState.ReusableEditorDraft.Clear;
  FState.ExpressionEditorDraft.Clear;
  FState.TabPagesDraft.Clear;
  FState.QueryEditorDraft.Clear;
  FState.LayoutEditorDraft.Clear;
  FState.TimeDomainEditorDraft.Clear;
  FState.ContentEditorDraft.Clear;
  FState.ThemeEditorDraft.Clear;
  FState.ImageEditorDraft.Clear;
  FState.ButtonEditorDraft.Clear;
  FState.ResourceEditorDraft.Clear;
  FState.ResourceRowsDraft.Clear;
  FState.CanvasPaletteDraft.Clear;
  FState.CanvasBoundaryDraft.Clear;
    FState.CanvasPrecisionDraft.Clear;
  FState.ResourceSelection := NyxNewResourceSelection;
  FSavedPair := EncodeNyxProject(FSession.ProjectSnapshot);
  FRootRemoval := nil;
  FCompilerReport := nil;
  FState.CallbackRemoval.Pending := False;
  FState.Status := 'Project opened' + LWarning;
  RequestRefresh(True);
end;

function TNyxNativeStudio.GetPresentationPending: Boolean;
begin
  Result := FQueued or FPainting or FCompletionQueued;
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
      .Text(FSourceCommands.Message).Visible(NyxStudioSourceStatusVisible(
        FState.SourceExpanded, FState.DisplayRecovery.BlocksInput,
        FSourceCommands.Message)).Done;

    if FSourcePaneView.Root.Find('studio-source-progress') <> nil then
    begin
      FSourcePaneView.Root.Find('studio-source-progress').Configure.Visible(SourceBusy).Done;
    end;
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

procedure TNyxNativeStudio.RetireStaleQueryPreview;
begin
  { Context retirement works even while the compact Inspector is unmounted. }
  RestoreNyxStudioQueryPreview(FState.QueryPreview, FSession, nil);

  if not FState.QueryPreview.Defined and (FQueryPreviewModal <> nil) then
  begin
    FQueryPreviewModal.Hide;
  end;
end;

procedure TNyxNativeStudio.QueryPreviewEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if (AEvent.Trigger = ntClick) and not AEvent.DefaultPrevented and
    (AEvent.OriginID = NyxQueryPreviewReaderCloseID) and (FQueryPreviewModal <> nil) then
  begin
    { Hide retains the independently owned Nyx view. Never free the native
      Close button while its click notification is still returning. }
    FQueryPreviewModal.Hide;
  end;
end;

procedure TNyxNativeStudio.ShowQueryPreview;
var
  LDocument: TNyxDocument;
begin
  RetireStaleQueryPreview;

  if not FState.QueryPreview.Defined then
  begin
    Exit;
  end;

  if FQueryPreviewModal = nil then
  begin
    FQueryPreviewModal := NewNyxLCLModalHost(FHost);
  end;

  if FQueryPreviewView = nil then
  begin
    FQueryPreviewView := TNyxLCLRenderer.Create;
  end;
  LDocument := NewNyxStudioQueryPreviewDocument(FState.QueryPreview.Text);
  try
    { Root callback borrows this controller. View/listeners retire first during
      controller destruction; the reader owns only copied report text. }
    FQueryPreviewView.OnEvent := QueryPreviewEvent;
    FQueryPreviewModal.Show(NyxModal('Query preview').Sizing(nhfAvailableHeight));
    FQueryPreviewView.Render(LDocument, LDocument.Pages[0], FQueryPreviewModal.Control);
    FQueryPreviewDocument.Free;
    FQueryPreviewDocument := LDocument;
    LDocument := nil;
  finally
    LDocument.Free;
  end;
end;

procedure TNyxNativeStudio.SourceModalDismiss;
begin
  FState.SourceExpanded := False;
  FSourceFocusPending := True;
  RequestRefresh;
end;

procedure TNyxNativeStudio.CaptureProjectSource;
var
  LInput: TWinControl;
  LSelection: TNyxTextSelection;
begin

  if (FCurrentProject = nil) or FRestoreProjectCode or not MatchesSourceInputFrame then
  begin
    Exit;
  end;
  LInput := TWinControl(FCodeView.InputFor('studio-code'));
  LSelection := CaptureNyxLCLSelection(LInput);
  FCurrentProject.CodeStart := LSelection.Start;
  FCurrentProject.CodeEnd := LSelection.Finish;
  FCurrentProject.CodeFocused := Screen.ActiveControl = LInput;
end;

procedure TNyxNativeStudio.CapturePresentation;
var
  LNode: TNyxNode;
  LGroup: TNyxInspectorGroup;
  LEventFilter: TNyxEventFilter;
begin

  if FShellView.Root = nil then
  begin
    Exit;
  end;

  if FSession.MatchesCommandContext(FShellCommandContext) then
  begin
    { Retain the last real selection before a compact panel parks this editor.
      The published input frame prevents a departing mount from contaminating
      its successor's per-project selection. }
    CaptureProjectSource;

    if ReadNyxEventFilterEditor(NyxStudioEventFilterID,
      FShellView.RootFor(NyxStudioEventFilterID), LEventFilter) then
    begin
      FState.EventFilter := LEventFilter;
    end;
    FState.MenuEditorDraft.Capture('inspector-menu', FShellView.RootFor('inspector-menu'));
    FState.MenuBarEditorDraft.Capture('inspector-menu-bar', FShellView.RootFor('inspector-menu-bar'));
    FState.TabsEditorDraft.Capture('inspector-tab-policy', FShellView.RootFor('inspector-tab-policy'));
    FState.KanbanEditorDraft.Capture(NyxStudioKanbanEditorID,
      FShellView.RootFor(NyxStudioKanbanEditorID));
    FState.MasterDetailEditorDraft.Capture(NyxStudioMasterDetailEditorID,
      FShellView.RootFor(NyxStudioMasterDetailEditorID));
    FState.DataToolbarEditorDraft.Capture(NyxStudioDataToolbarEditorID,
      FShellView.RootFor(NyxStudioDataToolbarEditorID));
    FState.SelectionEditorDraft.Capture('inspector-selection-policy',
      FShellView.RootFor('inspector-selection-policy'));
    FState.ReusableEditorDraft.Capture(NyxStudioReusableEditorID,
      FShellView.RootFor(NyxStudioReusableEditorID));
    FState.ExpressionEditorDraft.Capture(NyxStudioExpressionEditorID,
      FShellView.RootFor(NyxStudioExpressionEditorID));
    FState.TabPagesDraft.Capture('inspector-tab-pages', FShellView.RootFor('inspector-tab-pages'));

    for LGroup := Low(TNyxInspectorGroup) to High(TNyxInspectorGroup) do
    begin
      FState.InspectorGroups := FState.InspectorGroups.Captured(LGroup,
        FShellView.RootFor(NyxInspectorGroupID(LGroup)));
    end;
    FState.QueryEditorDraft.Capture('inspector-collection-query', FShellView.RootFor('inspector-collection-query'));
    FState.LayoutEditorDraft.Capture(NyxStudioLayoutEditorID,
      FShellView.RootFor(NyxStudioLayoutEditorID));
    FState.TimeDomainEditorDraft.Capture('inspector-time-domain', FShellView.RootFor('inspector-time-domain'));
    FState.ContentEditorDraft.Capture('inspector-content', FShellView.RootFor('inspector-content'));
    FState.ThemeEditorDraft.Capture('studio-theme-editor', FShellView.RootFor('studio-theme-editor'));
    FState.ImageEditorDraft.Capture('inspector-image', FShellView.RootFor('inspector-image'));
    FState.ButtonEditorDraft.Capture('inspector-button', FShellView.RootFor('inspector-button'));
    CaptureNyxStudioChrome(FShellView.RootFor('studio-selection-tools'), FState.SelectionToolsExpanded);
    CaptureNyxStudioAssets(FShellView.RootFor('studio-assets-picker'), FState.AssetPickerExpanded);
    FState.ResourceEditorDraft.Capture('studio-resource-editor', FShellView.RootFor('studio-resource-editor'));
    FState.ResourceRowsDraft.Capture('studio-resource-rows', FShellView.RootFor('studio-resource-rows'));
    FState.LogicDraft.Capture('studio-logic-editor', FShellView.RootFor('studio-logic-editor'));
    FState.ProgramDraft.Capture('studio-program-editor', FShellView.RootFor('studio-program-editor'));
    FState.CanvasPaletteDraft.Capture('inspector-canvas-content',
      FShellView.RootFor('inspector-canvas-content'));
    FState.CanvasBoundaryDraft.Capture('inspector-canvas-navigation',
      FShellView.RootFor('inspector-canvas-navigation'));
    FState.CanvasPrecisionDraft.Capture('inspector-canvas-precision',
      FShellView.RootFor('inspector-canvas-precision'));
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
  { Machine configuration is shared by this editor, not restored from a
    project's copied presentation when switching service workspaces. }
  FState.LocalSourceAvailable := FLocalSourceAvailable;
  FState.LocalSourceEnabled := FLocalSourceEnabled;
  FState.LocalSourceSettings := FLocalSourceSettings;
  FState.LocalSourcePending := FLocalSourceEnabled and
    (FLocalSourceSettings.Encode <> FConfiguredLocalSourceSettings.Encode);
  FState.LocalSourceMessage := FLocalSourceMessage;

  if FSourceLine > 0 then
  begin
    FState.SourceTab := nstSource;
  end;
  FState.Compact := NyxStudioCompactHost(FHostSpace.Extent.Width, FHostSpace.Extent.Height);
  FState.Preview := FPreview;
  FState.RootRemoval := NyxNull;

  if CurrentBridge <> nil then
  begin
    CurrentBridge.ObserveResource(FState.ResourceSelection.Reference,
      FState.ResourceSelection.Locale, FState.ResourcesVisible);
  end;
  FState.Agents := GetAgentState;
  FState.ImportConflict := FImportPacket <> '';
  FState.BuildControlReady := (CurrentBridge <> nil) and
    CurrentBridge.CanCancelBuild;
  FState.BuildInspectionReady := (CurrentBridge <> nil) and
    CurrentBridge.CanInspectBuild;
  FState.BuildInspectionSynchronized := (CurrentBridge <> nil) and
    CurrentBridge.SourceSynchronized;
  FComponentHelp := nil;
  FState.PendingDesign := FSourceCommands.PendingDesign;
  FState.SourceWorking := SourceBusy;
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
  FState.TabsEditorDraft.Restore(Result.Pages[0]);
  FState.KanbanEditorDraft.Restore(Result.Pages[0]);
  FState.MasterDetailEditorDraft.Restore(Result.Pages[0]);
  FState.DataToolbarEditorDraft.Restore(Result.Pages[0]);
  FState.SelectionEditorDraft.Restore(Result.Pages[0]);
  FState.ReusableEditorDraft.Restore(Result.Pages[0]);
  FState.ExpressionEditorDraft.Restore(Result.Pages[0]);
  FState.TabPagesDraft.Restore(Result.Pages[0]);
  FState.QueryEditorDraft.Restore(Result.Pages[0]);
  FState.LayoutEditorDraft.Restore(Result.Pages[0]);
  RestoreNyxStudioQueryPreview(FState.QueryPreview, FSession, Result.Pages[0]);
  RetireStaleQueryPreview;
  FState.TimeDomainEditorDraft.Restore(Result.Pages[0]);
  FState.ContentEditorDraft.Restore(Result.Pages[0]);
  FState.ThemeEditorDraft.Restore(Result.Pages[0]);
  FState.ImageEditorDraft.Restore(Result.Pages[0]);
  FState.ButtonEditorDraft.Restore('inspector-button', Result.Pages[0]);

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
  FState.LogicDraft.Restore('studio-logic-editor', Result.Pages[0]);
  FState.ProgramDraft.Restore('studio-program-editor', Result.Pages[0]);
  FState.CanvasPaletteDraft.Restore(Result.Pages[0]);
  FState.CanvasBoundaryDraft.Restore(Result.Pages[0]);
  FState.CanvasPrecisionDraft.Restore(Result.Pages[0]);
  Result.Pages[0].Configure.Height(FHost.ClientHeight).Done;
end;

procedure TNyxNativeStudio.Paint;
var
  LShell: TNyxDocument;
  LCanvasHost: TWinControl;
  LCodeHost: TWinControl;
  LSourceHost: TWinControl;
  LSourceDocument: TNyxDocument;
  LSourceContext: TNyxStudioCommandContext;
  LSourceFile: TNyxPascalSourceRef;
  LSourceText: TNyxText;
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
  LPartsFocus: TNyxPartsEditorFocus;
  LPartsInputs: TNyxContentFaceStates;
  LPartsViewport: TNyxViewportSnapshot;
  LChromeReference: TNyxControlRef;
  LChromeInputs: TNyxContentFaceStates;
  LChromeNode: TNyxNode;
  LResourcePrepared: INyxPreparedPublication;
  LResourceCompatible: Boolean;
  {$ifdef NYX_STUDIO_PROFILE}
  LPhaseStarted: QWord;
  LCanvasPhaseStarted: QWord;
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

  procedure RecordCanvasPhase(const AName: TNyxText);
  var
    LNow: QWord;
  begin
    { Nested timings leave their parent paint clock intact. Emit fixed stage
      names, mode and elapsed values only; no source/document text is logged. }
    LNow := GetTickCount64;
    WriteLn('studio-canvas,', AName, ',', FReplaceCanvas, ',',
      LNow - LCanvasPhaseStarted);
    LCanvasPhaseStarted := LNow;
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
    LPartsFocus := Default(TNyxPartsEditorFocus);
    LPartsInputs := nil;

    if (LFocus <> nil) and (FShellView.Root <> nil) and
      InsideControl(LFocus, FShellView.ControlFor(FShellView.Root.ID)) then
    begin
      for LSection := Low(TNyxStudioSection) to High(TNyxStudioSection) do
      begin

        if FShellView.SectionRoot(LSection) <> nil then
        begin

          if FSession.MatchesCommandContext(FShellCommandContext) then
          begin
            { Find the current physical owner once, then validate only that
              Parts action. Scanning the form again for each row multiplies
              work on large forms; this copied reference retains no control. }
            LChromeReference := FShellView.SectionView(LSection).ControlReference(LFocus);

            if LChromeReference.ID <> '' then
            begin
              LChromeNode := FShellView.SectionRoot(LSection).Find(LChromeReference.ID);

              if TryCaptureNyxPartsEditorFocus(LChromeNode,
                FShellView.SectionRoot(LSection), LPartsFocus) then
              begin
                LChromeID := LChromeReference.ID;

                if LSection = nssInspector then
                begin
                  { Sample the user's current offsets immediately before this
                    publication, including deliberate scrolling away. Capture
                    only copied scroll state, never a native pointer or draft. }
                  LPartsViewport := FShellView.SectionView(LSection).ViewportFor('studio-right');
                  SetLength(LPartsInputs, 1);
                  LPartsInputs[0] := CaptureNyxContentFace(
                    FShellView.SectionRoot(LSection).Find('studio-right'));
                  LPartsInputs[0].ScrollLeft := LPartsViewport.X.Position;
                  LPartsInputs[0].ScrollTop := LPartsViewport.Y.Position;
                end;
              end;
            end;
          end;

          if LChromeID = '' then
          begin
            LChromeID := EditingChromeIdentity(FShellView.SectionRoot(LSection));
          end;
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
    { Shell replacement can park the independently retained source pane before
      its own reconciliation. Revoke readers before that first outward change. }
    FSourceInputAvailable := False;
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

        { FPreview is the requested next mode. A mode cue can replace chrome
          before the independent canvas adopts that request, so capture only
          the currently admitted runtime view. Design bindings are dormant and
          correctly refuse runtime snapshots. The reverse transition must still
          capture its live preview even though the requested mode is design. }
        if (FCanvasView.Root <> nil) and not FCanvasView.DesignMode then
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
    if FPreview and (FSession.ActiveView <> nil) then
    begin
      FCanvasView.CanvasAdoptionHost := FSourceCommands.CanvasAdoptionHost(
        FSession.CommandContext, NyxControl(FSession.ActiveViewID));
    end
    else
    begin
      FCanvasView.CanvasAdoptionHost := nil;
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
      {$ifdef NYX_STUDIO_PROFILE}LCanvasPhaseStarted := GetTickCount64;{$endif}

      if (FCanvasView.Root <> nil) and LSameView then
      begin
        FCanvasView.MoveHost(LCanvasHost);
        {$ifdef NYX_STUDIO_PROFILE}RecordCanvasPhase('host');{$endif}

        if FReplaceCanvas and
          not FCanvasView.TryRefresh(FSession.Document, FSession.ActiveView,
            not FPreview, FCanvasRestores) then
        begin
          RenderStudioCanvas(LCanvasHost);
        end;
      end
      else
      begin
        RenderStudioCanvas(LCanvasHost);
      end;
      {$ifdef NYX_STUDIO_PROFILE}RecordCanvasPhase('admit');{$endif}

      if FPreview and (FCanvasMenus = nil) then
      begin
        FCanvasMenus := BindNyxLCLMenus(FSession.Document, FCanvasView);
      end;
      FCanvasView.Select(FSession.SelectedID);
      FState.PresentationSelection := FState.PresentationSelection.Reconciled(
        FCanvasView.Root.PresentationSnapshot);
      FCanvasView.PresentationSelection := FState.PresentationSelection;
      {$ifdef NYX_STUDIO_PROFILE}RecordCanvasPhase('selection');{$endif}

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
      {$ifdef NYX_STUDIO_PROFILE}RecordCanvasPhase('pending');{$endif}
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
        { This is presentation replay, not a new navigation request. Retain
          deliberate scrolling even when the focused face was scrolled away. }
        FCanvasView.FocusWithoutScroll(LCanvasFocusID, niRuntime);

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
      LSourceContext := FSession.CommandContext;
      LSourceFile := FSession.ActiveSourceFile;
      LSourceText := FSession.ActiveSourceText;

      if FCodeView.Root = nil then
      begin
        FreeAndNil(FCodeDocument);
        FCodeDocument := NewNyxStudioCodeDocument(LSourceText);
        FCodeDocument.Pages[0].Configure.HeightSizing(nsFill).Done;
        FCodeView.Render(FCodeDocument, FCodeDocument.Pages[0], LCodeHost);
      end
      else
      begin
        FCodeView.Root.Configure.Value(LSourceText)
          .Clear(atHeight).HeightSizing(nsFill).Done;
        FCodeView.Sync;
        FCodeView.MoveHost(LCodeHost);
      end;
      PublishSourceInputFrame(LSourceContext, LSourceFile, LSourceText,
        FCodeView.Root, LCodeHost);

      if FSourceInput <> nil then
      begin
        FSourceInput.Bind;
        FCodeView.BindTextHistory('studio-code', FSourceInput.Commands);
      end;
      { This temporary empty parking host must leave the modal before a later
        MoveHost admission requires its target to contain no other controls. }
      FCodeParking.Parent := FHost;

      if FSourceFocusPending and (FState.SourceTab = nstSource) then
      begin
        TWinControl(FCodeView.InputFor('studio-code')).SetFocus;
        FSourceFocusPending := False;
        { Explicit workspace navigation owns focus even without a source-line
          request. A captured Parts or editing field must not reclaim it. }
        LChromeID := '';
      end;

      if FSourceLine > 0 then
      begin
        FCurrentProject.RecoveryFocus := srfNone;
        { An explicit callback/diagnostic destination supersedes the optional
          return-to-project caret that has not yet found a mounted source pane. }
        FRestoreProjectCode := False;
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

    if (LChromeID <> '') and LPartsFocus.Defined then
    begin
      { Row ordinals and Hide/Restore IDs may change or be reused. Only the
        copied form/instance/path can select this replacement. Missing or
        changed owners yield no focus target, never the old positional ID. }
      LChromeID := LPartsFocus.Resolve(FShellView.SectionRoot(nssInspector)).ID;
    end;

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

      if LPartsFocus.Defined and (Length(LPartsInputs) <> 0) then
      begin
        { The anchor has resolved against the current instance/path above.
          Replay only its Inspector viewport before restoring focus, including
          a chosen zero offset; source navigation has already cleared its ID. }

        if not FShellView.ViewFor(LChromeID).RestoreInteraction(LPartsInputs, False) then
        begin
          raise ENyxModel.Create('Parts viewport restoration became busy');
        end;
      end;

      if LChromeCollection.Defined and (LFocus is TCustomEdit) then
      begin
        { The rebuilt collection panel owns accepted/current pending text.
          A refused extent can leave the renderer's accepted baseline unchanged
          while its physical editor still holds the rejected input. Replay only
          this exact chrome identity through Nyx's guarded input restoration;
          no widget callback, document edit or history command is introduced. }
        SetLength(LChromeInputs, 1);
        LChromeNode := FShellView.ViewFor(LChromeID).Root.Find(LChromeID);
        { This is the accepted serialized chrome face, which can be Text or a
          numeric domain. Do not reinterpret its wire value as a Text argument. }
        LChromeNode.SetProp(NyxAttributeName(atValue), FShell.Find(LChromeID).Prop('value'));
        LChromeInputs[0] := CaptureNyxContentFace(LChromeNode);
        LChromeInputs[0].HasText := True;
        LChromeInputs[0].Text := FShell.Find(LChromeID).Prop('value');
        LChromeInputs[0].Accepted := LChromeInputs[0].Text;
        FShellView.ViewFor(LChromeID).RestoreInteraction(LChromeInputs, False);
      end;

      if (LFocus <> nil) and LFocus.CanFocus then
      begin
        { Regeneration follows the current user viewport, even if its focused
          Parts action has deliberately been scrolled out of view. }
        FShellView.ViewFor(LChromeID).FocusWithoutScroll(LChromeID, niRuntime);

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
    FEventFrameOwner := FSession.SelectedID;
    FEventFrameView := FSession.ActiveViewID;
    FEventFrameSchema := NyxSchemaRevision;
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
    CompleteRecoveryFocus;
    {$ifdef NYX_STUDIO_PROFILE}RecordPhase('paint-finish');{$endif}
  finally
    LShell.Free;
    FPainting := False;
  end;
  CompletionChanged;
end;

procedure TNyxNativeStudio.CompleteRecoveryFocus;
var
  LID: TNyxText;
begin

  if FCurrentProject = nil then
  begin
    Exit;
  end;
  LID := NyxStudioRecoveryFocusID(FCurrentProject.RecoveryFocus);
  FCurrentProject.RecoveryFocus := srfNone;

  if LID <> '' then
  begin
    FShellView.FocusControl(LID, niRuntime);
  end;
end;

function TNyxNativeStudio.PendingEventFrameReady: Boolean;
begin
  Result := False;

  if (FSourceCommands = nil) or not FSourceCommands.EventPreparationOnly then
  begin
    Exit;
  end;
  { Classifier/schema reads precede the final local frame fence. In particular,
    a load identity alone cannot certify this selection or Inspector mount. }
  Result := FPendingEventPresentation and FRunning and not FPainting and
    not FChangingProject and not FQueued and not FReplaceCanvas and
    not FState.DisplayRecovery.BlocksInput and
    FSession.MatchesCommandContext(FShellCommandContext) and
    (FSession.SelectedID = FEventFrameOwner) and
    (FSession.ActiveViewID = FEventFrameView) and
    (NyxSchemaRevision = FEventFrameSchema) and (FPendingEventRoot <> nil) and
    (FShellView.SectionRoot(nssInspector) = FPendingEventRoot) and
    (FPendingEventFooter <> nil) and
    (FShellView.Root.Find('studio-status') = FPendingEventFooter);
end;

function TNyxNativeStudio.TryPendingEventPresentation: Boolean;
begin
  Result := False;

  if FPendingEventPresentation then
  begin
    Exit;
  end;
  FPendingEventPresentation := True;
  try
    try
      FPendingEventRoot := FShellView.SectionRoot(nssInspector);
      FPendingEventFooter := FShellView.Root.Find('studio-status');

      if not PendingEventFrameReady then
      begin
        Exit;
      end;

      if not TryRefreshNyxPendingEvents(FPendingEventRoot, FSession,
          FState.CallbackRemoval, FSourceCommands.PendingDesign,
          FState.EventFilter, PendingEventFrameReady) then
      begin
        Exit;
      end;
      { Existing models and controls remain mounted. Source activity was already
        synchronized by the notification; only event fields and status follow. }
      FShellView.SectionView(nssInspector).Sync;

      if not PendingEventFrameReady then
      begin
        Exit;
      end;
      FPendingEventFooter.Configure.Text(FState.Status).Done;
      FShellView.ViewFor(FPendingEventFooter.ID).Sync;
      Result := True;
    except
      { A presentation failure never publishes a pair or skips ordinary recovery. }
      Result := False;
    end;
  finally
    FPendingEventRoot := nil;
    FPendingEventFooter := nil;
    FPendingEventPresentation := False;
  end;
end;

procedure TNyxNativeStudio.SourceCommandChanged(AState: TNyxSourceCommandState;
  const AMessage: TNyxText);
var
  LProgramReference: TNyxProgramRef;
  LRestore: TNyxProjectionValueRestore;
  LRestoreCanvas: Boolean;
  LCreatedName: TNyxText;
  LNameField: TNyxNode;
  LEvent: TNyxStudioEventIntent;
  LOwner: TNyxText;
  LView: TNyxText;
  LHandler: TNyxHandlerRef;
begin

  if FSourceCommands.CompletedProgram(LProgramReference) then
  begin
    FState.ProgramReference := LProgramReference;
    FState.ProgramDraft.Clear;
  end;
  FState.Status := AMessage;
  FState.SourceStatus := AMessage;

  if FSourceCommands.CompletedFileChange.IsDefined then
  begin

    if FState.SourceFileRemoval.IsDefined and
      FState.SourceFileRemoval.FileReference.SameFile(
        FSourceCommands.CompletedFileChange.FileReference) then
    begin
      FState.SourceFileRemoval := Default(TNyxStudioFileRemovalReview);
    end;

    if (FState.SourceTab = nstFiles) and
      FSession.ActiveSourceFile.SameFile(FSourceCommands.CompletedFileChange.FileReference) and
      FState.SourceFilesDraft.AcceptCreation(FSourceCommands.CompletedFileChange) then
    begin
      FState.SourceTab := nstSource;
      FSourceLine := 1;
      FSourceColumn := 1;
    end;
  end;
  { Scalar preparation deliberately retains the existing source pane. Update
    its owned activity face immediately; a queued chrome rebuild can otherwise
    arrive after the worker has already completed. The nested editor stays owned
    by its independent view, with its text, focus and caret unchanged. }

  if RestoreNyxStudioSourceActivity(FSourcePaneView.Root, SourceBusy) then
  begin
    FSourcePaneView.Sync;
  end;

  if FSourceCommands.PublishedProject then
  begin
    ProjectOpened;
  end;

  if FImportOpening and (AState in [nssRejected, nssStale, nssCancelled, nssFailed]) then
  begin
    FImportOpening := False;
  end;

  if FSourceCommands.CompletedEvent(LEvent, LOwner, LView, LHandler) then
  begin

    if (LEvent.Action = seaAdd) and (LHandler.Name <> '') and
      (FSession.SelectedID = LOwner) and (FSession.ActiveViewID = LView) then
    begin
      FSession.ActiveSourceFile := FSession.SourceFile(0);
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

  if (AState = nssPreparing) and TryPendingEventPresentation then
  begin
    Exit;
  end;
  RequestRefresh(((AState = nssApplied) and not FSourceCommands.PublishedCanvasAdoption) or
    LRestoreCanvas);
end;

procedure TNyxNativeStudio.RenderStudioCanvas(AHost: TWinControl);
var
  LNext: INyxStudioPreviewOwner;
  LRuntime: TNyxStudioPreviewRuntime;
  LRoot: TNyxRootRef;
begin

  if FPreviewSlot = nil then
  begin
    FPreviewSlot := TNyxStudioPreviewSlot.Create(FCanvasView.Events.Scheduler);
  end;

  if FPreview then
  begin
    LRoot := NyxPageRoot(FSession.ActiveViewID);

    if FSession.Document.FindComponent(FSession.ActiveViewID) = FSession.ActiveView then
    begin
      LRoot := NyxReusableRoot(FSession.ActiveViewID);
    end;
    LNext := NewNyxStudioPreviewOwner(FSession.Document, LRoot,
      npfNativeLCL, FCanvasView.Events.Scheduler);
    LRuntime := LNext.Runtime;
    { The source stages exact candidate routes in the prospective event view.
      Install adopts this managed owner before any notification can fail; local
      interface release can never free stores borrowed by accepted controls. }
    FCanvasView.RenderContext(FSession.Document, FSession.ActiveView, AHost,
      False, LRuntime.State, LRuntime.Bindings, LRuntime.Context, nil,
      nrmAuthoredDefaults, NewNyxStudioPreviewSource(FPreviewSlot, LNext, FCanvasView.Events));
  end
  else
  begin
    { Design is an explicit successor that retires Interact at the same commit
      boundary, preserving the former runtime if hidden preparation refuses. }
    FCanvasView.RenderContext(FSession.Document, FSession.ActiveView, AHost,
      True, nil, nil, nil, nil, nrmAuthoredDefaults,
      NewNyxStudioPreviewSource(FPreviewSlot, nil, FCanvasView.Events));
  end;
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
var
  LContext: TNyxStudioCommandContext;
  LFile: TNyxPascalSourceRef;
  LRoot: TNyxNode;
  LHost: TWinControl;
begin

  if FPainting or FChangingProject then
  begin
    Exit;
  end;
  LContext := FSourceInputContext;
  LFile := FSourceInputFile;
  LRoot := FSourceInputRoot;
  LHost := FSourceInputHost;
  try

    if ((FSourceInput <> nil) and FSourceInput.Change(ANode, AEvent.Trigger)) or
      ((FSourceInput = nil) and RouteNyxStudioSource(FSession, ANode, AEvent.Trigger)) then
    begin
      { Change admitted this exact physical edit before advancing the draft.
        Recheck its retained owner before any bridge/queued completion follows. }
      PublishSourceInputFrame(LContext, LFile, FSession.ActiveSourceText, LRoot, LHost);
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

function TNyxNativeStudio.MatchesSourceInputFrame: Boolean;
begin
  Result := (FSession <> nil) and (FCodeView <> nil) and
    (FSourceInputRoot <> nil) and (FCodeView.Root = FSourceInputRoot) and
    (FShell <> nil) and (FShell.Find('studio-source-mount') <> nil) and
    (FSourcePaneView <> nil) and (FSourcePaneView.Root <> nil) and
    (FSourcePaneView.Root.Find('studio-code-host') <> nil) and
    (FSourcePaneView.ControlFor('studio-code-host') = FSourceInputHost) and
    FSession.MatchesCommandContext(FSourceInputContext) and
    FSession.ActiveSourceFile.SameFile(FSourceInputFile) and
    (FSession.ActiveSourceText = FSourceInputText);
end;

procedure TNyxNativeStudio.PublishSourceInputFrame(const AContext: TNyxStudioCommandContext;
  const AFile: TNyxPascalSourceRef; const AText: TNyxText;
  ARoot: TNyxNode; AHost: TWinControl);
begin
  { Call only after the complete physical write succeeds. Validate the facts
    captured before that write, rather than blessing a newly current session. }
  FSourceInputAvailable := False;
  FSourceInputContext := AContext;
  FSourceInputFile := AFile;
  FSourceInputText := AText;
  FSourceInputRoot := ARoot;
  FSourceInputHost := AHost;
  FSourceInputAvailable := MatchesSourceInputFrame;
end;

function TNyxNativeStudio.ReadSourceInput(out ASession: TNyxStudioSession): TNyxEditingSnapshot;
begin
  ASession := FSession;
  Result := Default(TNyxEditingSnapshot);

  if FSourceInputAvailable and MatchesSourceInputFrame and
    FState.CodeVisible and not FState.CanvasExpanded and (FState.SourceTab = nstSource) and
    (FCodeView <> nil) and (FCodeView.Root <> nil) and
    not FSourceInputWriting and NyxInteractionPolicy(FCodeView.Root).CanEditValue then
  begin
    Result := FCodeView.EditingFor('studio-code');
  end;
end;

function TNyxNativeStudio.ReadCompletionContext: INyxStudioCompletionContext;
begin
  Result := nil;

  if CurrentBridge <> nil then
  begin
    Result := CurrentBridge.CompletionContext;
  end;
end;

function TNyxNativeStudio.ReadCompletionOwner: TNyxPascalSourceRef;
var
  LContext: INyxStudioSourceOutlineContext;
begin
  Supports(FSourceOutline, INyxStudioSourceOutlineContext, LContext);
  Result := LContext.CompletionOwner(FSession);
end;

procedure TNyxNativeStudio.CompletionChanged;
begin

  if not FRunning or FCompletionQueued then
  begin
    Exit;
  end;
  FCompletionQueued := True;
  Application.QueueAsyncCall(UpdateCompletionPopup, 0);
end;

procedure TNyxNativeStudio.UpdateCompletionPopup(AData: PtrInt);
var
  LDocument: TNyxDocument;
  LLine: Integer;
  LColumn: Integer;
begin
  FCompletionQueued := False;

  if not FRunning or (FSourceInput = nil) then
  begin
    Exit;
  end;

  if FPainting or FQueued or ((FCodeView.Root <> nil) and not FCodeView.SectionPublicationReady) or
    ((FCompletionPopover <> nil) and not FCompletionPopover.Renderer.SectionPublicationReady) then
  begin
    CompletionChanged;
    Exit;
  end;
  { The popup callback has returned. Recheck the physical source and service
    before scheduling the ordinary pane refresh/focus for a project definition. }

  if FSourceInput.TakeSourceNavigation(LLine, LColumn) then
  begin
    FState.SourceTab := nstSource;
    FSourceLine := LLine;
    FSourceColumn := LColumn;
    FSourceFocusPending := True;
    FState.CodeVisible := True;
    FState.Panel := nspDesign;
    RequestRefresh(False);
    Exit;
  end;
  FSourceInput.Bind;

  if FCompletionRevision = FSourceInput.CompletionRevision then
  begin
    Exit;
  end;
  FCompletionRevision := FSourceInput.CompletionRevision;

  if FCompletionPopover <> nil then
  begin
    FCompletionPopover.Close;
  end;
  FCompletionPopover := nil;
  LDocument := FSourceInput.BuildCompletion;
  try

    if LDocument <> nil then
    begin
      FCompletionPopover := NewNyxLCLPopover(FCodeView.FocusFor('studio-code'),
        LDocument, NyxPageRoot('studio-completion'), FTheme);
      FCompletionPopover.Renderer.OnEvent := CompletionEvent;
      FCompletionPopover.OnDismiss.Subscribe(FSourceInput.CompletionDismissReceiver);
      FCompletionPopover.Open(NyxPopover('Pascal completion').Size(540, 360)
        .Spacing(6, 8).DismissOn([npdEscape, npdOutsidePress]));
    end;
  finally
    LDocument.Free;
  end;
end;

procedure TNyxNativeStudio.CompletionEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if FCompletionPopover <> nil then
  begin
    FSourceInput.RouteCompletion(FCompletionPopover.Renderer.Root, ANode, AEvent.Trigger);
  end;
end;

procedure TNyxNativeStudio.WriteSourceInput(const AFrame: TNyxTextEditorFrame; AChanged: Boolean);
var
  LPainting: Boolean;
  LContext: TNyxStudioCommandContext;
  LFile: TNyxPascalSourceRef;
  LRoot: TNyxNode;
  LHost: TWinControl;
begin
  LContext := FSourceInputContext;
  LFile := FSourceInputFile;
  LRoot := FSourceInputRoot;
  LHost := FSourceInputHost;
  FSourceInputAvailable := False;
  LPainting := FPainting;
  FPainting := True;
  FSourceInputWriting := True;
  try
    FCodeView.Root.Configure.Value(AFrame.AuthoredText).Done;
    FCodeView.Sync;
    RestoreNyxLCLInputText(TWinControl(FCodeView.InputFor('studio-code')), AFrame.Text);

    if AFrame.Selection.Defined then
    begin
      FCodeView.SetTextSelection('studio-code', AFrame.Selection);
    end;
    PublishSourceInputFrame(LContext, LFile, AFrame.AuthoredText, LRoot, LHost);
  finally
    FSourceInputWriting := False;
    FPainting := LPainting;
  end;

  if not AChanged then
  begin
    Exit;
  end;
  FState.Status := 'Pascal draft / apply when ready';

  if CurrentBridge <> nil then
  begin
    CurrentBridge.RecordDraft;
  end;
end;

procedure TNyxNativeStudio.UpdateTitleAndSource;
var
  LContext: TNyxStudioCommandContext;
  LFile: TNyxPascalSourceRef;
  LText: TNyxText;
  LRoot: TNyxNode;
  LHost: TWinControl;
begin
  FPainting := True;
  try

    if FCodeView.Root <> nil then
    begin
      LContext := FSession.CommandContext;
      LFile := FSession.ActiveSourceFile;
      LText := FSession.ActiveSourceText;
      LRoot := FCodeView.Root;
      LHost := FSourceInputHost;
      FSourceInputAvailable := False;
      FCodeView.Root.Configure.Value(LText).Done;
      FCodeView.Sync;
      PublishSourceInputFrame(LContext, LFile, LText, LRoot, LHost);

      if FSourceInput <> nil then
      begin
        FSourceInput.Bind;
      end;
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

function TNyxNativeStudio.CreateProjectFiles: INyxTextFileExchange;
begin
  Result := NewNyxLCLTextFiles;
end;

procedure TNyxNativeStudio.ImportProject;
begin

  if not FSourceCommands.CanOpenProject then
  begin
    raise ENyxModel.Create('Wait for pending editor changes before importing files');
  end;

  if FProjectFiles = nil then
  begin
    FProjectFiles := CreateProjectFiles;
  end;
  FImportContext := FSession.CommandContext;
  FImportSnapshot := EncodeNyxProject(FSession.ProjectSnapshot);
  FProjectFiles.Pick(NyxStudioProjectFiles, ProjectFilesPicked);
end;

procedure TNyxNativeStudio.ProjectFilesPicked(AStatus: TNyxFilePickStatus;
  const AFiles: TNyxTextFiles; const AError: TNyxText);
begin

  if AStatus = fpsCancelled then
  begin
    Exit;
  end;
  try

    if not FSourceCommands.CanOpenProject then
    begin
      raise ENyxProjectConflict.Create('Another project operation is running; current input is retained');
    end;

    if not FSession.MatchesCommandContext(FImportContext) then
    begin
      raise ENyxModel.Create('Selected project files belong to an earlier project');
    end;

    if AStatus = fpsFailed then
    begin
      raise ENyxFile.Create(AError);
    end;
    FImportPacket := ReadNyxStudioProjectFiles(AFiles);
    FOpeningBoundProject := '';
    FOpeningRevision := '';
    FState.FilesVisible := True;
    FState.Panel := nspProject;

    if EncodeNyxProject(FSession.ProjectSnapshot) <> FImportSnapshot then
    begin
      raise ENyxProjectConflict.Create('Your project changed while reading files. Choose before opening');
    end;
    AcceptImport(nprRequireMatch);
  except
    on LException: Exception do
    begin
      FState.Status := LException.Message;
    end;
  end;
  RequestRefresh(True);
end;

procedure TNyxNativeStudio.AcceptImport(AResolution: TNyxProjectResolution);
var
  LPair: TNyxProjectPair;
  LIdentity: TGUID;
  LRevision: TNyxText;
  LRemote: TNyxText;
begin

  if not FSourceCommands.CanOpenProject then
  begin
    raise ENyxModel.Create('Wait for pending editor changes before opening files');
  end;

  if FImportPacket = '' then
  begin
    raise ENyxModel.Create('No imported project is awaiting resolution');
  end;
  LPair := DecodeNyxProject(FImportPacket);
  { Preserve the current admitted pair before compiler work begins. Failed
    execution keeps this live pair/history and the imported input; neither file
    input nor a pending draft becomes an opaque live checkpoint on its own. }
  CreateGUID(LIdentity);

  if not FStore.SaveCapturedProject('backup-' + Copy(GUIDToString(LIdentity), 2, 36),
    '', FSession.ProjectSnapshot, FSession.AcceptedSourceCheckpoint, LRevision, LRemote) then
  begin
    raise ENyxModel.Create('Cannot back up the current project; imported input is retained');
  end;
  FImportOpening := True;
  try
    FSourceCommands.OpenProject(LPair, AResolution);
  except
    FImportOpening := False;
    raise;
  end;
end;

procedure TNyxNativeStudio.ExportProject(ACompanion: Boolean; AInput: Boolean);
var
  LFiles: TNyxTextFiles;
begin

  if SourceBusy then
  begin
    raise ENyxModel.Create('Wait for pending editor changes before exporting files');
  end;

  if FProjectFiles = nil then
  begin
    FProjectFiles := CreateProjectFiles;
  end;

  if AInput then
  begin

    if FImportPacket = '' then
    begin
      raise ENyxModel.Create('No imported input is available to export');
    end;
    LFiles := nil;
    SetLength(LFiles, 1);
    LFiles[0] := NyxTextFile('imported-project.nyxproject', FImportPacket);
  end
  else if ACompanion then
  begin
    LFiles := NyxStudioProjectCompanion(FSession.ProjectSnapshot);
  end
  else
  begin
    LFiles := NyxStudioProjectBackup(FSession.ProjectSnapshot);
  end;
  { This copied read never rebuilds mounted controls or records document history.
    Host cancellation may follow an earlier delivery in a multi-file export. }
  FProjectFiles.ExportFiles(LFiles);
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

  if not FStore.SaveCapturedProject(FState.ProjectName, LExpected,
    FSession.ProjectSnapshot, FSession.AcceptedSourceCheckpoint, FRemoteRevision, FRemotePair) then
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

  if not FSourceCommands.CanOpenProject then
  begin
    raise ENyxModel.Create('Wait for pending editor changes before opening a project');
  end;
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
  FImportPacket := FRemotePair;
  FOpeningBoundProject := FState.ProjectName;
  FOpeningRevision := FRemoteRevision;
  AcceptImport(nprRequireMatch);
end;

procedure TNyxNativeStudio.AcceptRemote;
begin

  if not FSourceCommands.CanOpenProject then
  begin
    raise ENyxModel.Create('Wait for pending editor changes before opening a project');
  end;

  if not FState.ProjectConflict or (FRemotePair = '') then
  begin
    raise ENyxModel.Create('No pending project conflict');
  end;
  { Preserve the current exact pair before the explicit warned-open action. A
    failed backup or candidate admission leaves the current session untouched. }
  FImportPacket := FRemotePair;
  FOpeningBoundProject := FState.ProjectName;
  FOpeningRevision := FRemoteRevision;
  AcceptImport(nprRequireMatch);
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
  LAnchor: INyxLCLPopoverAnchor;
begin
  { Preserve the independent open menu only for this exact project/selection/
    command state and retained native anchor. Current button registrations are
    borrowed from the newly observed section router; no retired node is reused. }

  if (FActionMenu <> nil) and
    FActionMenuState.Matches(FSession) and
    Supports(FActionMenu.Presentation, INyxLCLPopoverAnchor, LAnchor) and
    LAnchor.AnchoredTo(FShellView.FocusFor(NyxStudioActionMenuID)) then
  begin
    FActionButton := nil;
    FActionButton := NewNyxMenuButton(RetainNyxControl(
      FShellView.Root.Find(NyxStudioActionMenuID)) as INyxButton,
      FShellView.ViewFor(NyxStudioActionMenuID).Events, FActionMenu,
      NyxMenu('Component actions'));
    Exit;
  end;
  FActionButton := nil;

  if FActionMenu <> nil then
  begin
    FActionMenu.Close;
  end;
  FActionMenu := nil;
  LContent := BuildNyxStudioActionMenu(FSession, LItems, True);
  try
    FActionMenu := NewNyxLCLMenu(FShellView.FocusFor(NyxStudioActionMenuID),
      LContent, NyxPageRoot(NyxStudioActionMenuRoot), LItems, FTheme);
    FActionMenu.OnInvoke.Subscribe(NewNyxStudioMenuCallback(MenuAction));
    FActionButton := NewNyxMenuButton(RetainNyxControl(
      FShellView.Root.Find(NyxStudioActionMenuID)) as INyxButton,
      FShellView.ViewFor(NyxStudioActionMenuID).Events, FActionMenu, NyxMenu('Component actions'));
    FActionMenuState := TNyxStudioActionMenuState.Capture(FSession);
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
  LDiagramAction: TNyxDiagramLibraryAction;
  LDiagramReference: TNyxDiagramRef;
  LInspectorGroup: TNyxInspectorGroup;
  LQueryPreview: TNyxStudioQueryPreview;
  LEffect: TNyxInspectorEffect;
  LRemoval: TNyxCallbackRemoval;
  LDiagnostic: TNyxSourceDiagnostic;
  LBuildInspection: TNyxBuildDiagnosticInspection;
  LBuildSynchronized: Boolean;
  LLine: Integer;
  LChanged: Boolean;
  LBefore: TNyxText;
  LBackup: TGUID;
  LRecoveryBackup: TNyxProjectPair;
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
  LLogicCapture: TNyxLogicEditorCapture;
  LProgramReference: TNyxProgramRef;
  LResourcePane: TNyxResourceWorkspacePane;
  LContentFocus: TWinControl;
  LEventFilter: TNyxEventFilter;
  LSourceFilesEffect: TNyxStudioSourceFilesEffect;
  LOutlineEffect: TNyxStudioSourceOutlineEffect;
  LOutlineLocation: TNyxPascalRoutineLocation;
begin

  if not FPainting and not FChangingProject and
    FSession.MatchesCommandContext(FShellCommandContext) and
    (FShellView.Root.Find(ANode.ID) = ANode) and
    RouteNyxEventsFilter(ANode, AEvent.Trigger,
      FShellView.RootFor(NyxStudioEventFilterID), LEventFilter) then
  begin
    FState.EventFilter := LEventFilter;

    if FCurrentProject <> nil then
    begin
      FCurrentProject.State.EventFilter := LEventFilter;
    end;
    FShellView.Sync;
    Exit;
  end;

  if (AEvent.Trigger = ntClick) and TryNyxInspectorGroup(ANode.ID, LInspectorGroup) then
  begin

    if not FPainting and not FChangingProject and
      FSession.MatchesCommandContext(FShellCommandContext) and
      (FShellView.Root.Find(ANode.ID) = ANode) then
    begin
      FState.InspectorGroups := FState.InspectorGroups.Captured(LInspectorGroup, ANode);

      if FCurrentProject <> nil then
      begin
        FCurrentProject.State.InspectorGroups := FState.InspectorGroups;
      end;
    end;
    { Keep the mounted input/scroll frame; this is not a source command. }
    Exit;
  end;

  if not FPainting and not FChangingProject and
    (AEvent.Trigger = ntClick) and
    NyxViewRecoveryAction(ANode, AEvent, NyxStudioDisplayRecoveryID) then
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

    if (AEvent.Trigger = ntClick) and not FPainting and not FChangingProject and
      FSession.MatchesCommandContext(FShellCommandContext) and
      (FShellView.Root.Find(ANode.ID) = ANode) and
      (PreviewNyxReusableInspector(FSession, ANode,
        FShellView.RootFor(NyxStudioReusableEditorID)) or
        PreviewNyxExpressionInspector(FSession, ANode,
          FShellView.RootFor(NyxStudioExpressionEditorID))) then
    begin
      FShellView.Sync;
      Exit;
    end;
    if (AEvent.Trigger = ntChange) and (ANode.SourceID =
      NyxExpressionEditorFieldID(NyxStudioExpressionEditorID, eefSource)) then
    begin
      FState.ExpressionEditorDraft.Capture(NyxStudioExpressionEditorID,
        FShellView.RootFor(NyxStudioExpressionEditorID));
      Exit;
    end;

    if FLocalSourceAvailable and (AEvent.Trigger = ntChange) and
      SetNyxLocalSourceField(ANode.ID, ANode.Prop('value'), FLocalSourceSettings) then
    begin
      { Keep notifying inputs and the next pressed action mounted through blur.
        A later paint presents the unapplied-settings state; no pair is touched. }
      Exit;
    end;

    if FLocalSourceAvailable and (AEvent.Trigger = ntClick) and
      (NyxLocalSourceAction(ANode.ID) <> lsaUnknown) then
    begin
      case NyxLocalSourceAction(ANode.ID) of
        lsaConfigure:
        begin
          ConfigureLocalSource(FLocalSourceSettings);
        end;
        lsaDisable:
        begin
          DisableLocalSource;
        end;
      else
        begin
          raise ENyxModel.Create('Unknown local source configuration action');
        end;
      end;
      RequestRefresh;
      Exit;
    end;

    if (CurrentBridge <> nil) and CurrentBridge.RouteBuildInspection(ANode, AEvent.Trigger) then
    begin
      RequestRefresh;
      Exit;
    end;

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
              { Selection metadata has already been admitted by the public
                proposal operation. A full repaint would compare its former
                private baseline and replace these exact live inputs. Show the
                new variant awaiting its reply without obsolete observations. }
              SuspendNyxStudioResourceSelectionViews(FShellView.RootFor('studio-resource-editor'),
                FState.ResourceSelection, FState.Agents.CanInspectResourceRuntime);
              FShellView.Sync;
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

    if (AEvent.Trigger = ntClick) and
      ((ANode.ID = 'action-panel-project') or (ANode.ID = 'action-panel-design') or
      (ANode.ID = 'action-panel-inspector')) then
    begin
      FState.ProgramsVisible := False;
    end;

    { Program text is a separate local proposal. Retain it before any refusal;
      only Save/Remove fall through to the ordinary paired source command. }

    if (AEvent.Trigger = ntClick) and
      (FShellView.Root.Find(ANode.ID) = ANode) and
      CaptureNyxDiagramLibrary(ANode, LDiagramAction, LDiagramReference) then
    begin
      case LDiagramAction of
        ndlaNew: ShowDiagram(Default(TNyxDiagramRef));
        ndlaOpen: ShowDiagram(LDiagramReference);
        ndlaResume: ResumeDiagram;
      else
        raise ENyxDiagram.Create('Unknown diagram library intent');
      end;
      Exit;
    end;

    if (AEvent.Trigger in [ntClick, ntChange]) and
      (Pos('studio-program-editor-', ANode.ID) = 1) then
    begin
      FState.ProgramDraft.Capture('studio-program-editor', FShellView.RootFor(ANode.ID));

      if AEvent.Trigger = ntChange then
      begin
        Exit;
      end;

      if HandleNyxProgramEditor(ANode, FShellView.RootFor(ANode.ID)) then
      begin
        FState.ProgramDraft.Capture('studio-program-editor', FShellView.RootFor(ANode.ID));
        FShellView.Sync;
        Exit;
      end;

    if OpenNyxProgramEditorChoice(ANode, FShellView.RootFor(ANode.ID), LProgramReference) then
      begin
        FState.ProgramReference := LProgramReference;
        FState.ProgramDraft.Clear;
        RequestRefresh;
        Exit;
      end;
    end;

    if (AEvent.Trigger = ntClick) and
      ((ANode.ID = 'action-programs-toggle') or (ANode.ID = 'action-programs-close')) then
    begin
      FState.ProgramsVisible := (ANode.ID <> 'action-programs-close') and not FState.ProgramsVisible;
      FState.CanvasExpanded := False;

      if FState.ProgramsVisible then
      begin
        FState.LogicVisible := False;
        FState.ResourcesVisible := False;
      end;
      RequestRefresh;
      Exit;
    end;

    if (AEvent.Trigger in [ntClick, ntChange]) and
      (HandleNyxControlRouteEditor(ANode, FShellView.RootFor(ANode.ID), FSession.Document, 0) or
      HandleNyxFlowRowsEditor(ANode, FShellView.RootFor(ANode.ID), FSession.Document, 0) or
      HandleNyxLogicEditor(ANode, FShellView.RootFor(ANode.ID))) then
    begin
      FState.LogicDraft.Capture('studio-logic-editor', FShellView.RootFor(ANode.ID));
      FShellView.Sync;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and CaptureNyxLogicEditor(ANode,
      FShellView.RootFor(ANode.ID), LLogicCapture) and LLogicCapture.Open then
    begin
      FState.LogicReference := LLogicCapture.Reference;
      FState.LogicDraft.Clear;
      RequestRefresh;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and
      ((ANode.ID = 'action-logic-toggle') or (ANode.ID = 'action-logic-close')) then
    begin
      FState.LogicVisible := (ANode.ID <> 'action-logic-close') and not FState.LogicVisible;
      FState.ProgramsVisible := False;
      FState.CanvasExpanded := False;
      RequestRefresh;
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
      FState.ProgramsVisible := False;
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
              SuspendNyxStudioResourceSelectionViews(FShellView.RootFor('studio-resource-editor'),
                FState.ResourceSelection, FState.Agents.CanInspectResourceRuntime);
              FShellView.Sync;
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

    if RouteNyxCanvasItemPalette(ANode, FShellView.RootFor(ANode.ID), AEvent.Trigger) then
    begin
      FState.CanvasPaletteDraft.Capture('inspector-canvas-content', FShellView.RootFor(ANode.ID));
      FShellView.Sync;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and (ANode.ID = NyxStudioSourceOutlineTabID) then
    begin
      FState.SourceTab := nstOutline;
      RequestRefresh(False);
      Exit;
    end;
    LOutlineEffect := FState.SourceOutline.Route(FSession,
      FSourcePaneView.Root, ANode, AEvent, LOutlineLocation);

    if LOutlineEffect <> soeNone then
    begin

      if LOutlineEffect = soeNavigate then
      begin
        FState.SourceTab := nstSource;
        FSourceLine := LOutlineLocation.Line;
        FSourceColumn := LOutlineLocation.Column;
        FSourceFocusPending := True;
        FState.CodeVisible := True;
        FState.Panel := nspDesign;
      end;

      if LOutlineEffect <> soeField then
      begin
        RequestRefresh(False);
      end;
      Exit;
    end;
    LSourceFilesEffect := RouteNyxStudioSourceFiles(FSession, FSourceCommands,
      FSourcePaneView.Root, ANode, AEvent, FState.SourceFilesDraft, FState.SourceFileRemoval);

    if LSourceFilesEffect <> ssfeNone then
    begin

      if LSourceFilesEffect <> ssfeField then
      begin
        RequestRefresh;
      end;
      Exit;
    end;

    if (AEvent.Trigger = ntChange) and (ANode.ID = NyxStudioSourceFileID) then
    begin
      RouteNyxStudioSource(FSession, ANode, AEvent.Trigger);
      FSourceLine := 1;
      FSourceColumn := 1;
      RequestRefresh(False);
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and (ANode.ID = 'action-source-definition') then
    begin
      FSourceInput.RequestDefinition;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and (ANode.ID = 'action-source-back') then
    begin
      FSourceInput.NavigateBack;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and (ANode.ID = 'action-source-forward') then
    begin
      FSourceInput.NavigateForward;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and (ANode.ID = 'action-export-source-draft') then
    begin

      if FProjectFiles = nil then
      begin
        FProjectFiles := CreateProjectFiles;
      end;
      FProjectFiles.ExportFiles([NyxTextFile(
        FSession.SourceFileTitle(FSession.ActiveSourceFile), FSession.ActiveSourceText)]);
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
            FSession.ActiveSourceFile := FSession.SourceFile(0);
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

      if (LEffect <> nieNone) or not TryPendingEventPresentation then
      begin
        RequestRefresh;
      end;
      Exit;
    end;
  except
    on LException: Exception do
    begin

      if FLocalSourceAvailable and (AEvent.Trigger = ntClick) and
        (NyxLocalSourceAction(ANode.ID) <> lsaUnknown) then
      begin
        { Refusal belongs beside these fields as well as the global footer. This
          early routing boundary owns configuration before document commands. }
        FLocalSourceMessage := LException.Message;
      end;
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

    if RouteNyxStudioProjectNavigation(FState.ProjectNavigation, ANode, AEvent.Trigger) then
    begin
      FState.Panel := nspProject;
      FState.CanvasExpanded := False;

      if AEvent.Trigger = ntClick then
      begin
        RequestRefresh;
      end;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and (ANode.ID = 'action-project-create') then
    begin

      if (CurrentBridge = nil) or SourceBusy then
      begin
        raise ENyxModel.Create('Connect the service and finish the current operation before creating another project');
      end;
      CurrentBridge.CreateWorkspace(FState.ProjectNavigation.Name);
      FState.Status := 'Creating project / your current work stays open';
      RequestRefresh;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and (ANode.ID = 'action-project-browse') then
    begin
      FState.FilesVisible := True;
      FState.ProjectNavigation.Panel := sppClosed;
      FState.Panel := nspProject;
      FState.CanvasExpanded := False;
      RequestRefresh;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and (ANode.ID = 'action-project-save-current') then
    begin
      FState.FilesVisible := True;
      FState.Panel := nspProject;
      FState.CanvasExpanded := False;

      if FState.ProjectName = '' then
      begin
        FState.Status := 'Choose a saved project name, or export a portable backup';
      end
      else
      begin
        SaveProject;
      end;
      RequestRefresh;
      Exit;
    end;

    if (AEvent.Trigger = ntClick) and RouteNyxStudioWorkspace(FState, ANode.ID) then
    begin

      if (ANode.ID = 'action-details-toggle') and FState.DetailsExpanded and
        FState.AgentsVisible and (FCurrentProject <> nil) then
      begin

        if FState.Agents.Paused then
        begin
          FCurrentProject.RecoveryFocus := srfReconnect;
        end
        else if FState.Agents.Conflict then
        begin

          if FCurrentProject.RecoveryReview.Delivery <> sbdNone then
          begin
            FCurrentProject.RecoveryFocus := srfConfirm;
          end
          else
          begin
            FCurrentProject.RecoveryFocus := srfBackup;
          end;
        end;
      end;
      RequestRefresh;
      Exit;
    end;

    LBuildInspection := Default(TNyxBuildDiagnosticInspection);
    LBuildSynchronized := False;

    if CurrentBridge <> nil then
    begin
      LBuildInspection := CurrentBridge.State.BuildInspection;
      LBuildSynchronized := CurrentBridge.SourceSynchronized;
    end;

    if RouteNyxBuildDiagnostic(FSession, ANode, AEvent.Trigger,
      LBuildInspection, LBuildSynchronized, LDiagnostic) or
      RouteNyxCompilerDiagnostic(FSession, ANode, AEvent.Trigger,
      FCompilerReport, LDiagnostic) or
      RouteNyxSourceDiagnostic(FSession, ANode, AEvent.Trigger, LDiagnostic) then
    begin
      FSourceLine := LDiagnostic.Line;
      FSourceColumn := LDiagnostic.Column;
      FState.CodeVisible := True;
      FState.Panel := nspDesign;
    end
    else if (AEvent.Trigger = ntChange) and (ANode.ID = 'output-clipboard-writing') then
    begin
      FOutputs.AllowClipboardWriting(AEvent.Value.AsBoolean);
      FOutputEdits.Mark(nosClipboardWriting);
      FState.Status := 'Clipboard runtime setting edited / applied when building';
    end
    else if (AEvent.Trigger = ntChange) and (ANode.ID = 'output-http') then
    begin
      FOutputs.AllowHTTP(AEvent.Value.AsBoolean);
      FOutputEdits.Mark(nosHTTP);
      FState.Status := 'HTTP runtime setting edited / applied when building';
    end
    else if (AEvent.Trigger = ntChange) and (ANode.ID = 'output-file-opening') then
    begin
      FOutputs.AllowFileOpening(AEvent.Value.AsBoolean);
      FOutputEdits.Mark(nosFileOpening);
      FState.Status := 'File-open runtime setting edited / applied when building';
    end
    else if (AEvent.Trigger = ntChange) and (ANode.ID = 'output-file-saving') then
    begin
      FOutputs.AllowFileSaving(AEvent.Value.AsBoolean);
      FOutputEdits.Mark(nosFileSaving);
      FState.Status := 'File-save runtime setting edited / applied when building';
    end
    else if RouteNyxStudioQueryPreview(FSession, AEvent,
      FShellView.RootFor(ANode.ID), FSourceCommands.PendingDesign, LQueryPreview) then
    begin
      FState.QueryPreview := LQueryPreview;
      FState.Status := 'Query preview / design and Pascal retained';
    end
    else if RouteNyxStudioQueryPreviewExpansion(FSession, AEvent,
      FShellView.RootFor(ANode.ID), FState.QueryPreview) then
    begin
      ShowQueryPreview;
      Exit;
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
            FOutputEdits.MarkField(ANode.Prop('output-field'));
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
          ncSourceFilesTab:
            begin
              FState.SourceTab := nstFiles;
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
          ncSaveOutputs:
            begin
              ReadOutputs(FCurrentProject, False, osiSave);
            end;
          ncReloadOutputs:
            begin
              ReadOutputs(FCurrentProject, False, osiReload);
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
          ncProjectImport:
            begin
              ImportProject;
              Exit;
            end;
          ncProjectExport, ncProjectExportFiles, ncImportBackup:
            begin
              ExportProject(DecodeNativeCommand(ANode.ID) = ncProjectExportFiles,
                DecodeNativeCommand(ANode.ID) = ncImportBackup);
              Exit;
            end;
          ncImportPascal:
            begin
              AcceptImport(nprUsePascal);
            end;
          ncImportDesign:
            begin
              AcceptImport(nprUseDesign);
            end;
          ncImportCancel:
            begin

              if FImportOpening then
              begin
                FSourceCommands.Cancel;
                FImportOpening := False;
              end;
              FImportPacket := '';
              FState.ImportConflict := False;
              FState.Status := 'Import cancelled; current project retained';
            end;
          ncAgents:
            begin

              if FState.Agents.Conflict or FState.Agents.Paused then
              begin
                FState.AgentsVisible := True;

                if FState.Agents.Paused then
                begin
                  FCurrentProject.RecoveryFocus := srfReconnect;
                end
                else if FCurrentProject.RecoveryReview.Delivery <> sbdNone then
                begin
                  FCurrentProject.RecoveryFocus := srfConfirm;
                end
                else
                begin
                  FCurrentProject.RecoveryFocus := srfBackup;
                end;
              end
              else
              begin
                FState.AgentsVisible := not FState.AgentsVisible;
              end;
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
              FCurrentProject.RecoveryChoice := Default(TNyxStudioRecoveryChoice);
              FCurrentProject.RecoveryReview := Default(TNyxStudioRecoveryReview);
              FCurrentProject.RecoveryFocus := srfNone;
              CurrentBridge.Connect;
            end;
          ncAgentPause:
            begin
              FCurrentProject.RecoveryChoice := Default(TNyxStudioRecoveryChoice);
              FCurrentProject.RecoveryReview := Default(TNyxStudioRecoveryReview);
              CurrentBridge.Pause;
              FCurrentProject.RecoveryFocus := srfReconnect;
            end;
          ncAgentAccept:
            begin
              FCurrentProject.RecoveryChoice := Default(TNyxStudioRecoveryChoice);
              FCurrentProject.RecoveryReview := Default(TNyxStudioRecoveryReview);
              FCurrentProject.RecoveryChoice := CurrentBridge.ReviewRecovery;
              LRecoveryBackup := FCurrentProject.RecoveryChoice.Backup;
              CreateGUID(LBackup);

              if not FStore.SaveCapturedProject('backup-' + Copy(GUIDToString(LBackup), 2, 36),
                '', LRecoveryBackup, FSession.AcceptedSourceCheckpoint,
                LBackupRevision, LBackupRemote) then
              begin
                raise ENyxModel.Create('Cannot save the exact local backup; current work is retained');
              end;
              FCurrentProject.RecoveryReview.Delivery := sbdStored;
              FCurrentProject.RecoveryReview.BackupName := 'backup-' + Copy(GUIDToString(LBackup), 2, 36);
              FCurrentProject.RecoveryReview.SharedRevision := FCurrentProject.RecoveryChoice.SharedRevision;
              FState.DetailsExpanded := True;
              FCurrentProject.RecoveryFocus := srfConfirm;
            end;
          ncAgentConfirm:
            begin
              FCurrentProject.RecoveryFocus := srfNone;
              CurrentBridge.AcceptRemote(FCurrentProject.RecoveryChoice);
              FCurrentProject.RecoveryChoice := Default(TNyxStudioRecoveryChoice);
              FCurrentProject.RecoveryReview := Default(TNyxStudioRecoveryReview);
            end;
          ncAgentCancelReview:
            begin
              FCurrentProject.RecoveryChoice := Default(TNyxStudioRecoveryChoice);
              FCurrentProject.RecoveryReview := Default(TNyxStudioRecoveryReview);
              FState.Status := 'Recovery cancelled; your local project and Pascal are retained';
              FCurrentProject.RecoveryFocus := srfBackup;
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
