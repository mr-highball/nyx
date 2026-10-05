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
  nyx.studio.session, nyx.studio.view, nyx.studio.projects,
  nyx.studio.projectstore, nyx.studio.outputs, nyx.studio.rootedits,
  nyx.studio.compiler, nyx.studio.agentbridge, nyx.studio.agentview,
  nyx.studio.workspaces;

type
  TNyxNativeStudio = class;

  { Private native context owns one portable mirror and its immutable service
    bridge. Owner is borrowed. Views remain owned by Studio; this record retains
    typed editor state and scalar/scroll positions, never borrowed widget handles.
    Inactive bridges may observe their own project without painting another one. }
  TNyxNativeStudioProject = class
  public
    Owner: TNyxNativeStudio;
    Reference: TNyxWorkspaceRef;
    Session: TNyxStudioSession;
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
    procedure Refresh(AContentChanged: Boolean);
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
    FTheme: TNyxTheme;
    FShell: TNyxDocument;
    FCodeDocument: TNyxDocument;
    FShellView: TNyxLCLRenderer;
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
    FPaintCount: Integer;
    FServiceURL: TNyxText;
    FProjects: array of TNyxNativeStudioProject;
    FCurrentProject: TNyxNativeStudioProject;
    FPendingProject: TNyxNativeStudioProject;
    FChangingProject: Boolean;
    FRestoreProject: Boolean;
    FAgentCompilerSequence: Integer;
    FInitialPair: TNyxText;
    procedure HostResize(ASender: TObject);
    procedure PaintQueued(AData: PtrInt);
    procedure Paint;
    procedure ShellEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure CanvasEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure SourceEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure SaveProject;
    procedure OpenProject;
    procedure AcceptRemote;
    procedure CapturePresentation;
    { Update independent source/status controls without replacing the title field
      that is currently notifying. Guard programmatic source feedback. }
    procedure UpdateTitleAndSource;
    procedure AgentRefresh(AContentChanged: Boolean);
    procedure RecordLocal;
    procedure CaptureProject;
    procedure AdmitProject(AProject: TNyxNativeStudioProject);
    procedure RestoreProjectControls;
    function CurrentBridge: TNyxStudioAgentBridge;
    function GetAgentState: TNyxStudioAgentView;
    function ComposeShell: TNyxDocument;
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
    property ShellView: TNyxLCLRenderer read FShellView;
    property CanvasView: TNyxLCLRenderer read FCanvasView;
    property CodeView: TNyxLCLRenderer read FCodeView;
    property PaintCount: Integer read FPaintCount;
    property Status: TNyxText read FState.Status;
    property Agents: TNyxStudioAgentView read GetAgentState;
  end;

implementation

uses
  StdCtrls, nyx.editing, nyx.editing.lcl, nyx.contract, nyx.source,
  nyx.studio.commands, nyx.studio.authoring, nyx.studio.inspector,
  nyx.studio.palette, nyx.studio.source, nyx.studio.diagnostics, nyx.studio.rootview,
  nyx.studio.exchange.lcl, nyx.studio.agents, Math;

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
    ncBuildApplication);

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
    'action-build-app');

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

procedure TNyxNativeStudioProject.Refresh(AContentChanged: Boolean);
begin

  if (Owner <> nil) and (Owner.FPendingProject = Self) then
  begin
    Owner.AdmitProject(Self);
  end;

  if (Owner <> nil) and (Owner.FCurrentProject = Self) then
  begin
    Owner.AgentRefresh(AContentChanged);
  end;
end;

destructor TNyxNativeStudioProject.Destroy;
begin
  Owner := nil;
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
  FTheme := TNyxTheme.Create;
  FOutputs := TNyxOutputConfiguration.Create;
  FStore := TNyxProjectStore.Create(AProjectDirectory);
  FState := DefaultNyxStudioViewState;
  FState.CodePresentation := ncpHosted;
  FState.Outputs := FOutputs;
  FShellView := TNyxLCLRenderer.Create(FTheme);
  FCanvasView := TNyxLCLRenderer.Create(FTheme);
  FCodeView := TNyxLCLRenderer.Create(FTheme);
  FShellView.OnEvent := ShellEvent;
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
  { Remove this object's queued callbacks before any view/session lifetime ends. }
  Application.RemoveAsyncCalls(Self);
  { Detach every context before joining even one worker. Waiting for a native
    thread may dispatch host synchronization; no other context may consult the
    current editor after its views or an earlier context have been released. }
  for LIndex := 0 to High(FProjects) do
  begin
    FProjects[LIndex].Owner := nil;
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

  if LState.Conflict then
  begin
    FState.AgentsVisible := True;
  end;
  RequestRefresh(AContentChanged);
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
    LProject.Bridge := TNyxStudioAgentBridge.Create(FSession, LProject.Refresh,
      AWorkspace, TNyxLCLEditorExchange.Create(ABaseURL));
    SetLength(FProjects, 1);
    FProjects[0] := LProject;
    FCurrentProject := LProject;
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
  LCanvas: TControl;
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
    LCanvas := FCanvasView.ControlFor(FCanvasView.Root.ID).Parent;

    if LCanvas is TScrollBox then
    begin
      FCurrentProject.CanvasTop := TScrollBox(LCanvas).VertScrollBar.Position;
      FCurrentProject.CanvasLeft := TScrollBox(LCanvas).HorzScrollBar.Position;
    end;
  end;
end;

procedure TNyxNativeStudio.RestoreProjectControls;
var
  LInput: TWinControl;
  LLength: Integer;
  LStart: Integer;
  LEnd: Integer;
  LCanvas: TControl;
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
    LCanvas := FCanvasView.ControlFor(FCanvasView.Root.ID).Parent;

    if LCanvas is TScrollBox then
    begin
      TScrollBox(LCanvas).VertScrollBar.Position := FCurrentProject.CanvasTop;
      TScrollBox(LCanvas).HorzScrollBar.Position := FCurrentProject.CanvasLeft;
    end;
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
      LProject.State := DefaultNyxStudioViewState;
      LProject.State.CodePresentation := ncpHosted;
      LProject.State.Outputs := FOutputs;
      LProject.SavedPair := EncodeNyxProject(LProject.Session.ProjectSnapshot);
      LProject.Bridge := TNyxStudioAgentBridge.Create(LProject.Session,
        LProject.Refresh, AWorkspace, TNyxLCLEditorExchange.Create(FServiceURL));
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
        FCodeView.NavigateCodeLine('studio-code', FSourceLine);
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
    FReplaceCanvas := False;
    RestoreProjectControls;
    FChangingProject := False;
    Inc(FPaintCount);
  finally
    LShell.Free;
    FPainting := False;
  end;
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
      RecordLocal;
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
begin

  if FPainting or FChangingProject then
  begin
    Exit;
  end;
  LChanged := False;
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
              raise ENyxModel.Create('Native build requests are unavailable');
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
