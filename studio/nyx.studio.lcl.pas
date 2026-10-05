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
  nyx.studio.compiler;

type
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
    { Borrowed public contracts for embedding/qualification. Never free them or
      rebuild them from inside a native widget notification. }
    property Session: TNyxStudioSession read FSession;
    property ShellView: TNyxLCLRenderer read FShellView;
    property CanvasView: TNyxLCLRenderer read FCanvasView;
    property CodeView: TNyxLCLRenderer read FCodeView;
    property PaintCount: Integer read FPaintCount;
    property Status: TNyxText read FState.Status;
  end;

implementation

uses
  StdCtrls, nyx.editing, nyx.editing.lcl, nyx.contract, nyx.source,
  nyx.studio.commands, nyx.studio.authoring, nyx.studio.inspector,
  nyx.studio.palette, nyx.studio.source, nyx.studio.diagnostics, nyx.studio.rootview;

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
    ncProjectCopy);

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
    'action-project-copy');

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
end;

destructor TNyxNativeStudio.Destroy;
begin
  FRunning := False;
  { Remove this object's queued callbacks before any view/session lifetime ends. }
  Application.RemoveAsyncCalls(Self);

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
  FSession.Free;
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
    Inc(FPaintCount);
  finally
    LShell.Free;
    FPainting := False;
  end;
end;

procedure TNyxNativeStudio.SourceEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if FPainting then
  begin
    Exit;
  end;
  try

    if RouteNyxStudioSource(FSession, ANode, AEvent.Trigger) then
    begin
      FState.Status := 'Pascal draft / apply when ready';
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

  if FPainting then
  begin
    Exit;
  end;
  try
    case AEvent.Trigger of
      ntDesignSelect:
        begin
          FSession.Select(ANode.DesignID);
          FCanvasView.Select(FSession.SelectedID);
          RequestRefresh;
        end;
      ntDesignValue:
        begin
          FSession.SetCanvasValue(ANode);
          FState.Status := 'Design updated / Pascal generated';
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
begin

  if FPainting then
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

      if ANode.Prop('add-kind') <> '' then
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
                    FSession.Undo;
                  end;
                ncRedo:
                  begin
                    FSession.Redo;
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
