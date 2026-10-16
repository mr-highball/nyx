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
{ Shared editor behavior compiled inside the two target adapters. Only host
  allocation, pointer mapping and mounting differ; diagram meaning, revision/
  draft guards, prepared publication and ordinary Nyx chrome remain shared. }

type
  { Copied mapping from a graph vertex to one private canvas content lifetime.
    Values retain no interfaces or trees. Stable names survive geometry/header
    changes; a removed and recreated vertex receives a fresh controlled ID. }
  TDiagramContentBinding = record
    Vertex: TNyxDiagramVertex;
    Item: TNyxCanvasItemRef;
    Control: TNyxControlRef;
  end;

  { Independently built mapping vector adopted alongside graph/projection data.
    Records are immutable after installation and contain no interface fields. }
  TDiagramContentBindings = array of TDiagramContentBinding;

  { Forward owner for revocable borrowed callbacks. The mounted interface, never
    a component-tree backreference, provides the active lifetime lease. }
  TEditorView = class;

  { Private PostUI-only coalescer borrows its view. It owns no scheduler/view
    cycle; Disconnect revokes Owner before target retirement. Execute is admitted
    only through the owning UI queue and acquires a live interface before work. }
  TDiagramFrameWork = class(TInterfacedObject, INyxWork)
  public
    Owner: TEditorView;
    procedure Execute(const AExecution: INyxExecution);
  end;

  { Single-use UI reply borrows its editor through a revocable pointer. Its
    independent scheduler refuses worker entry before that pointer is read.
    The queue retains this request, not the editor, across compiler turns. }
  TEditorSaveRequest = class(TInterfacedObject, INyxDiagramSaveRequest)
  public
    Owner: TEditorView;
    Scheduler: INyxScheduler;
    Value: TNyxDiagramSaveSnapshot;
    PendingValue: Boolean;
    function GetSnapshot: TNyxDiagramSaveSnapshot;
    function GetPending: Boolean;
    function Accepted(ADocument: TNyxDocument): Boolean;
    function Refused(const AMessage: TNyxText): Boolean;
  end;

  { Drawing provider owns only an already admitted immutable display list.
    Publication replaces that value without invoking a platform or callback.
    Neither this provider nor the scene's Drawing retains its editor/view. }
  TDiagramProjection = class(TInterfacedObject, INyxDrawingProvider)
  public
    Plan: INyxDrawingPlan;
    function Build(const AFrame: TNyxDrawingFrame): INyxDrawingPlan;
  end;

  { Event router owns this revocable bridge; Owner is borrowed. Invoke acquires
    the mounted interface before work, so a later callback may disconnect safely.
    Disconnect clears Owner before cancelling subscriptions/retiring controls.
    Its independent scheduler refuses worker entry before reading that pointer. }
  TDiagramEditorCallback = class(TNyxEventCallback)
  public
    Owner: TEditorView;
    Scheduler: INyxScheduler;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { Canvas notification borrows its editor through one revocable scope. It
    binds a mounted picture's event router and queues stale gesture/zoom work;
    it never changes camera/layout synchronously during notification. }
  TDiagramEditorObserver = class(TInterfacedObject, INyxCanvasPresentationObserver)
  public
    Owner: TEditorView;
    procedure Presented(ARevision: Integer);
  end;

  { Single-use graph/preview participant in the existing canvas prepared group.
    Preview installation never advances accepted graph revision. The
    calling view lease outlives this borrowed Owner. Only normalized private
    diagram/drawing/source values install, with no creator callbacks or allocation.
    Source replacement owns its candidate/former documents until adoption and
    retirement. Final source inspection retains a revocable host capability,
    never a session pointer; source and graph join the same publication.
    Scene geometry and view camera install before any participant notifies. }
  TDiagramEditorCommit = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  public
    Owner: TEditorView;
    Candidate: INyxDiagram;
    Projection: INyxDrawingPlan;
    Selection: TNyxDiagramVertexRef;
    SelectedEdge: TNyxDiagramEdgeRef;
    Expected: Integer;
    Installed: Boolean;
    Preview: Boolean;
    { Owned source copies transfer only during the callback-free Install. }
    NewDocument: TNyxDocument;
    FormerDocument: TNyxDocument;
    SourceChange: Boolean;
    SourceInspection: INyxDiagramSourceInspection;
    SourceSnapshot: TNyxDiagramSourceSnapshot;
    PreviousTarget: TNyxDiagramSaveTarget;
    SaveHost: INyxDiagramSaveHost;
    ExpectedProperties: TNyxText;
    Zoom: Double;
    ContentBindings: TDiagramContentBindings;
    NextContentID: Integer;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { UI-owned ordinary Nyx composition and existing canvas attachment. Each mount
    normalizes its own initial graph, store and runtime chrome; hosts/stores are
    borrowed when supplied. The private scene owns the Drawing underlay and
    independently materialized content; its named provider owns no view.
    All retained callbacks are explicitly revoked first.
    Runtime exports grant no document/source/history or filesystem authority. }
  TEditorView = class(TInterfacedObject, INyxDiagramEditorView, IDiagramTargetView,
    INyxDiagramContentView, INyxDiagramSourceView, INyxDiagramPortsView)
  private
    FEditorID: TNyxText;
    FPortToolsExpanded: Boolean;
    FProjectionPorts: Boolean;
    FPortSelection: TNyxDiagramPortRef;
    FPortVertex: TNyxDiagramVertexRef;
    FConnectionStartPort: TNyxDiagramPortRef;
    FPortFieldBaseline: TNyxText;
    FOptions: TNyxDiagramEditorOptions;
    FRenderer: TDiagramRenderer;
    FScheduler: INyxScheduler;
    FDocument: TNyxDocument;
    FState: TNyxState;
    FOwnState: Boolean;
    FContentMode: TNyxDiagramContentMode;
    FCanvas: IDiagramCanvasView;
    FWorkspace: INyxCanvasWorkspace;
    FScene: INyxCanvasScene;
    FPicture: INyxDrawing;
    FProvider: TDiagramProjection;
    FProviderLease: INyxDrawingProvider;
    FProviderName: TNyxDrawingProviderRef;
    FItem: TNyxCanvasItemRef;
    FLayer: TNyxCanvasLayerRef;
    FContentLayer: TNyxCanvasLayerRef;
    FContentBindings: TDiagramContentBindings;
    FNextContentID: Integer;
    FContentToolsExpanded: Boolean;
    FDiagram: INyxDiagram;
    FSelection: TNyxDiagramVertexRef;
    FSelectedEdge: TNyxDiagramEdgeRef;
    FConnectionStart: TNyxDiagramVertexRef;
    FConnectionRouting: TNyxDiagramRouting;
    FDrawToolsExpanded: Boolean;
    FReconnectingEdge: TNyxDiagramEdgeRef;
    FReconnectEnd: TNyxDiagramConnectionEnd;
    FPreviewDiagram: INyxDiagram;
    FPointerActive: Boolean;
    FCommittingPointer: Boolean;
    FPointerAbortRequested: Boolean;
    FPointerID: Integer;
    FPointerRevision: Integer;
    FPointerEventRevision: Integer;
    FPointerCamera: TNyxCanvasCamera;
    FPointerProperties: TNyxText;
    FPointerMinimum: TNyxCanvasBounds;
    FPointerChange: TNyxDiagramManipulation;
    FPointerEventsLease: INyxEvents;
    FPointerSample: TNyxCanvasPoint;
    FPointerSamplePending: Boolean;
    FPointerBypassGrid: Boolean;
    FFrameWork: TDiagramFrameWork;
    FFrameLease: INyxWork;
    FFrameQueued: Boolean;
    FProjectionZoom: Double;
    FDisconnecting: Boolean;
    FRevision: Integer;
    FConnected: Boolean;
    FPublishing: Boolean;
    FSaveTarget: TNyxDiagramSaveTarget;
    FSaveHost: INyxDiagramSaveHost;
    FSaveRequest: INyxDiagramSaveRequest;
    FSaveRequestOwner: TEditorSaveRequest;
    FToolsExpanded: Boolean;
    FFormBaseline: TNyxText;
    FLastError: TNyxText;
    FCallbacks: array of TDiagramEditorCallback;
    FCallbackLeases: array of INyxEventCallback;
    FSubscriptions: array of INyxEventSubscription;
    FObserver: TDiagramEditorObserver;
    FObserverLease: INyxCanvasPresentationObserver;
    FObservation: INyxCancellationScope;
    FPointerEvents: INyxEvents;
    procedure RequireConnected;
    procedure RequireWritable;
    function Part(APart: TNyxDiagramEditorPart): TNyxNode;
    function Field(APart: TNyxDiagramEditorPart): INyxTextInput;
    function PropertyBuffer: TNyxText;
    function PortFieldBuffer: TNyxText;
    function GetPortToolsExpanded: Boolean;
    function GetConnectionStartPort: TNyxDiagramPortRef;
    procedure RefreshPortFields;
    procedure ChoosePort;
    function ReadPortFields: TNyxDiagramPort;
    function SelectedPortDirection: TNyxDiagramAttachment;
    procedure RefreshProperties;
    procedure SyncPicture;
    procedure RefreshContentFields;
    procedure RefreshContentChoices;
    procedure RefreshContentHelp;
    function SelectedContentKind: TNyxDiagramContentKind;
    function ReadContentFields: TNyxDiagramContent;
    procedure BindPictureEvents;
    procedure Feedback(const AMessage: TNyxText);
    procedure SetupRenderer(ARenderer: TDiagramRenderer);
    procedure Subscribe(const AEvents: INyxEvents; const AID: TNyxText;
      ATrigger: TNyxTrigger);
    procedure Event(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
    procedure AdvanceSelection(ADelta: Integer);
    procedure AdvanceEdgeSelection(ADelta: Integer);
    procedure RefreshDraw;
    procedure AddShapeAtCenter(AShape: TNyxDiagramShape);
    function FreshVertexReference(AShape: TNyxDiagramShape): TNyxDiagramVertexRef;
    function FreshEdgeReference: TNyxDiagramEdgeRef;
    procedure Publish(const ADiagram: INyxDiagram; AExpectedRevision: Integer);
    procedure PublishSelection(const ADiagram: INyxDiagram; AExpectedRevision: Integer;
      const AVertex: TNyxDiagramVertexRef; const AEdge: TNyxDiagramEdgeRef;
      APreview: Boolean = False);
    function Projection(const ADiagram: INyxDiagram; const AVertex: TNyxDiagramVertexRef;
      const AEdge: TNyxDiagramEdgeRef): INyxDrawingPlan;
    function BuildContentPlan(const ADiagram: INyxDiagram; const AProjection: INyxDrawingPlan;
      out ABindings: TDiagramContentBindings; out ANextContentID: Integer;
      ASource: TNyxDocument = nil): INyxCanvasLayoutPlan;
    procedure PublishSourceSelection(const ADiagram: INyxDiagram; AExpectedRevision: Integer;
      const AVertex: TNyxDiagramVertexRef; const AEdge: TNyxDiagramEdgeRef;
      APreview: Boolean; ASource: TNyxDocument; const AInspection: INyxDiagramSourceInspection;
      const ASnapshot: TNyxDiagramSourceSnapshot);
    procedure QueueFrame;
    procedure RefreshFrame;
    function PointerFrameCurrent: Boolean;
    procedure ReleaseCapturedPointer;
    procedure RetirePointer;
    procedure HandlePointer(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
    function BeginPointer(const AEvent: TNyxEventInfo; const AExecution: INyxExecution): Boolean;
    function HasConnectionIntent: Boolean;
    function PointerWorld(const AEvent: TNyxEventInfo): TNyxCanvasPoint;
    procedure CompleteSave(ARequest: TEditorSaveRequest; ADocument: TNyxDocument;
      const AMessage: TNyxText);
  public
    constructor Create(const AEditor: INyxDiagramEditor; ADocument: TNyxDocument;
      AHost: TDiagramHost; AState: TNyxState; ATheme: TNyxTheme;
      AMode: TNyxDiagramContentMode; const AContext: INyxApplicationContext;
      const AResolver: INyxLogicResolver; const AResources: INyxApplicationResources);
    destructor Destroy; override;
    function GetRenderer: TDiagramRenderer;
    function ContentItem(const AVertex: TNyxDiagramVertexRef): TNyxCanvasItemRef;
    function ContentControl(const AVertex: TNyxDiagramVertexRef): TNyxControlRef;
    procedure SetContent(const AVertex: TNyxDiagramVertexRef;
      const AContent: TNyxDiagramContent; AExpectedRevision: Integer);
    procedure ShowContentTools(AExpanded: Boolean);
    function GetContentToolsExpanded: Boolean;
    function GetConnected: Boolean;
    function GetDiagram: INyxDiagram;
    function GetRevision: Integer;
    function GetSelection: TNyxDiagramVertexRef;
    function GetSelectedEdge: TNyxDiagramEdgeRef;
    function GetConnectionStart: TNyxDiagramVertexRef;
    function GetDrawToolsExpanded: Boolean;
    function GetDisplayDiagram: INyxDiagram;
    function GetManipulating: Boolean;
    function GetReconnectingEdge: TNyxDiagramEdgeRef;
    procedure CancelManipulation;
    function GetCanvas: INyxCanvasWorkspaceView;
    function GetHasPendingProperties: Boolean;
    function GetToolsExpanded: Boolean;
    function GetLastError: TNyxText;
    procedure SavingTo(const ATarget: TNyxDiagramSaveTarget; const AHost: INyxDiagramSaveHost);
    procedure Save;
    procedure ReloadSaved(ADiscardLocal: Boolean);
    procedure ShowPortTools(AExpanded: Boolean);
    procedure SetPort(const AVertex: TNyxDiagramVertexRef;
      const APort: TNyxDiagramPort; AExpectedRevision: Integer);
    procedure RemovePort(const AVertex: TNyxDiagramVertexRef;
      const APort: TNyxDiagramPortRef; APolicy: TNyxDiagramRemoval; AExpectedRevision: Integer);
    procedure BeginPortConnection(const AVertex: TNyxDiagramVertexRef;
      const APort: TNyxDiagramPortRef; ARouting: TNyxDiagramRouting);
    procedure CompletePortConnection(const AVertex: TNyxDiagramVertexRef;
      const APort: TNyxDiagramPortRef; AExpectedRevision: Integer);
    procedure ReplaceSource(ADocument: TNyxDocument; const ADiagram: INyxDiagram;
      AExpectedRevision: Integer; ADiscardProperties: Boolean = False);
    function GetSavePending: Boolean;
    function GetHasUnsavedChanges: Boolean;
    procedure SelectVertex(const AReference: TNyxDiagramVertexRef;
      ADiscardProperties: Boolean);
    procedure SelectAt(const AWorldPoint: TNyxCanvasPoint; ADiscardProperties: Boolean);
    procedure SelectEdge(const AReference: TNyxDiagramEdgeRef; ADiscardProperties: Boolean);
    procedure AddVertex(const AVertex: TNyxDiagramVertex; AExpectedRevision: Integer);
    procedure SetEdge(const AEdge: TNyxDiagramEdge; AExpectedRevision: Integer);
    procedure RemoveVertex(const AReference: TNyxDiagramVertexRef;
      ARemoval: TNyxDiagramRemoval; AExpectedRevision: Integer);
    procedure RemoveEdge(const AReference: TNyxDiagramEdgeRef; AExpectedRevision: Integer);
    procedure BeginConnection(const AStart: TNyxDiagramVertexRef; ARouting: TNyxDiagramRouting);
    procedure CompleteConnection(const AFinish: TNyxDiagramVertexRef; AExpectedRevision: Integer);
    procedure CancelConnection;
    procedure BeginReconnect(const AEdge: TNyxDiagramEdgeRef; AEnd: TNyxDiagramConnectionEnd);
    procedure ReconnectEdge(const AEdge: TNyxDiagramEdgeRef; AEnd: TNyxDiagramConnectionEnd;
      const AEndpoint: TNyxDiagramEndpoint; AExpectedRevision: Integer);
    procedure ShowDrawTools(AExpanded: Boolean);
    procedure ReplaceDiagram(const ADiagram: INyxDiagram; AExpectedRevision: Integer;
      ADiscardProperties: Boolean);
    procedure SetGeometry(const AReference: TNyxDiagramVertexRef;
      const ABounds: TNyxCanvasBounds; AExpectedRevision: Integer);
    procedure NudgeSelection(AX, AY: Double; AExpectedRevision: Integer);
    procedure ApplyProperties;
    procedure RestoreProperties;
    procedure ShowTools(AExpanded: Boolean);
    procedure RevealSelection;
    procedure Disconnect;
  end;

{ Save replies run on the owning UI scheduler even after their view retires. }
function TEditorSaveRequest.GetSnapshot: TNyxDiagramSaveSnapshot;
begin
  Scheduler.RequireUI;
  Result := Value;
end;

function TEditorSaveRequest.GetPending: Boolean;
begin
  Scheduler.RequireUI;
  Result := PendingValue and (Owner <> nil);
end;

function TEditorSaveRequest.Accepted(ADocument: TNyxDocument): Boolean;
var
  LLease: INyxDiagramSaveRequest;
  LView: INyxDiagramEditorView;
  LOwner: TEditorView;
begin
  Scheduler.RequireUI;
  LLease := Self;
  Result := False;

  if not GetPending then
  begin
    Exit;
  end;

  if not Value.Matches(ADocument) then
  begin
    raise ENyxDiagram.Create('Published diagram does not match the submitted snapshot');
  end;
  LOwner := Owner;
  LView := LOwner;
  PendingValue := False;
  Owner := nil;
  LOwner.CompleteSave(Self, ADocument, 'Diagram saved');
  Result := True;
end;

function TEditorSaveRequest.Refused(const AMessage: TNyxText): Boolean;
var
  LLease: INyxDiagramSaveRequest;
  LView: INyxDiagramEditorView;
  LOwner: TEditorView;
begin
  Scheduler.RequireUI;
  LLease := Self;
  Result := False;

  if not GetPending then
  begin
    Exit;
  end;
  LOwner := Owner;
  LView := LOwner;
  PendingValue := False;
  Owner := nil;
  LOwner.CompleteSave(Self, nil, AMessage);
  Result := True;
end;

procedure TEditorView.SavingTo(const ATarget: TNyxDiagramSaveTarget;
  const AHost: INyxDiagramSaveHost);
begin
  RequireConnected;

  if FPublishing then
  begin
    raise ENyxDiagram.Create('The diagram is publishing its current model');
  end;

  if GetSavePending then
  begin
    raise ENyxDiagram.Create('Wait for this diagram save before changing its destination');
  end;

  if (AHost <> nil) and not ATarget.Defined then
  begin
    raise ENyxDiagram.Create('A save host requires an explicit diagram destination');
  end;
  FSaveTarget := ATarget;
  FSaveHost := AHost;
  RetainNyxControl(Part(ndepSave)).Configure.Visible(AHost <> nil).Done;
  RetainNyxControl(Part(ndepReloadSaved)).Configure.Visible(AHost <> nil).Done;
  FRenderer.Sync;
end;

function TEditorView.GetSavePending: Boolean;
begin
  RequireConnected;
  Result := (FSaveRequest <> nil) and FSaveRequest.Pending;
end;

function TEditorView.GetHasUnsavedChanges: Boolean;
begin
  RequireConnected;
  Result := FPointerActive or (FPreviewDiagram <> nil) or GetHasPendingProperties or (FSaveTarget.Defined and
    (FDiagram.ToData.ToJSON <> FSaveTarget.Before.ToJSON));
end;

procedure TEditorView.Save;
var
  LRequest: TEditorSaveRequest;
  LLease: INyxDiagramSaveRequest;
  LHost: INyxDiagramSaveHost;
begin
  RequireWritable;

  if GetHasPendingProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished properties before saving the diagram');
  end;

  if GetSavePending then
  begin
    raise ENyxDiagram.Create('This diagram save is already pending');
  end;
  LHost := FSaveHost;

  if LHost = nil then
  begin
    raise ENyxDiagram.Create('This editor has no owning diagram save host');
  end;

  if not GetHasUnsavedChanges then
  begin
    Feedback('No local diagram changes');
    Exit;
  end;
  LRequest := TEditorSaveRequest.Create;
  LLease := LRequest;
  LRequest.Value := CaptureNyxDiagramSave(FSaveTarget, FDiagram, FRevision);
  LRequest.Scheduler := FScheduler;
  LRequest.Owner := Self;
  LRequest.PendingValue := True;
  FSaveRequestOwner := LRequest;
  FSaveRequest := LLease;
  Feedback('Saving diagram');
  try
    LHost.Submit(LLease);
  except
    on E: Exception do
    begin
      LLease.Refused(E.Message);
      raise;
    end;
  end;
end;

procedure TEditorView.ReloadSaved(ADiscardLocal: Boolean);
var
  LInspection: INyxDiagramSaveInspection;
  LSourceInspection: INyxDiagramSourceInspection;
  LSource: TNyxDiagramSourceSnapshot;
  LDocument: TNyxDocument;
  LHost: INyxDiagramSaveHost;
  LTarget: TNyxDiagramSaveTarget;
  LPrevious: TNyxDiagramSaveTarget;
  LRevision: Integer;
  LBuffer: TNyxText;
  LSelection: TNyxText;
begin
  RequireWritable;

  if GetSavePending then
  begin
    raise ENyxDiagram.Create('Wait for the pending diagram save before reloading');
  end;

  if GetHasUnsavedChanges and not ADiscardLocal then
  begin
    raise ENyxDiagram.Create('Confirm discarding local changes before reloading');
  end;
  LHost := FSaveHost;

  Supports(LHost, INyxDiagramSourceInspection, LSourceInspection);

  if not FSaveTarget.Defined or
    ((LSourceInspection = nil) and
      not Supports(LHost, INyxDiagramSaveInspection, LInspection)) then
  begin
    raise ENyxDiagram.Create('The owning host does not expose saved diagram inspection');
  end;
  LPrevious := FSaveTarget;
  LRevision := FRevision;
  LBuffer := PropertyBuffer;
  LSelection := FSelection.Name;
  LSource := Default(TNyxDiagramSourceSnapshot);

  if LSourceInspection <> nil then
  begin
    LSource := LSourceInspection.CurrentSource(LPrevious.Reference);
    LTarget := LSource.Target;
  end
  else
  begin
    LTarget := LInspection.Current(LPrevious.Reference);
  end;
  RequireConnected;

  if GetSavePending or (FRevision <> LRevision) or (PropertyBuffer <> LBuffer) or
    (FSelection.Name <> LSelection) or (FSaveHost <> LHost) or
    (FSaveTarget.Reference.Name <> LPrevious.Reference.Name) or
    (FSaveTarget.Before.ToJSON <> LPrevious.Before.ToJSON) then
  begin
    raise ENyxDiagram.Create('The local diagram changed during saved inspection; its proposal is retained');
  end;

  if not LTarget.Defined or (LTarget.Reference.Name <> LPrevious.Reference.Name) or
    (LTarget.Before.Kind = ndNull) then
  begin
    raise ENyxDiagram.Create('The named diagram has no saved definition to reload');
  end;
  if LSource.Defined then
  begin
    LDocument := LSource.OpenDocument;
    try
      PublishSourceSelection(NyxDiagramFromData(LTarget.Before), LRevision, FSelection,
        FSelectedEdge, False, LDocument, LSourceInspection, LSource);
    finally
      LDocument.Free;
    end;
  end
  else
  begin
    Publish(NyxDiagramFromData(LTarget.Before), LRevision);
  end;
  RequireConnected;
  FSaveTarget := LTarget;
  (RetainNyxControl(Part(ndepConfirmReload)) as INyxBooleanInput).Checked := False;
  RetainNyxControl(Part(ndepConfirmReload)).Configure.Visible(False).Done;
  Feedback('Saved diagram reloaded');
end;

procedure TEditorView.CompleteSave(ARequest: TEditorSaveRequest;
  ADocument: TNyxDocument; const AMessage: TNyxText);
begin
  RequireConnected;

  if FSaveRequestOwner <> ARequest then
  begin
    Exit;
  end;

  if ADocument <> nil then
  begin
    FSaveTarget := AcceptedNyxDiagramSaveTarget(ARequest.Value, ADocument);
  end;
  FSaveRequestOwner := nil;
  FSaveRequest := nil;
  Feedback(AMessage);
end;

function TDiagramProjection.Build(const AFrame: TNyxDrawingFrame): INyxDrawingPlan;
begin
  Result := Plan;
end;

procedure TDiagramEditorCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LOwner: TEditorView;
  LLease: INyxDiagramEditorView;
begin
  Scheduler.RequireUI;
  LOwner := Owner;

  if LOwner = nil then
  begin
    Exit;
  end;
  LLease := LOwner;
  try
    LOwner.Event(AEvent, AExecution);
  except
    on E: Exception do
    begin

      if LOwner.GetConnected then
      begin
        LOwner.Feedback(E.Message);
      end;
      raise;
    end;
  end;
end;

procedure TDiagramEditorObserver.Presented(ARevision: Integer);
var
  LOwner: TEditorView;
  LLease: INyxDiagramEditorView;
begin
  LOwner := Owner;

  if LOwner <> nil then
  begin
    LLease := LOwner;
    LOwner.BindPictureEvents;

    if not LOwner.FPublishing and
      ((LOwner.FProjectionZoom <> LOwner.FCanvas.Camera.Zoom) or
        ((LOwner.FPreviewDiagram <> nil) and not LOwner.FPointerActive) or
        (LOwner.FPointerActive and not LOwner.PointerFrameCurrent)) then
    begin
      LOwner.QueueFrame;
    end;
  end;
end;

procedure TDiagramEditorCommit.CheckReady;
begin

  if (Owner = nil) or not Owner.GetConnected or (Owner.FRevision <> Expected) or Installed then
  begin
    raise ENyxDiagram.Create('Diagram publication is stale or retired');
  end;

  if SourceChange and (Owner.PropertyBuffer <> ExpectedProperties) then
  begin
    raise ENyxDiagram.Create('Diagram fields changed while source was staged');
  end;

  if (SourceInspection <> nil) and ((Owner.FSaveHost <> SaveHost) or
    (Owner.FSaveTarget.Reference.Name <> PreviousTarget.Reference.Name) or
    (Owner.FSaveTarget.Before.ToJSON <> PreviousTarget.Before.ToJSON)) then
  begin
    raise ENyxDiagram.Create('Diagram source owner changed during staging');
  end;
end;

procedure TDiagramEditorCommit.Validate;
begin
  CheckReady;
  Owner.RequireConnected;

  if (SourceInspection <> nil) and not SourceInspection.IsCurrent(SourceSnapshot) then
  begin
    raise ENyxDiagram.Create('Accepted diagram source changed during staging');
  end;

  if (Candidate = nil) or (Projection = nil) or (Expected = High(Integer)) then
  begin
    raise ENyxDiagram.Create('Diagram publication lacks an admitted candidate or revision capacity');
  end;
end;

procedure TDiagramEditorCommit.Install;
begin

  if NewDocument <> nil then
  begin
    FormerDocument := Owner.FDocument;
    Owner.FDocument := NewDocument;
    NewDocument := nil;
  end;

  if SourceInspection <> nil then
  begin
    Owner.FSaveTarget := SourceSnapshot.Target;
  end;

  if Preview then
  begin

    if Owner.FPointerActive then
    begin
      Owner.FPreviewDiagram := Candidate;
    end
    else
    begin
      Owner.FPreviewDiagram := nil;
    end;
  end
  else
  begin
    Owner.FDiagram := Candidate;
    Owner.FPreviewDiagram := nil;
    Owner.FSelection := Selection;
    Owner.FSelectedEdge := SelectedEdge;
    Owner.FConnectionStart := Default(TNyxDiagramVertexRef);
    Owner.FConnectionStartPort := Default(TNyxDiagramPortRef);
    Owner.FReconnectingEdge := Default(TNyxDiagramEdgeRef);
    Inc(Owner.FRevision);
  end;
  Owner.FProvider.Plan := Projection;
  Owner.FProjectionZoom := Zoom;
  Owner.FProjectionPorts := Owner.FPortToolsExpanded;
  Owner.FContentBindings := ContentBindings;
  Owner.FNextContentID := NextContentID;
  Installed := True;
end;

procedure TDiagramEditorCommit.Notify;
begin
  { Physical reconciliation is the existing canvas owner's notification phase. }
end;

procedure TDiagramEditorCommit.Retire;
begin
  Owner := nil;
  FreeAndNil(NewDocument);
  FreeAndNil(FormerDocument);
  SourceInspection := nil;
  SaveHost := nil;
  Candidate := nil;
  Projection := nil;
  ContentBindings := nil;
end;

constructor TEditorView.Create(const AEditor: INyxDiagramEditor;
  ADocument: TNyxDocument; AHost: TDiagramHost; AState: TNyxState; ATheme: TNyxTheme;
  AMode: TNyxDiagramContentMode; const AContext: INyxApplicationContext;
  const AResolver: INyxLogicResolver; const AResources: INyxApplicationResources);
var
  LRoot: TNyxNode;
  LHost: TDiagramHost;
  LPart: TNyxDiagramEditorPart;
  LObservation: INyxCanvasPresentationObservation;
  LToolbar: INyxControl;
  LSurface: INyxControl;
  LPolicy: TNyxInteractionPolicy;
  LInitialContent: INyxCanvasLayoutPlan;
  LInitialBindings: TDiagramContentBindings;
  LNextContentID: Integer;
  LContextState: TNyxState;
begin
  inherited Create;
  { Refuse worker entry before borrowing caller trees, host or context. }
  NewNyxScheduler.RequireUI;

  if (Ord(AMode) < Ord(Low(TNyxDiagramContentMode))) or
    (Ord(AMode) > Ord(High(TNyxDiagramContentMode))) then
  begin
    raise ENyxDiagram.Create('Unknown diagram content execution mode');
  end;
  FContentMode := AMode;
  LContextState := AState;

  if AContext <> nil then
  begin
    AContext.Scheduler.RequireUI;

    if not AContext.Active then
    begin
      raise ENyxApplicationContext.Create('Diagram content requires active application authority');
    end;
    LContextState := AContext.State;

    if (LContextState = nil) or ((AState <> nil) and (AState <> LContextState)) then
    begin
      raise ENyxApplicationContext.Create('Diagram controls and behavior must share the exact runtime store');
    end;
  end
  else

    if AResolver <> nil then
    begin
      raise ENyxApplicationContext.Create('A diagram Logic resolver requires explicit application context');
    end;

  if (AEditor = nil) or (ADocument = nil) or (AHost = nil) then
  begin
    raise ENyxDiagram.Create('Diagram mount requires its editor, document and borrowed host');
  end;
  FEditorID := AEditor.ID;
  FOptions := AEditor.Options;
  FDiagram := NyxDiagramFromData(AEditor.InitialDiagram.ToData);
  ADocument.ValidateDiagramContent(FDiagram);

  if FDiagram.VertexCount > 0 then
  begin
    FSelection := FDiagram.Vertex(0).Reference;
  end;
  FProvider := TDiagramProjection.Create;
  FProviderLease := FProvider;
  FProvider.Plan := Projection(FDiagram, FSelection, Default(TNyxDiagramEdgeRef));
  FProjectionZoom := FOptions.Camera.Zoom;
  FProviderName := NyxDrawingProvider(AEditor.ID + '-diagram-provider');
  FItem := NyxCanvasItem('Diagram picture');
  FLayer := NyxCanvasLayer('Diagram');
  FContentLayer := NyxCanvasLayer('Diagram content');
  FPicture := NewNyxDrawing(AEditor.ID + '-diagram-picture')
    .WithText('Editable diagram').WithPlan(FProvider.Plan).WithProvider(FProviderName);
  FPicture.Configure.HeightSizing(nsFill).MinimumHeight(0).TouchBehavior(ntbNone)
    .Hint('Drag a shape to move; drag a boundary grip to resize. Escape cancels. Alt bypasses grid snapping.').Done;
  FScene := NewNyxCanvasScene;
  FScene.AddLayer(FLayer).AddControl(FItem,
    NyxCanvasPlacement(FLayer, FProvider.Plan.ViewBox), FPicture);
  FScene.AddLayer(FContentLayer);
  LInitialContent := BuildContentPlan(FDiagram, FProvider.Plan, LInitialBindings, LNextContentID, ADocument);
  LInitialContent.Commit;
  FContentBindings := LInitialBindings;
  FNextContentID := LNextContentID;
  LInitialContent := nil;
  FWorkspace := NewNyxCanvasWorkspace(AEditor.ID + '-diagram-viewport', FScene)
    .WithCamera(FOptions.Camera).WithGrid(FOptions.Grid);
  FWorkspace.Configure.MinimumHeight(0).Done;
  LToolbar := RetainNyxControl(FWorkspace.Node.Find(
    NyxCanvasWorkspacePart(FWorkspace.ID, ncwpToolbar).ID));
  LToolbar.Configure.Visible(False).Done;
  LSurface := RetainNyxControl(FWorkspace.Node.Find(
    NyxCanvasWorkspacePart(FWorkspace.ID, ncwpSurface).ID));
  LSurface.Configure.MinimumHeight(64).Done;
  FDocument := NewNyxCanvasRenderContext(ADocument, AEditor.Node, LRoot);
  FState := LContextState;
  FOwnState := (AState = nil) and (AContext = nil);

  if FOwnState then
  begin
    FState := FDocument.State.Clone;
  end;
  FRenderer := TDiagramRenderer.Create(ATheme);
  FScheduler := FRenderer.Events.Scheduler;
  FScheduler.RequireUI;
  FFrameWork := TDiagramFrameWork.Create;
  FFrameLease := FFrameWork;
  FFrameWork.Owner := Self;
  FRenderer.Render(FDocument, LRoot, AHost, False, FState);
  {$ifdef PAS2JS}
  LHost := FRenderer.ElementFor(NyxDiagramEditorPart(AEditor.ID, ndepCanvasHost).ID);
  FCanvas := MountNyxBrowserCanvasWorkspace(FWorkspace, FDocument, LHost, FState,
    ATheme, SetupRenderer, AMode = ndcmDesignPreview, AContext, AResolver, AResources);
  {$else}
  LHost := TWinControl(FRenderer.ControlFor(NyxDiagramEditorPart(AEditor.ID, ndepCanvasHost).ID));
  FCanvas := MountNyxLCLCanvasWorkspace(FWorkspace, FDocument, LHost, FState,
    ATheme, SetupRenderer, AMode = ndcmDesignPreview, AContext, AResolver, AResources);
  {$endif}
  FConnected := True;
  LPolicy := NyxInteractionPolicy(FRenderer.Root);
  FCanvas.Constrain(LPolicy.Enabled, LPolicy.ReadOnly);
  FObserver := TDiagramEditorObserver.Create;
  FObserverLease := FObserver;
  FObserver.Owner := Self;

  if not Supports(FCanvas, INyxCanvasPresentationObservation, LObservation) then
  begin
    raise ENyxDiagram.Create('Diagram canvas lacks presentation observation');
  end;
  FObservation := LObservation.ObservePresentation(FObserverLease);
  RestoreProperties;
  ShowTools(FOptions.InitiallyExpanded);

  if FSelection.Defined then
  begin
    RevealSelection;
  end;
  for LPart := ndepPrevious to ndepToggleTools do
  begin
    Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, LPart).ID, ntClick);
  end;
  for LPart := ndepLeft to ndepDown do
  begin
    Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, LPart).ID, ntClick);
  end;
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepApply).ID, ntClick);
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepRestore).ID, ntClick);
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepSave).ID, ntClick);
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepReloadSaved).ID, ntClick);
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepSelection).ID, ntClick);
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepSelection).ID, ntBeforeKeyDown);
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepSelection).ID, ntAfterExit);
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepToggleContent).ID, ntClick);
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepTogglePorts).ID, ntClick);
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepPortChoice).ID, ntChange);
  for LPart := ndepApplyPort to ndepCancelPortConnection do
  begin

    if LPart <> ndepPortIncidentEdges then
    begin
      Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, LPart).ID, ntClick);
    end;
  end;
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepApplyContent).ID, ntClick);
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepRestoreContent).ID, ntClick);
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepContentKind).ID, ntChange);
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepContentReference).ID, ntChange);
  Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, ndepContentLocalized).ID, ntChange);
  for LPart := ndepToggleDraw to ndepReconnectFinish do
  begin

    if LPart in [ndepDrawTools, ndepConnectionPrompt, ndepIncidentEdges] then
    begin
      Continue;
    end;
    Subscribe(FRenderer.Events, NyxDiagramEditorPart(AEditor.ID, LPart).ID, ntClick);
  end;
  BindPictureEvents;

  if FContentMode = ndcmDesignPreview then
  begin
    Feedback('Component actions are paused');
  end;
end;

destructor TEditorView.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TEditorView.SetupRenderer(ARenderer: TDiagramRenderer);
begin
  ARenderer.RegisterDrawingProvider(FProviderName, FProviderLease);
end;

procedure TEditorView.BindPictureEvents;
var
  LEvents: INyxEvents;
begin

  if not GetConnected or (FCanvas.MountedCount = 0) then
  begin
    Exit;
  end;
  LEvents := FCanvas.EventsFor(FItem);

  if LEvents <> FPointerEvents then
  begin
    Subscribe(LEvents, FPicture.ID, ntPointerDown);
    Subscribe(LEvents, FPicture.ID, ntPointerMove);
    Subscribe(LEvents, FPicture.ID, ntPointerUp);
    Subscribe(LEvents, FPicture.ID, ntPointerCancel);
    Subscribe(LEvents, FPicture.ID, ntPointerCaptureLost);
    Subscribe(LEvents, FPicture.ID, ntBeforeKeyDown);
    FPointerEvents := LEvents;
  end;
end;

procedure TEditorView.Feedback(const AMessage: TNyxText);
var
  LCaption: INyxCaptionControl;
begin
  FScheduler.RequireUI;
  FLastError := AMessage;
  LCaption := RetainNyxControl(Part(ndepStatus)) as INyxCaptionControl;
  LCaption.Text := AMessage;

  if AMessage = '' then
  begin
    LCaption.Text := 'Ready';
  end;

  if FContentMode = ndcmDesignPreview then
  begin
    LCaption.Text := 'Design preview / ' + LCaption.Text;
  end;
  FRenderer.Sync;
end;

procedure TEditorView.RequireConnected;
begin

  if FScheduler <> nil then
  begin
    FScheduler.RequireUI;
  end;

  if not GetConnected then
  begin
    raise ENyxDiagram.Create('Diagram editor is disconnected');
  end;
end;

procedure TEditorView.RequireWritable;
begin
  RequireConnected;

  if FPublishing then
  begin
    raise ENyxDiagram.Create('Diagram publication is already reserved');
  end;

  if FPointerActive or ((FPreviewDiagram <> nil) and not FCommittingPointer) then
  begin
    raise ENyxDiagram.Create('Finish or cancel the active diagram manipulation first');
  end;
end;

function TEditorView.GetConnected: Boolean;
begin

  if FScheduler <> nil then
  begin
    FScheduler.RequireUI;
  end;
  Result := FConnected and (FCanvas <> nil) and FCanvas.Connected;
end;

function TEditorView.GetRenderer: TDiagramRenderer;
begin
  RequireConnected;
  Result := FRenderer;
end;

function TEditorView.GetDiagram: INyxDiagram;
begin
  RequireConnected;
  Result := FDiagram;
end;

function TEditorView.GetRevision: Integer;
begin
  RequireConnected;
  Result := FRevision;
end;

function TEditorView.GetSelection: TNyxDiagramVertexRef;
begin
  RequireConnected;
  Result := FSelection;
end;

function TEditorView.GetCanvas: INyxCanvasWorkspaceView;
begin
  RequireConnected;
  Result := FCanvas;
end;

function TEditorView.Part(APart: TNyxDiagramEditorPart): TNyxNode;
begin
  Result := FRenderer.Root.Find(NyxDiagramEditorPart(FEditorID, APart).ID);

  if Result = nil then
  begin
    raise ENyxDiagram.Create('Diagram editor part is missing');
  end;
end;

function TEditorView.Field(APart: TNyxDiagramEditorPart): INyxTextInput;
var
  LControl: INyxControl;
begin
  LControl := RetainNyxControl(Part(APart));

  if not Supports(LControl, INyxTextInput, Result) then
  begin
    raise ENyxDiagram.Create('Diagram geometry field changed its contract');
  end;
end;

function TEditorView.PortFieldBuffer: TNyxText;
begin
  Result := NyxArray([NyxData(Field(ndepPortName).Value), NyxData(Field(ndepPortX).Value),
    NyxData(Field(ndepPortY).Value), NyxData(Field(ndepPortDirection).Value),
    NyxData(Field(ndepPortCaption).Value)]).ToJSON;
end;

function TEditorView.PropertyBuffer: TNyxText;
begin
  { A structured scalar snapshot prevents NUL or delimiter collisions in
    unfinished input. This private fence is never generated authoring code. }
  Result := NyxArray([NyxData(Field(ndepX).Value), NyxData(Field(ndepY).Value),
    NyxData(Field(ndepWidth).Value), NyxData(Field(ndepHeight).Value),
    NyxData(Field(ndepCaption).Value), NyxData(Field(ndepContentKind).Value),
    NyxData(Field(ndepContentReference).Value), NyxData(Field(ndepContentPadding).Value),
    NyxData(Field(ndepContentHeader).Value), NyxData(Field(ndepContentFit).Value),
    NyxData((RetainNyxControl(Part(ndepContentLocalized)) as INyxBooleanInput).Checked),
    NyxData(Field(ndepContentLocale).Value), NyxData(Field(ndepContentFallback).Value),
    NyxData(Field(ndepPortName).Value), NyxData(Field(ndepPortX).Value),
    NyxData(Field(ndepPortY).Value), NyxData(Field(ndepPortDirection).Value),
    NyxData(Field(ndepPortCaption).Value)]).ToJSON;
end;

function TEditorView.GetHasPendingProperties: Boolean;
begin
  RequireConnected;
  Result := PropertyBuffer <> FFormBaseline;
end;

function TEditorView.GetToolsExpanded: Boolean;
begin
  RequireConnected;
  Result := FToolsExpanded;
end;

function TEditorView.GetLastError: TNyxText;
begin
  RequireConnected;
  Result := FLastError;
end;

procedure TEditorView.RefreshProperties;
var
  LPart: TNyxDiagramEditorPart;
  LVertex: TNyxDiagramVertex;
  LCaption: INyxCaptionControl;
begin

  if FSelection.Defined then
  begin
    LVertex := FDiagram.FindVertex(FSelection);
    Field(ndepX).Value := NyxData(LVertex.Bounds.Left).ToJSON;
    Field(ndepY).Value := NyxData(LVertex.Bounds.Top).ToJSON;
    Field(ndepWidth).Value := NyxData(LVertex.Bounds.Width).ToJSON;
    Field(ndepHeight).Value := NyxData(LVertex.Bounds.Height).ToJSON;
    Field(ndepCaption).Value := LVertex.Caption;
    LCaption := RetainNyxControl(Part(ndepSelection)) as INyxCaptionControl;
    LCaption.Text := 'Selected / ' + FSelection.Name;
  end
  else
  begin
    Field(ndepX).Value := '';
    Field(ndepY).Value := '';
    Field(ndepWidth).Value := '';
    Field(ndepHeight).Value := '';
    Field(ndepCaption).Value := '';
    LCaption := RetainNyxControl(Part(ndepSelection)) as INyxCaptionControl;

    if FSelectedEdge.Defined then
    begin
      LCaption.Text := 'Connection / ' + FSelectedEdge.Name;
    end
    else
    begin
      LCaption.Text := 'No shape selected';
    end;
  end;
  { Connector properties live in Draw. Empty shape fields must not accept UI
    input that has no shape owner; direct typed authoring remains independent. }
  for LPart := ndepX to ndepCaption do
  begin
    RetainNyxControl(Part(LPart)).Configure.Enabled(FSelection.Defined).Done;
  end;
  RefreshContentFields;
  RefreshPortFields;
  FPortFieldBaseline := PortFieldBuffer;
  FFormBaseline := PropertyBuffer;
  RefreshDraw;
  FRenderer.Sync;
end;

procedure TEditorView.RestoreProperties;
begin
  RequireWritable;
  RefreshProperties;
end;

procedure TEditorView.SyncPicture;
begin

  if GetConnected and (FCanvas.MountedCount > 0) then
  begin
    FCanvas.RendererFor(FItem).Sync;
  end;
end;

procedure TEditorView.SelectVertex(const AReference: TNyxDiagramVertexRef;
  ADiscardProperties: Boolean);
var
  LProjection: INyxDrawingPlan;
begin
  RequireWritable;

  if (AReference.Name = FSelection.Name) and not FSelectedEdge.Defined then
  begin
    Exit;
  end;

  if GetHasPendingProperties and not ADiscardProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished geometry before changing selection');
  end;

  if AReference.Defined then
  begin
    FDiagram.FindVertex(AReference);
  end;
  LProjection := Projection(FDiagram, AReference, Default(TNyxDiagramEdgeRef));
  FSelection := AReference;
  FSelectedEdge := Default(TNyxDiagramEdgeRef);
  FProvider.Plan := LProjection;
  SyncPicture;
  RefreshProperties;
end;

procedure TEditorView.SelectAt(const AWorldPoint: TNyxCanvasPoint;
  ADiscardProperties: Boolean);
var
  LVertex: TNyxDiagramVertexRef;
  LEdge: TNyxDiagramEdgeRef;
begin
  RequireWritable;
  LVertex := FDiagram.PickVertex(AWorldPoint);

  if LVertex.Defined then
  begin
    SelectVertex(LVertex, ADiscardProperties);
  end
  else
  begin
    LEdge := FDiagram.PickEdge(AWorldPoint, 6 / FCanvas.Camera.Zoom);
    SelectEdge(LEdge, ADiscardProperties);
  end;
end;

procedure TEditorView.ShowTools(AExpanded: Boolean);
begin
  RequireWritable;
  RetainNyxControl(Part(ndepTools)).Configure.Visible(AExpanded).Done;
  FToolsExpanded := AExpanded;

  if AExpanded then
  begin
    RetainNyxControl(Part(ndepDrawTools)).Configure.Visible(False).Done;
    FDrawToolsExpanded := False;
    RetainNyxControl(Part(ndepContentTools)).Configure.Visible(False).Done;
    FContentToolsExpanded := False;
    RetainNyxControl(Part(ndepPortTools)).Configure.Visible(False).Done;
    FPortToolsExpanded := False;
  end;
  RefreshFrame;
  FRenderer.Sync;
end;

procedure TEditorView.RevealSelection;
var
  LRoute: TNyxDiagramRoute;
begin
  RequireWritable;

  if FSelection.Defined then
  begin
    FCanvas.SetCamera(FCanvas.Camera.AtCenter(FDiagram.FindVertex(FSelection).Bounds.Center));
  end
  else if FSelectedEdge.Defined then
  begin
    LRoute := FDiagram.Route(FSelectedEdge);
    FCanvas.SetCamera(FCanvas.Camera.AtCenter(LRoute.Point(LRoute.Count div 2)));
  end;
end;

procedure TEditorView.AdvanceSelection(ADelta: Integer);
var
  LIndex: Integer;
  LSelected: Integer;
begin
  RequireWritable;

  if FDiagram.VertexCount = 0 then
  begin
    Exit;
  end;
  LSelected := 0;
  for LIndex := 0 to FDiagram.VertexCount - 1 do
  begin

    if FDiagram.Vertex(LIndex).Reference.Name = FSelection.Name then
    begin
      LSelected := LIndex;
      Break;
    end;
  end;
  LSelected := (LSelected + ADelta + FDiagram.VertexCount) mod FDiagram.VertexCount;
  SelectVertex(FDiagram.Vertex(LSelected).Reference, False);
  RevealSelection;
end;

procedure TEditorView.Publish(const ADiagram: INyxDiagram; AExpectedRevision: Integer);
begin
  PublishSelection(ADiagram, AExpectedRevision, FSelection, FSelectedEdge);
end;

procedure TEditorView.PublishSelection(const ADiagram: INyxDiagram; AExpectedRevision: Integer;
  const AVertex: TNyxDiagramVertexRef; const AEdge: TNyxDiagramEdgeRef; APreview: Boolean);
begin
  PublishSourceSelection(ADiagram, AExpectedRevision, AVertex, AEdge, APreview, nil,
    nil, Default(TNyxDiagramSourceSnapshot));
end;

procedure TEditorView.PublishSourceSelection(const ADiagram: INyxDiagram; AExpectedRevision: Integer;
  const AVertex: TNyxDiagramVertexRef; const AEdge: TNyxDiagramEdgeRef; APreview: Boolean;
  ASource: TNyxDocument; const AInspection: INyxDiagramSourceInspection;
  const ASnapshot: TNyxDiagramSourceSnapshot);
var
  LCandidate: INyxDiagram;
  LProjection: INyxDrawingPlan;
  LSelection: TNyxDiagramVertexRef;
  LSelectedEdge: TNyxDiagramEdgeRef;
  LIndex: Integer;
  LFound: Boolean;
  LView: INyxCanvasCoordinatedLayoutView;
  LSourceView: INyxCanvasSourceLayoutView;
  LDocument: TNyxDocument;
  LOwnedDocument: TNyxDocument;
  LPresentation: INyxCanvasLayoutView;
  LPlan: INyxCanvasLayoutPlan;
  LCommit: TDiagramEditorCommit;
  LPublication: INyxPreparedPublication;
  LLease: INyxDiagramEditorView;
  LFrame: TNyxCanvasControlFrame;
  LPropertyBuffer: TNyxText;
  LContentBindings: TDiagramContentBindings;
  LNextContentID: Integer;
begin
  RequireConnected;

  if not APreview then
  begin
    RequireWritable;
  end;

  if FPublishing then
  begin
    raise ENyxDiagram.Create('Diagram publication is already reserved');
  end;

  if (AExpectedRevision <> FRevision) or (FRevision = High(Integer)) then
  begin
    raise ENyxDiagram.Create('Diagram revision is stale or exhausted');
  end;

  if ADiagram = nil then
  begin
    raise ENyxDiagram.Create('Diagram candidate is absent');
  end;
  LLease := Self;
  LPropertyBuffer := PropertyBuffer;
  FPublishing := True;
  LOwnedDocument := nil;
  try
    LDocument := FDocument;

    if ASource <> nil then
    begin
      LOwnedDocument := ASource.Clone;
      LOwnedDocument.Validate;

      if NyxUTF8ByteCount(TNyxCodec.Encode(LOwnedDocument)) > MaximumNyxDiagramSourceBytes then
      begin
        raise ENyxDiagram.Create('Diagram source exceeds its copied UTF-8 budget');
      end;
      LDocument := LOwnedDocument;
    end;
    LCandidate := NyxDiagramFromData(ADiagram.ToData);
    { ToData belongs to a caller implementation and may run arbitrary UI code.
      Revocation or freshly typed properties must win over the older proposal,
      including a proposal that explicitly discarded the fields seen on entry. }
    RequireConnected;

    if (FRevision <> AExpectedRevision) or (PropertyBuffer <> LPropertyBuffer) then
    begin
      raise ENyxDiagram.Create('Diagram admission changed while its snapshot was captured');
    end;
    LSelection := AVertex;
    LSelectedEdge := Default(TNyxDiagramEdgeRef);
    for LIndex := 0 to LCandidate.EdgeCount - 1 do
    begin

      if LCandidate.Edge(LIndex).Reference.Name = AEdge.Name then
      begin
        LSelectedEdge := AEdge;
        Break;
      end;
    end;

    if LSelectedEdge.Defined then
    begin
      LSelection := Default(TNyxDiagramVertexRef);
    end;
    LFound := False;
    for LIndex := 0 to LCandidate.VertexCount - 1 do
    begin
      LFound := LFound or (LCandidate.Vertex(LIndex).Reference.Name = LSelection.Name);
    end;

    if not LFound then
    begin
      LSelection := Default(TNyxDiagramVertexRef);

      if (LCandidate.VertexCount > 0) and not LSelectedEdge.Defined then
      begin
        LSelection := LCandidate.Vertex(0).Reference;
      end;
    end;
    LProjection := Projection(LCandidate, LSelection, LSelectedEdge);
    LDocument.ValidateDiagramContent(LCandidate);
    LFrame := NyxCanvasControlFrame(FCanvas.Camera, LProjection.ViewBox, nccsReflow);
    NyxDrawingMapping(LProjection, LFrame.Width, LFrame.Height);

    if not Supports(FCanvas, INyxCanvasCoordinatedLayoutView, LView) or
      not Supports(FCanvas, INyxCanvasLayoutView, LPresentation) then
    begin
      raise ENyxDiagram.Create('Diagram host lacks prepared canvas publication');
    end;
    LPlan := BuildContentPlan(LCandidate, LProjection, LContentBindings, LNextContentID, LDocument);
    LCommit := TDiagramEditorCommit.Create;
    LPublication := LCommit;
    LCommit.Owner := Self;
    LCommit.SourceChange := LOwnedDocument <> nil;
    LCommit.NewDocument := LOwnedDocument;
    LOwnedDocument := nil;
    LCommit.SourceInspection := AInspection;
    LCommit.SourceSnapshot := ASnapshot;
    LCommit.ExpectedProperties := LPropertyBuffer;
    LCommit.SaveHost := FSaveHost;
    LCommit.PreviousTarget := FSaveTarget;
    LCommit.Candidate := LCandidate;
    LCommit.Projection := LProjection;
    LCommit.Selection := LSelection;
    LCommit.SelectedEdge := LSelectedEdge;
    LCommit.Expected := FRevision;
    LCommit.Preview := APreview;
    LCommit.Zoom := FCanvas.Camera.Zoom;
    LCommit.ContentBindings := LContentBindings;
    LCommit.NextContentID := LNextContentID;
    try

      if LCommit.SourceChange then
      begin

        if not Supports(FCanvas, INyxCanvasSourceLayoutView, LSourceView) then
        begin
          raise ENyxDiagram.Create('Diagram host lacks prepared source publication');
        end;

        if not LSourceView.ApplySourceLayout(LCommit.NewDocument, LPlan, FCanvas.Camera,
          LPresentation.PresentationRevision, LPublication) then
        begin
          raise ENyxDiagram.Create('Diagram source presentation was unavailable');
        end;
      end
      else if not LView.ApplyCoordinatedLayout(LPlan, FCanvas.Camera,
        LPresentation.PresentationRevision, LPublication) then
      begin
        raise ENyxDiagram.Create('Diagram presentation was not available for publication');
      end;
    finally
      LPublication.Retire;

      if LCommit.Installed and GetConnected then
      begin
        SyncPicture;

        if not APreview then
        begin
          RefreshProperties;
        end;
      end;
    end;
  finally
    LOwnedDocument.Free;
    FPublishing := False;
  end;
end;

procedure TEditorView.ReplaceSource(ADocument: TNyxDocument; const ADiagram: INyxDiagram;
  AExpectedRevision: Integer; ADiscardProperties: Boolean);
begin
  RequireWritable;

  if (ADocument = nil) or (GetHasPendingProperties and not ADiscardProperties) then
  begin
    raise ENyxDiagram.Create('Source replacement requires a document and resolved properties');
  end;
  PublishSourceSelection(ADiagram, AExpectedRevision, FSelection, FSelectedEdge, False,
    ADocument, nil, Default(TNyxDiagramSourceSnapshot));
end;

procedure TEditorView.ReplaceDiagram(const ADiagram: INyxDiagram; AExpectedRevision: Integer;
  ADiscardProperties: Boolean);
begin
  RequireWritable;

  if GetHasPendingProperties and not ADiscardProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished geometry before replacing the diagram');
  end;
  Publish(ADiagram, AExpectedRevision);
end;

procedure TEditorView.SetGeometry(const AReference: TNyxDiagramVertexRef;
  const ABounds: TNyxCanvasBounds; AExpectedRevision: Integer);
begin
  RequireWritable;

  if GetHasPendingProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished geometry before moving a shape');
  end;
  Publish(FDiagram.WithVertex(FDiagram.FindVertex(AReference).AtBounds(ABounds)), AExpectedRevision);
end;

procedure TEditorView.NudgeSelection(AX, AY: Double; AExpectedRevision: Integer);
var
  LVertex: TNyxDiagramVertex;
begin
  RequireWritable;

  if not FSelection.Defined then
  begin
    raise ENyxDiagram.Create('Select a shape before moving it');
  end;
  LVertex := FDiagram.FindVertex(FSelection);
  SetGeometry(FSelection, NyxCanvasBounds(LVertex.Bounds.Left + AX, LVertex.Bounds.Top + AY,
    LVertex.Bounds.Width, LVertex.Bounds.Height), AExpectedRevision);
end;

function GeometryNumber(const AText: TNyxText): Double;
begin

  if not TryNyxStateNumber(AText, Result) then
  begin
    raise ENyxDiagram.Create('Geometry fields require finite numbers');
  end;
end;

procedure TEditorView.ApplyProperties;
var
  LVertex: TNyxDiagramVertex;
  LCandidate: INyxDiagram;
  LPort: TNyxDiagramPortRef;
begin
  RequireWritable;

  if not FSelection.Defined then
  begin
    raise ENyxDiagram.Create('Select a shape before applying properties');
  end;
  LVertex := FDiagram.FindVertex(FSelection)
    .AtBounds(NyxCanvasBounds(GeometryNumber(Field(ndepX).Value),
      GeometryNumber(Field(ndepY).Value), GeometryNumber(Field(ndepWidth).Value),
      GeometryNumber(Field(ndepHeight).Value)))
    .Captioned(Field(ndepCaption).Value).WithContent(ReadContentFields);
  LPort := Default(TNyxDiagramPortRef);

  if Field(ndepPortName).Value <> '' then
  begin
    LVertex := LVertex.WithPort(ReadPortFields);
    LPort := NyxDiagramPortRef(Field(ndepPortName).Value);
  end
  else if PortFieldBuffer <> FPortFieldBaseline then
  begin
    raise ENyxDiagram.Create('Give the new port a stable name before applying its fields');
  end;
  LCandidate := FDiagram.WithVertex(LVertex);
  Publish(LCandidate, FRevision);

  if LPort.Defined and GetConnected then
  begin
    FPortSelection := LPort;
    RefreshPortFields;
    FPortFieldBaseline := PortFieldBuffer;
    FFormBaseline := PropertyBuffer;
    FRenderer.Sync;
  end;
end;

procedure TEditorView.Subscribe(const AEvents: INyxEvents; const AID: TNyxText;
  ATrigger: TNyxTrigger);
var
  LCallback: TDiagramEditorCallback;
  LIndex: Integer;
begin
  LCallback := TDiagramEditorCallback.Create;
  LCallback.Scheduler := FScheduler;
  LCallback.Owner := Self;
  LIndex := Length(FCallbacks);
  SetLength(FCallbacks, LIndex + 1);
  SetLength(FCallbackLeases, LIndex + 1);
  SetLength(FSubscriptions, LIndex + 1);
  FCallbackLeases[LIndex] := LCallback;
  FCallbacks[LIndex] := LCallback;
  FSubscriptions[LIndex] := AEvents.On(NyxControlEvents(AID), ATrigger).Subscribe(LCallback);
end;

procedure TEditorView.Event(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LPart: TNyxDiagramEditorPart;
  LDistance: Double;
  LResponse: INyxEventResponse;
  LHandled: Boolean;
  LPolicy: TNyxInteractionPolicy;
  LHit: TNyxDiagramVertexRef;
  LPortHit: TNyxDiagramPortHit;
  LRemoval: TNyxDiagramRemoval;
begin
  RequireConnected;

  if AEvent.Trigger in [ntPointerMove, ntPointerUp, ntPointerCancel, ntPointerCaptureLost, ntAfterExit] then
  begin
    HandlePointer(AEvent, AExecution);
    Exit;
  end;

  if FPointerActive and AEvent.HasKeyboard and (AEvent.Trigger = ntBeforeKeyDown) and
    AEvent.Keyboard.Matches(nkEscapeKey) then
  begin
    CancelManipulation;
    LResponse := NyxEventResponse(AExecution);

    if LResponse.CanConsume then
    begin
      LResponse.Consume;
    end;
    Exit;
  end;

  if FPointerActive and AEvent.HasPointer and (AEvent.Trigger = ntPointerDown) then
  begin
    CancelManipulation;
    Exit;
  end;
  RequireWritable;
  LPolicy := NyxInteractionPolicy(FRenderer.Root);

  if not LPolicy.CanIssueCommand then
  begin
    Exit;
  end;
  LDistance := FOptions.NudgeDistance;

  if AEvent.HasPointer and (AEvent.Trigger = ntPointerDown) and
    (AEvent.Pointer.Button = npbPrimary) then
  begin
    { Only the selected shape displays port glyphs. Never hit invisible ports
      belonging to other shapes, or steal embedded controls' own pointer input. }
    LPortHit := Default(TNyxDiagramPortHit);

    if FPortToolsExpanded and FSelection.Defined then
    begin
      LPortHit := PickNyxDiagramPort(NewNyxDiagram.WithVertex(FDiagram.FindVertex(FSelection)),
        PointerWorld(AEvent), 8 / FCanvas.Camera.Zoom);
    end;

    if LPortHit.Defined then
    begin

      if LPolicy.ReadOnly then
      begin
        Exit;
      end;

      if HasConnectionIntent then
      begin
        CompletePortConnection(LPortHit.Vertex, LPortHit.Port, FRevision);
      end
      else
      begin
        BeginPortConnection(LPortHit.Vertex, LPortHit.Port, ndrOrthogonal);
      end;
      Feedback('');
      Exit;
    end;

    if HasConnectionIntent then
    begin

      if LPolicy.ReadOnly then
      begin
        Exit;
      end;
      LHit := FDiagram.PickVertex(PointerWorld(AEvent));

      if not LHit.Defined then
      begin
        Feedback('Choose a destination shape or cancel the connection');
        Exit;
      end;
      if FPortToolsExpanded then
      begin
        SelectVertex(LHit, False);
        Feedback('Choose a named port on this shape, or use Connect to port');
        Exit;
      end;
      CompleteConnection(LHit, FRevision);
    end
    else
    begin

      if not LPolicy.ReadOnly and BeginPointer(AEvent, AExecution) then
      begin
        Exit;
      end;
      SelectAt(PointerWorld(AEvent), False);
    end;
    Feedback('');
    Exit;
  end;

  if AEvent.HasKeyboard and (AEvent.Trigger = ntBeforeKeyDown) then
  begin

    if (AEvent.Keyboard.Modifiers <> []) or LPolicy.ReadOnly then
    begin
      Exit;
    end;
    LHandled := FSelection.Defined and
      (AEvent.Keyboard.Key in [nkLeftKey, nkUpKey, nkRightKey, nkDownKey]);

    if not LHandled then
    begin
      Exit;
    end;
    { Reserve the recognized key's default response even when draft/geometry
      admission subsequently refuses; a failed nudge must not scroll the page. }
    LResponse := NyxEventResponse(AExecution);
    LResponse.Consume;
    case AEvent.Keyboard.Key of
      nkLeftKey: NudgeSelection(-LDistance, 0, FRevision);
      nkUpKey: NudgeSelection(0, -LDistance, FRevision);
      nkRightKey: NudgeSelection(LDistance, 0, FRevision);
      nkDownKey: NudgeSelection(0, LDistance, FRevision);
    else
      Exit;
    end;
    Feedback('');
    Exit;
  end;
  for LPart := Low(TNyxDiagramEditorPart) to High(TNyxDiagramEditorPart) do
  begin

    if AEvent.OriginID <> NyxDiagramEditorPart(FEditorID, LPart).ID then
    begin
      Continue;
    end;

    if LPolicy.ReadOnly and (LPart in [ndepLeft, ndepUp, ndepRight,
      ndepDown, ndepApply, ndepRestore, ndepSave, ndepReloadSaved,
      ndepApplyContent, ndepRestoreContent]) then
    begin
      Exit;
    end;

    if LPolicy.ReadOnly and (LPart in [ndepAddRectangle, ndepAddEllipse,
      ndepAddDiamond, ndepStartOrthogonal, ndepStartStraight, ndepFinishConnection,
      ndepRemoveVertex, ndepStraightEdge, ndepOrthogonalEdge, ndepRemoveEdge,
      ndepReconnectStart, ndepReconnectFinish]) then
    begin
      Exit;
    end;

    if LPolicy.ReadOnly and (LPart in [ndepPortChoice, ndepApplyPort, ndepRestorePort,
      ndepRemovePort, ndepStartPortConnection, ndepFinishPortConnection]) then
    begin
      Exit;
    end;
    case LPart of
      ndepPrevious: AdvanceSelection(-1);
      ndepNext: AdvanceSelection(1);
      ndepZoomOut: FCanvas.Execute(nccZoomOut);
      ndepZoomIn: FCanvas.Execute(nccZoomIn);
      ndepFit: FCanvas.Execute(nccFit);
      ndepActualSize:
        begin
          FCanvas.ZoomAt(1, NyxCanvasPoint(FCanvas.Camera.ViewportWidth / 2,
            FCanvas.Camera.ViewportHeight / 2));
          RevealSelection;
        end;
      ndepToggleTools:
        begin

          if FSelectedEdge.Defined then
          begin
            ShowDrawTools(True);
          end
          else
          begin
            ShowTools(not FToolsExpanded);
          end;
        end;
      ndepLeft: NudgeSelection(-LDistance, 0, FRevision);
      ndepUp: NudgeSelection(0, -LDistance, FRevision);
      ndepRight: NudgeSelection(LDistance, 0, FRevision);
      ndepDown: NudgeSelection(0, LDistance, FRevision);
      ndepApply: ApplyProperties;
      ndepRestore: RestoreProperties;
      ndepSelection: RevealSelection;
      ndepSave: Save;
      ndepToggleDraw: ShowDrawTools(not FDrawToolsExpanded);
      ndepToggleContent: ShowContentTools(not FContentToolsExpanded);
      ndepTogglePorts: ShowPortTools(not FPortToolsExpanded);
      ndepPortChoice: ChoosePort;
      ndepApplyPort:
        begin

          if Field(ndepPortName).Value = '' then
          begin
            raise ENyxDiagram.Create('Give the port a stable name before applying');
          end;
          ApplyProperties;
        end;
      ndepRestorePort: RestoreProperties;
      ndepRemovePort:
        begin
          LRemoval := ndrmRequireDisconnected;

          if (RetainNyxControl(Part(ndepPortIncidentEdges)) as INyxBooleanInput).Checked then
          begin
            LRemoval := ndrmIncidentEdges;
          end;
          RemovePort(FSelection, FPortSelection, LRemoval, FRevision);
          (RetainNyxControl(Part(ndepPortIncidentEdges)) as INyxBooleanInput).Checked := False;
        end;
      ndepStartPortConnection: BeginPortConnection(FSelection, FPortSelection, ndrOrthogonal);
      ndepFinishPortConnection: CompletePortConnection(FSelection, FPortSelection, FRevision);
      ndepCancelPortConnection: CancelConnection;
      ndepApplyContent: ApplyProperties;
      ndepRestoreContent: RestoreProperties;
      ndepContentKind:
        begin
          RefreshContentChoices;
          RefreshContentHelp;
          FRenderer.Sync;
        end;
      ndepContentReference, ndepContentLocalized:
        begin
          RefreshContentHelp;
          FRenderer.Sync;
        end;
      ndepAddRectangle: AddShapeAtCenter(ndsRectangle);
      ndepAddEllipse: AddShapeAtCenter(ndsEllipse);
      ndepAddDiamond: AddShapeAtCenter(ndsDiamond);
      ndepStartOrthogonal: BeginConnection(FSelection, ndrOrthogonal);
      ndepStartStraight: BeginConnection(FSelection, ndrStraight);
      ndepFinishConnection: CompleteConnection(FSelection, FRevision);
      ndepCancelConnection: CancelConnection;
      ndepPreviousEdge: AdvanceEdgeSelection(-1);
      ndepNextEdge: AdvanceEdgeSelection(1);
      ndepStraightEdge: SetEdge(FDiagram.FindEdge(FSelectedEdge).Routed(ndrStraight), FRevision);
      ndepOrthogonalEdge: SetEdge(FDiagram.FindEdge(FSelectedEdge).Routed(ndrOrthogonal), FRevision);
      ndepRemoveEdge: RemoveEdge(FSelectedEdge, FRevision);
      ndepReconnectStart: BeginReconnect(FSelectedEdge, ndceStart);
      ndepReconnectFinish: BeginReconnect(FSelectedEdge, ndceFinish);
      ndepRemoveVertex:
        begin
          LRemoval := ndrmRequireDisconnected;

          if (RetainNyxControl(Part(ndepIncidentEdges)) as INyxBooleanInput).Checked then
          begin
            LRemoval := ndrmIncidentEdges;
          end;
          RemoveVertex(FSelection, LRemoval, FRevision);
          (RetainNyxControl(Part(ndepIncidentEdges)) as INyxBooleanInput).Checked := False;
        end;
      ndepReloadSaved:
        begin

          if GetHasUnsavedChanges and not
            (RetainNyxControl(Part(ndepConfirmReload)) as INyxBooleanInput).Checked then
          begin
            RetainNyxControl(Part(ndepConfirmReload)).Configure.Visible(True).Done;
            Feedback('Confirm discarding local changes before reloading the saved diagram');
            Exit;
          end;
          ReloadSaved((RetainNyxControl(Part(ndepConfirmReload)) as INyxBooleanInput).Checked);
        end;
    else
      Exit;
    end;
    Feedback('');
    Exit;
  end;
end;

procedure TEditorView.Disconnect;
var
  LIndex: Integer;
begin

  if FDisconnecting or ((FRenderer = nil) and (FCanvas = nil) and (FDocument = nil)) then
  begin
    Exit;
  end;

  if FScheduler <> nil then
  begin
    FScheduler.RequireUI;
  end;
  FDisconnecting := True;
  FConnected := False;

  if FFrameWork <> nil then
  begin
    FFrameWork.Owner := nil;
  end;
  FFrameWork := nil;
  FFrameLease := nil;

  if FSaveRequestOwner <> nil then
  begin
    FSaveRequestOwner.Owner := nil;
    FSaveRequestOwner.PendingValue := False;
  end;
  FSaveRequestOwner := nil;
  FSaveRequest := nil;
  FSaveHost := nil;

  if FObserver <> nil then
  begin
    FObserver.Owner := nil;
  end;

  if FObservation <> nil then
  begin
    FObservation.Cancel;
    FObservation := nil;
  end;
  FObserverLease := nil;
  FObserver := nil;
  for LIndex := 0 to High(FCallbacks) do
  begin

    if FCallbacks[LIndex] <> nil then
    begin
      FCallbacks[LIndex].Owner := nil;
    end;
  end;
  for LIndex := 0 to High(FSubscriptions) do
  begin

    if FSubscriptions[LIndex] <> nil then
    begin
      FSubscriptions[LIndex].Cancel;
    end;
  end;
  FCallbacks := nil;
  FSubscriptions := nil;
  FCallbackLeases := nil;
  try
    RetirePointer;
  except
    { Target retirement must still revoke the view if physical release refuses. }
  end;
  FPreviewDiagram := nil;
  FPointerEvents := nil;

  if FCanvas <> nil then
  begin
    FCanvas.Disconnect;
    FCanvas := nil;
  end;
  FreeAndNil(FRenderer);
  FWorkspace := nil;
  FScene := nil;
  FContentBindings := nil;
  FPicture := nil;
  FProvider := nil;
  FProviderLease := nil;
  FDiagram := nil;
  FreeAndNil(FDocument);

  if FOwnState then
  begin
    FreeAndNil(FState);
  end;
end;

{$I nyx.diagram.editor.authoring.inc}
{$I nyx.diagram.editor.manipulation.inc}
{$I nyx.diagram.editor.content.inc}
{$I nyx.diagram.editor.ports.inc}
