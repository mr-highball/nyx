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


unit nyx.tool.canvas;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.behavior, nyx.canvas.types, nyx.canvas.scene, nyx.canvas.workspace,
  nyx.tool.ports, nyx.scheduler, nyx.tool.connections, nyx.events,
  nyx.canvas.routes, nyx.flow.controls.editor, nyx.tool.wires, nyx.canvas.geometry,
  nyx.canvas.adoption, nyx.content.mount, nyx.flow.declarations,
  nyx.programs.declarations, nyx.flow.types;

type
  { Public portable inspector/controller consumed by both target attachments.
    It owns an independent source context and Nyx form, never a widget/renderer.
    Runtime scene/behavior access is revoked by Disconnect before borrowed hosts
    retire. The optional connection capability below adds runtime graph editing
    through public Nyx forms; script/source editing retains its separate owner. }
  INyxToolCanvasInspector = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000019}']
    procedure Disconnect;
    function GetDocument: TNyxDocument;
    function GetRoot: INyxCard;
    function Handle(ANode: TNyxNode; const AEvent: TNyxEventInfo): Boolean;
    procedure SelectControl(const AItem: TNyxCanvasItemRef; const AControl: TNyxControlRef);
    property Document: TNyxDocument read GetDocument;
    property Root: INyxCard read GetRoot;
  end;

  { Optional connection editing preserves the original inspector ABI. Physical
    adapters provide a scoped gesture response; programmatic creators can use
    the same typed controller without acquiring widgets or renderer authority.
    ConnectionEditor is nil for creator views lacking optional flow editing. }
  INyxToolCanvasConnections = interface(IInterface)
    ['{8902BF7F-5E04-40A0-A098-91DB02CC1501}']
    function ConnectionEditor: INyxToolConnectionEditor;
    procedure ShowConnections(AVisible: Boolean = True);
    function HandleGesture(ANode: TNyxNode; const AEvent: TNyxEventInfo;
      const AResponse: INyxGestureResponse): Boolean;
  end;

  { Closed target-adapter publication request. Synchronize updates the current
    form's values/visibility without replacing its focused editor or caret.
    Rebuild publishes a newly composed form on the adapter's next UI turn. }
  TNyxToolCanvasUpdate = (ntcuNone, ntcuSynchronize, ntcuRebuild);

  { Optional portable input bridge for a renderer's actual owned root. Public
    Nyx forms require exact tree membership; a borrowed event node alone cannot
    stand in for the independently owned blueprint. No DOM/LCL types enter it. }
  INyxToolCanvasInput = interface(IInterface)
    ['{32496DC3-1296-49C2-865B-6A790C8717F6}']
    function HandleInput(ANode, ARoot: TNyxNode;
      const AEvent: TNyxEventInfo): TNyxToolCanvasUpdate;
  end;

  { Optional managed control/event-to-script route authoring. It composes the
    same public form used by Studio and applies only runtime item overrides.
    Hiding/reopening the same selection retains unfinished fields. A stale
    proposal remains owned and refuses Apply; reload requires explicit discard
    when a draft exists. The returned specialized form is independently retained
    and conveys no renderer, host or source-history authority. }
  INyxToolCanvasRouteAuthoring = interface(IInterface)
    ['{AAE1FDEB-514E-402B-AB1A-E830D464B2F7}']
    procedure ShowEventRoutes(AVisible: Boolean = True);
    procedure ReloadEventRoutes(ADiscardDraft: Boolean = False);
    function GetRouteEditor: INyxControlRouteEditor;
    property RouteEditor: INyxControlRouteEditor read GetRouteEditor;
  end;

  { Optional runtime Logic workbench. It composes the public operation toolbox,
    typed configuration/ports and saved-program discovery without a second graph
    engine. Show/hide retains unfinished fields; reload/navigation refuse their
    loss without explicit discard. Steps/wires stage in an independent proposal;
    ApplyLogic publishes the complete valid group through exact scene/flow guards
    and prepared publication, preserving mounted controls. RuntimeFlows remains
    the explicit detached export boundary; this interface grants no project save,
    script-source compiler or host authority. All calls/final release are UI-only.
    Editor is independently retained, nil until opened, and has no backreference. }
  INyxToolCanvasLogicAuthoring = interface(IInterface)
    ['{8D1EBC25-A5D3-4945-A5B2-1A1016000001}']
    procedure ShowLogic(AVisible: Boolean = True);
    procedure SelectLogicFlow(const AReference: TNyxFlowRef;
      ADiscardDraft: Boolean = False);
    procedure ReloadLogic(ADiscardDraft: Boolean = False);
    procedure ApplyLogic;
    function GetHasPendingLogic: Boolean;
    function GetLogicEditor: INyxCard;
    property LogicEditor: INyxCard read GetLogicEditor;
    property HasPendingLogic: Boolean read GetHasPendingLogic;
  end;

  { Optional UI-only interpreted Pascal authoring. The returned public Nyx form
    owns copied source/schema metadata, never compiler/host authority. Invalid
    proposals and tab switches retain exact fields; reload/navigation require
    explicit discard when unfinished. Applying uses the canvas's prepared program
    publisher, keeping embedded controls and unrelated callbacks alive. Applied
    definitions export through RuntimePrograms or the explicitly configured save
    host. Nil Editor means the form has not been opened. }
  INyxToolCanvasProgramAuthoring = interface(IInterface)
    ['{8D1EBC25-A5D3-4945-A5B2-1A1016000003}']
    procedure ShowPrograms(AVisible: Boolean = True);
    procedure SelectProgram(const AReference: TNyxProgramRef; ADiscardDraft: Boolean = False);
    procedure ReloadProgram(ADiscardDraft: Boolean = False);
    function GetProgramEditor: INyxCard;
    function GetHasPendingProgram: Boolean;
    property ProgramEditor: INyxCard read GetProgramEditor;
    property HasPendingProgram: Boolean read GetHasPendingProgram;
  end;

  { Optional canvas-anchored control/pipeline connection presentation. Hiding
    retains unfinished wiring. A different selected item requires explicit
    discard before replacing its context. The controller exposes copied typed
    metadata and ordinary independent Nyx faces, never target widgets. }
  INyxToolCanvasControlConnectors = interface(IInterface)
    ['{1E3BE37E-AAD2-46CE-8D87-64341EE0091C}']
    procedure ShowControlConnectors(AVisible: Boolean = True; ADiscardDraft: Boolean = False);
    function GetControlConnectorsVisible: Boolean;
    function GetControlWires: INyxToolControlWires;
    property ControlConnectorsVisible: Boolean read GetControlConnectorsVisible;
    property ControlWires: INyxToolControlWires read GetControlWires;
  end;

  { Optional viewport-logical placement of the pipeline face. Drag/keyboard
    movement and caller placement share this contract; the target clips it to
    its actual allocated surface. Reset restores adaptive default placement.
    Positions own no platform handles and do not mutate scene/history. }
  INyxToolCanvasConnectorPresentation = interface(IInterface)
    ['{DCB784EF-D15E-4C97-9D1F-C58D2966B181}']
    procedure MovePipeline(const APosition: TNyxCanvasPoint);
    procedure ResetPipelinePosition;
    function GetPipelineBounds: TNyxCanvasBounds;
    property PipelineBounds: TNyxCanvasBounds read GetPipelineBounds;
  end;

  { Copied runtime presentation position for a named operation in one graph.
    World points move with the camera; the ordinary Nyx card keeps logical size.
    Home is the initial placement, independent of later pan/zoom. These values
    own no control or graph and are not implicitly saved into design/history. }
  TNyxToolPipelineNodePosition = record
    Flow: TNyxFlowRef;
    Step: TNyxFlowStepRef;
    Position: TNyxCanvasPoint;
    Home: TNyxCanvasPoint;
  end;

  { Optional UI-only expansion of an already enabled canvas connection layer.
    Per-step Nyx cards share the captured route controller and internal graph
    metadata. Full face/port budgets admit before switching; no graph executes.
    Move/Reset and pointer/keyboard input use world coordinates, with logical
    card/font size on both adapters. Bounds are viewport-logical, undefined when
    the requested node is not mounted. Export is an explicit inert persistence
    boundary for current runtime positions, not a project save or live handle.
    Disconnect revokes all use before borrowed hosts/controls retire. }
  INyxToolCanvasPipelineNodes = interface(IInterface)
    ['{8D1EBC25-A5D3-4945-A5B2-1A1016000011}']
    procedure ShowPipelineNodes(AVisible: Boolean = True;
      AMaximumNodes: Integer = 128; AMaximumPorts: Integer = 4096);
    function GetPipelineNodesVisible: Boolean;
    procedure MovePipelineNode(const AStep: TNyxFlowStepRef; const AWorld: TNyxCanvasPoint);
    procedure ResetPipelineNode(const AStep: TNyxFlowStepRef);
    function PipelineNodeBounds(const AStep: TNyxFlowStepRef): TNyxCanvasBounds;
    function ExportPipelineNodePositions: TNyxDataValue;
    property PipelineNodesVisible: Boolean read GetPipelineNodesVisible;
  end;

  { Optional project adoption preserves the original inspector interfaces.
    A host explicitly owns persistence; the inspector never writes files/source.
    Configure is UI-only and refuses an unresolved submission. The borrowed
    Changed receiver must outlive this attachment and is revoked on Disconnect.
    Saving captures applied graphs/routes only; unfinished forms and later live changes
    remain owned. Successful acknowledgement updates the source baseline while
    preserving the mounted runtime, controller, local forms and their drafts. }
  INyxToolCanvasRoutePersistence = interface(IInterface)
    ['{2B549B67-182B-493A-9B06-89FDA7D7E463}']
    procedure ConfigureAdoption(const AOwner: TNyxControlRef;
      const AHost: INyxCanvasRouteAdoptionHost; AChanged: TNyxContentUpdate);
    procedure SaveConnections;
    function GetAdoptionPending: Boolean;
    function GetAdoptionFeedback: TNyxText;
    property AdoptionPending: Boolean read GetAdoptionPending;
    property AdoptionFeedback: TNyxText read GetAdoptionFeedback;
  end;

  { Adapter-owned face. Close precedes disposal of its borrowed host and the
    canvas it observes. Inspector values remain ordinary public Nyx controls. }
  INyxToolCanvasView = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000020}']
    procedure Disconnect;
    procedure Refresh;
    function GetInspector: INyxToolCanvasInspector;
    function GetRefreshExecution: INyxExecution;
    property Inspector: INyxToolCanvasInspector read GetInspector;
    { Deferred physical publication reports ordinary scheduler failure. Its
      diagnostic can safely outlive the retired view. }
    property RefreshExecution: INyxExecution read GetRefreshExecution;
  end;

{ An existing ordinary canvas view supplies independently owned item content and
  optional behavior. The form uses normal Nyx cards/inputs/selects/switches/badges.
  Query and execution feedback are bounded; creator callback payloads are never
  displayed as executable source or persisted runtime authority. }
function NewNyxToolCanvasInspector(const AID: TNyxText; ADocument: TNyxDocument;
  const AView: INyxCanvasWorkspaceView): INyxToolCanvasInspector;

implementation

uses
  nyx.composition, nyx.logic.catalog, nyx.view.behavior,
  nyx.state, nyx.root.types, nyx.codec, nyx.flow.routes, nyx.flow.route.model,
  nyx.canvas.observers, nyx.logic.editor, nyx.flow.editing,
  nyx.programs.editor, nyx.programs.editing;

type
  { Forward reference for the inspector's privately owned pending ticket.
    Its complete declaration below defines the revocable weak receiver contract. }
  TAdoptionRequest = class;

  { Forward declaration for the revocable shared Logic receiver. Its managed
    lease retains only the canvas flow capability, never its Inspector owner. }
  TLogicGraphHost = class;

  { Closed inspector presentation. Each port/connection/event/Logic/program mode
    uses the available panel space instead of stacking multiple long forms. }
  TInspectorTab = (titPorts, titConnections, titEventRoutes, titLogic, titPrograms);

  { Independent UI form/source owner. Its connection controller retains only
    the separate canvas view; no returned node retains this inspector. }
  TInspector = class(TInterfacedObject, INyxToolCanvasInspector, INyxToolCanvasConnections,
    INyxToolCanvasInput, INyxToolCanvasRouteAuthoring, INyxToolCanvasControlConnectors,
    INyxToolCanvasRoutePersistence, INyxToolCanvasLogicAuthoring, INyxToolCanvasProgramAuthoring)
  private
    FID: TNyxText;
    FSource: TNyxDocument;
    FDocument: TNyxDocument;
    FRoot: INyxCard;
    FView: INyxCanvasWorkspaceView;
    FRuntime: INyxCanvasRuntimeView;
    FItems: array of TNyxCanvasItemRef;
    FItemLabels: array of TNyxText;
    FControls: array of TNyxControlRef;
    FControlLabels: array of TNyxText;
    { One effective item snapshot avoids revalidating unrelated scenes on every
      search/page/toggle. Exact content bytes and scene revision invalidate it;
      external mutation of a scene control cannot leave stale port discovery. }
    FEffectiveItem: TNyxCanvasItemRef;
    FEffectiveContext: TNyxDocument;
    FEffectiveRoot: TNyxNode;
    FEffectiveRevision: Integer;
    FEffectiveContent: TNyxText;
    FSelectedItem: TNyxCanvasItemRef;
    FSelectedControl: TNyxControlRef;
    FQuery: TNyxText;
    FPortOffset: Integer;
    FShowPorts: Boolean;
    FConnected: Boolean;
    FUpdating: Boolean;
    FConnections: INyxToolConnectionEditor;
    FRouteEditing: INyxCanvasRouteEditing;
    FRouteContext: TNyxDocument;
    FRouteEditor: INyxControlRouteEditor;
    FRouteDraft: TNyxControlRouteEditorDraft;
    FRouteItem: TNyxCanvasItemRef;
    FRouteControl: TNyxControlRef;
    FRouteScene: Integer;
    FRouteRevision: Integer;
    FRouteFeedback: TNyxText;
    FFlowEditing: INyxCanvasFlowEditing;
    FLogicContext: TNyxDocument;
    FLogicBaseFlows: INyxFlowDeclarations;
    FLogicEditor: INyxCard;
    FLogicDraft: TNyxLogicEditorDraft;
    FLogicPristine: TNyxLogicEditorDraft;
    FLogicFlow: TNyxFlowRef;
    FLogicScene: Integer;
    FLogicRevision: Integer;
    FLogicFeedback: TNyxText;
    FLogicVersion: Integer;
    FLogicHost: TLogicGraphHost;
    FLogicHostLease: INyxToolGraphHost;
    FProgramEditing: INyxCanvasProgramEditing;
    FProgramContext: TNyxDocument;
    FProgramEditor: INyxCard;
    FProgramDraft: TNyxProgramEditorDraft;
    FProgramPristine: TNyxProgramEditorDraft;
    FProgramReference: TNyxProgramRef;
    FProgramScene: Integer;
    FProgramRevision: Integer;
    FProgramFeedback: TNyxText;
    FProgramCheckFeedback: TNyxText;
    FControlWires: INyxToolControlWires;
    FControlConnectorsVisible: Boolean;
    FControlConnectorFeedback: TNyxText;
    FTab: TInspectorTab;
    FAdoptionOwner: TNyxControlRef;
    FAdoptionHost: INyxCanvasRouteAdoptionHost;
    FAdoptionChanged: TNyxContentUpdate;
    FAdoptionFeedback: TNyxText;
    { Raw access borrows the same ticket held by the managed lease. Disconnect
      revokes its receiver before releasing that lease or any private context. }
    FAdoptionRequest: TAdoptionRequest;
    FAdoptionLease: INyxCanvasRouteAdoptionRequest;
    procedure AdoptionCompleted(ARequest: TAdoptionRequest;
      AOwnedSource: TNyxDocument; const AMessage: TNyxText);
    procedure UpdateAdoptionFeedback;
    procedure AddTabs(const AParent: INyxCard);
    procedure PublishForm(const ARoot: INyxCard);
    procedure RequireConnected;
    procedure Rebuild;
    function PartID(const APart: TNyxText): TNyxText;
    function CurrentBehavior: INyxViewBehavior;
    procedure BuildEventRoutes(const AParent: INyxCard);
    procedure BuildLogic(const AParent: INyxCard);
    procedure EnsureLogicContext;
    function NewLogicContext(out AFlows: INyxFlowDeclarations): TNyxDocument;
    procedure SelectLogicProposal(const AReference: TNyxFlowRef;
      ADiscardDraft, AActivate: Boolean);
    function CaptureLogicFrame(const AFlow: TNyxFlowRef): TNyxToolGraphFrame;
    procedure CheckLogicFrame(const AExpected: TNyxToolGraphFrame);
    procedure StageLogicChanges(const AChanges: array of TNyxFlowChange);
    function HandleLogic(ANode, ARoot: TNyxNode;
      const AEvent: TNyxEventInfo): TNyxToolCanvasUpdate;
    procedure BuildPrograms(const AParent: INyxCard);
    function HandlePrograms(ANode, ARoot: TNyxNode;
      const AEvent: TNyxEventInfo): TNyxToolCanvasUpdate;
    function HandleBase(ANode: TNyxNode; const AEvent: TNyxEventInfo): Boolean;
  public
    constructor Create(const AID: TNyxText; ADocument: TNyxDocument;
      const AView: INyxCanvasWorkspaceView);
    destructor Destroy; override;
    procedure Disconnect;
    function GetDocument: TNyxDocument;
    function GetRoot: INyxCard;
    function Handle(ANode: TNyxNode; const AEvent: TNyxEventInfo): Boolean;
    function HandleInput(ANode, ARoot: TNyxNode;
      const AEvent: TNyxEventInfo): TNyxToolCanvasUpdate;
    procedure ShowEventRoutes(AVisible: Boolean = True);
    procedure ReloadEventRoutes(ADiscardDraft: Boolean = False);
    function GetRouteEditor: INyxControlRouteEditor;
    procedure ShowControlConnectors(AVisible: Boolean = True; ADiscardDraft: Boolean = False);
    function GetControlConnectorsVisible: Boolean;
    function GetControlWires: INyxToolControlWires;
    procedure SelectControl(const AItem: TNyxCanvasItemRef; const AControl: TNyxControlRef);
    function ConnectionEditor: INyxToolConnectionEditor;
    procedure ShowConnections(AVisible: Boolean = True);
    function HandleGesture(ANode: TNyxNode; const AEvent: TNyxEventInfo;
      const AResponse: INyxGestureResponse): Boolean;
    procedure ConfigureAdoption(const AOwner: TNyxControlRef;
      const AHost: INyxCanvasRouteAdoptionHost; AChanged: TNyxContentUpdate);
    procedure SaveConnections;
    function GetAdoptionPending: Boolean;
    function GetAdoptionFeedback: TNyxText;
    procedure ShowLogic(AVisible: Boolean = True);
    procedure SelectLogicFlow(const AReference: TNyxFlowRef;
      ADiscardDraft: Boolean = False);
    procedure ReloadLogic(ADiscardDraft: Boolean = False);
    procedure ApplyLogic;
    function GetHasPendingLogic: Boolean;
    function GetLogicEditor: INyxCard;
    procedure ShowPrograms(AVisible: Boolean = True);
    procedure SelectProgram(const AReference: TNyxProgramRef; ADiscardDraft: Boolean = False);
    procedure ReloadProgram(ADiscardDraft: Boolean = False);
    function GetProgramEditor: INyxCard;
    function GetHasPendingProgram: Boolean;
  end;

  { Revocable UI bridge onto the Inspector's existing staged graph. The flow
    capability enforces scheduler/lifetime before the raw receiver is read.
    Each call takes a temporary Inspector lease; Disconnect clears Owner first.
    Neither returned frames nor controller-held leases form an ownership cycle. }
  TLogicGraphHost = class(TInterfacedObject, INyxToolGraphHost)
  private
    FEditing: INyxCanvasFlowEditing;
    function Acquire: INyxToolCanvasInspector;
  public
    Owner: TInspector;
    constructor Create(AOwner: TInspector; const AEditing: INyxCanvasFlowEditing);
    function OpenGraph(const AFlow: TNyxFlowRef;
      ADiscardDraft: Boolean): TNyxToolGraphFrame;
    function ReadGraph(const AFlow: TNyxFlowRef): TNyxToolGraphFrame;
    function GraphVersion(const AFlow: TNyxFlowRef): Integer;
    function StageGraph(const AExpected: TNyxToolGraphFrame;
      const AChanges: array of TNyxFlowChange): TNyxToolGraphFrame;
    function ApplyGraph(const AExpected: TNyxToolGraphFrame): TNyxToolGraphFrame;
    function ReloadGraph(const AFlow: TNyxFlowRef;
      ADiscardDraft: Boolean): TNyxToolGraphFrame;
  end;

  { One immutable submitted snapshot with a borrowed, revocable UI receiver.
    External hosts may retain the ticket after its inspector/view retires; no
    strong backreference keeps those owners alive. Completion acquires both
    request and inspector leases before calling owner code. }
  TAdoptionRequest = class(TInterfacedObject, INyxCanvasRouteAdoptionRequest)
  private
    FOwner: TInspector;
    FSnapshot: TNyxCanvasRouteAdoptionSnapshot;
    FPending: Boolean;
  public
    constructor Create(AOwner: TInspector; const ASnapshot: TNyxCanvasRouteAdoptionSnapshot);
    procedure Retire;
    function GetSnapshot: TNyxCanvasRouteAdoptionSnapshot;
    function GetPending: Boolean;
    function Accepted(ADocument: TNyxDocument): Boolean;
    function Refused(const AMessage: TNyxText): Boolean;
  end;

{$I nyx.tool.canvas.adoption.inc}

function Caption(const AText: TNyxText; AMaximum: Integer): TNyxText;
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  Result := '';
  LIndex := 1;
  LCount := 0;
  while (LIndex <= Length(AText)) and (LCount < AMaximum) do
  begin
    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise ENyxModel.Create('Tool port caption has invalid Unicode');
    end;

    if LScalar < 32 then
    begin
      Result := Result + ' ';
    end
    else
    begin
      Result := Result + NyxScalarText(LScalar);
    end;
    Inc(LCount);
  end;
end;

constructor TInspector.Create(const AID: TNyxText; ADocument: TNyxDocument;
  const AView: INyxCanvasWorkspaceView);
var
  LRuntime: INyxCanvasRuntimeView;
  LEditing: INyxCanvasFlowEditing;
  LRoutes: INyxCanvasRouteEditing;
  LPrograms: INyxCanvasProgramEditing;
begin
  inherited Create;

  if Supports(AView, INyxCanvasProgramEditing, LPrograms) then
  begin
    FProgramEditing := LPrograms;
  end;

  if (ADocument = nil) or (AView = nil) or
    not Supports(AView, INyxCanvasRuntimeView, LRuntime) then
  begin
    raise ENyxModel.Create('Tool Canvas inspector requires its source and mounted runtime view');
  end;
  { pas2js finalizes a direct interface out-field argument on routine exit.
    Admit into a local first, then assign an ordinary retained field lease. }
  FRuntime := LRuntime;
  FID := AID;
  FSource := ADocument.Clone;
  { The ordinary inspector form owns only its private controls. Retaining the
    complete canvas/application in that render document repeatedly validates
    unrelated scenes and graphs on each UI refresh. Discovery uses the separate
    immutable source context; no state projection is authored by this form. }
  FDocument := TNyxDocument.Create;
  FView := AView;
  FConnected := True;
  FShowPorts := True;

  if Supports(FView, INyxCanvasFlowEditing, LEditing) then
  begin
    FFlowEditing := LEditing;
    FLogicHost := TLogicGraphHost.Create(Self, LEditing);
    FLogicHostLease := FLogicHost;
    FConnections := NewNyxToolConnectionEditor(PartID('connections'), FView);
  end;
  Supports(FView, INyxCanvasRouteEditing, LRoutes);
  FRouteEditing := LRoutes;
  Rebuild;
end;

destructor TInspector.Destroy;
begin
  Disconnect;
  ReleaseNyxNode(FEffectiveRoot);
  FEffectiveContext.Free;
  FSource.Free;
  FRouteContext.Free;
  FLogicContext.Free;
  FProgramContext.Free;
  FDocument.Free;
  FRoot := nil;
  inherited Destroy;
end;

procedure TInspector.RequireConnected;
begin

  if not FConnected or (FView = nil) or not FView.Connected then
  begin
    raise ENyxModel.Create('Tool Canvas inspector is retired');
  end;
end;

procedure TInspector.Disconnect;
begin
  FConnected := False;

  if FLogicHost <> nil then
  begin
    FLogicHost.Owner := nil;
    FLogicHost := nil;
  end;
  FLogicHostLease := nil;
  FAdoptionChanged := nil;
  FAdoptionHost := nil;

  if FAdoptionRequest <> nil then
  begin
    FAdoptionRequest.Retire;
    FAdoptionRequest := nil;
  end;
  FAdoptionLease := nil;

  if FControlWires <> nil then
  begin
    FControlWires.Disconnect;
    FControlWires := nil;
  end;

  if FConnections <> nil then
  begin
    FConnections.Disconnect;
  end;
  FView := nil;
  FRuntime := nil;
  FRouteEditing := nil;
  FFlowEditing := nil;
  FProgramEditing := nil;
end;

function TInspector.GetDocument: TNyxDocument;
begin
  RequireConnected;
  Result := FDocument;
end;

function TInspector.GetRoot: INyxCard;
begin
  RequireConnected;
  Result := FRoot;
end;

function TInspector.PartID(const APart: TNyxText): TNyxText;
begin
  Result := FID + '-' + APart;
end;

{$I nyx.tool.canvas.routes.inc}
{$I nyx.tool.canvas.logic.inc}
{$I nyx.tool.canvas.programs.inc}

procedure TInspector.ShowControlConnectors(AVisible: Boolean; ADiscardDraft: Boolean);
var
  LSnapshot: INyxCanvasSnapshot;
  LItem: TNyxCanvasItemRef;
  LGeometry: INyxCanvasControlGeometry;
  LObservation: INyxCanvasPresentationObservation;
  LNext: INyxToolControlWires;
  LGraph: INyxToolPipelineGraphEditing;
begin
  RequireConnected;

  if AVisible then
  begin

    if not Supports(FView, INyxCanvasControlGeometry, LGeometry) or
      not Supports(FView, INyxCanvasPresentationObservation, LObservation) then
    begin
      raise ENyxModel.Create('This canvas does not expose revocable control geometry');
    end;
    LItem := FSelectedItem;

    if not LItem.Defined then
    begin
      LSnapshot := FRuntime.Scene.Snapshot;

      if LSnapshot.Count = 0 then
      begin
        raise ENyxModel.Create('Add an authored canvas item before enabling its connections');
      end;
      LItem := LSnapshot.Items[0].Item;
    end;

    if (FControlWires = nil) or (FControlWires.Item.Name <> LItem.Name) then
    begin

      if (FControlWires <> nil) and FControlWires.Dirty and not ADiscardDraft then
      begin
        raise ENyxModel.Create('The previous item retains unfinished wires. Reload it with explicit discard before switching items.');
      end;
      LNext := NewNyxToolControlWires(PartID('canvas-wires'), FView, LItem,
        TNyxToolWireOptions.Defaults);

      if Supports(LNext, INyxToolPipelineGraphEditing, LGraph) then
      begin
        LGraph.ConfigureGraphHost(FLogicHostLease);
      end;

      if FControlWires <> nil then
      begin
        FControlWires.Disconnect;
      end;
      FControlWires := LNext;
    end;
  end;
  FControlConnectorsVisible := AVisible;
  FControlConnectorFeedback := '';
  Rebuild;
end;

function TInspector.GetControlConnectorsVisible: Boolean;
begin
  RequireConnected;
  Result := FControlConnectorsVisible;
end;

function TInspector.GetControlWires: INyxToolControlWires;
begin
  RequireConnected;
  Result := FControlWires;
end;


function TInspector.CurrentBehavior: INyxViewBehavior;
begin
  RequireConnected;
  Result := nil;

  if FSelectedItem.Defined then
  begin
    try
      Result := FRuntime.BehaviorFor(FSelectedItem);
    except
      on E: ENyxCanvasScene do
      begin
        { Off-screen items may not have a physical mount yet. Inspector discovery
          still works from their owned content, without creating hidden controls. }
      end;
    end;
  end;
end;

procedure TInspector.Rebuild;
var
  LNew: INyxCard;
  LItems: INyxSelect;
  LControls: INyxSelect;
  LSwitch: INyxSwitch;
  LActions: INyxRow;
  LPorts: INyxToolControlPorts;
  LRows: INyxColumn;
  LRow: INyxCard;
  LPort: TNyxToolControlPort;
  LSnapshot: INyxCanvasSnapshot;
  LContent: INyxNode;
  LRuntimeRoot: TNyxNode;
  LContext: TNyxDocument;
  LAuthoredRoot: TNyxNode;
  LNode: TNyxNode;
  LBehavior: INyxViewBehavior;
  LIndex: Integer;
  LCount: Integer;
  LTotal: Integer;
  LList: TNyxText;
  LLabel: TNyxText;
  LItemChoice: TNyxText;
  LControlChoice: TNyxText;
  LStatus: TNyxText;
  LReport: TNyxDataValue;
  LConnected: Boolean;
  LContentKey: TNyxText;

  procedure Collect(ANode: TNyxNode);
  var
    LIndex: Integer;
    LCount: Integer;
  begin
    { The UI offers a bounded initial control window. Public SelectControl can
      address any exact effective identity; broader searchable paging stays open. }

    if Length(FControls) < 128 then
    begin
      LCount := Length(FControls);
      SetLength(FControls, LCount + 1);
      SetLength(FControlLabels, LCount + 1);
      FControls[LCount] := NyxControl(ANode.ID);
      FControlLabels[LCount] := IntToStr(LCount + 1) + ' / ' + Caption(ANode.ID, 80) +
        ' / ' + Caption(ANode.Kind, 32);
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Collect(ANode.Children[LIndex]);
    end;
  end;

begin
  RequireConnected;

  if FUpdating then
  begin
    raise ENyxModel.Create('Tool Canvas inspector is already updating');
  end;
  FUpdating := True;
  LRuntimeRoot := nil;
  LContext := nil;
  try

    if FProgramEditor <> nil then
    begin
      FProgramDraft.Capture(FProgramEditor.ID, FProgramEditor.Node);
    end;

    if FTab = titPrograms then
    begin
      LNew := NewNyxCard(FID);
      AddTabs(LNew);
      BuildPrograms(LNew);
      PublishForm(LNew);
      Exit;
    end;

    if FLogicEditor <> nil then
    begin
      FLogicDraft.Capture(FLogicEditor.ID, FLogicEditor.Node);
    end;

    if FTab = titLogic then
    begin
      LNew := NewNyxCard(FID);
      AddTabs(LNew);
      BuildLogic(LNew);
      PublishForm(LNew);
      Exit;
    end;

    if FTab = titEventRoutes then
    begin
      LNew := NewNyxCard(FID);
      AddTabs(LNew);
      BuildEventRoutes(LNew);
      PublishForm(LNew);
      Exit;
    end;

    if (FTab = titConnections) and (FConnections <> nil) then
    begin
      LNew := NewNyxCard(FID);
      AddTabs(LNew);
      LNew.Add(FConnections.Build);
      PublishForm(LNew);
      Exit;
    end;
    LSnapshot := FRuntime.Scene.Snapshot;
    SetLength(FItems, LSnapshot.Count);
    SetLength(FItemLabels, LSnapshot.Count);
    LItemChoice := '';
    LList := '';
    for LIndex := 0 to LSnapshot.Count - 1 do
    begin
      FItems[LIndex] := LSnapshot.Items[LIndex].Item;
      FItemLabels[LIndex] := IntToStr(LIndex + 1) + ' / ' + Caption(FItems[LIndex].Name, 80);

      if LIndex > 0 then
      begin
        LList := LList + #10;
      end;
      LList := LList + FItemLabels[LIndex];

      if FSelectedItem.Defined and (FItems[LIndex].Name = FSelectedItem.Name) then
      begin
        LItemChoice := FItemLabels[LIndex];
      end;
    end;

    if (LItemChoice = '') and (Length(FItems) > 0) then
    begin
      FSelectedItem := FItems[0];
      LItemChoice := FItemLabels[0];
    end;
    LNew := NewNyxCard(FID);
    AddTabs(LNew);
    LNew.Add(NewNyxHeading(PartID('title')).WithText('Tool ports'));
    LNew.Add(NewNyxLabel(PartID('help')).WithText(
      'Inspect real event outputs and state inputs. Graph execution is independent from static callbacks.'));
    LItems := NewNyxSelect(PartID('item')).WithText('Canvas item');
    LItems.Configure.Items(LList).Value(LItemChoice).Done;
    LNew.Add(LItems);
    FControls := nil;
    FControlLabels := nil;
    LControlChoice := '';

    if LItemChoice <> '' then
    begin
      LContent := FRuntime.Scene.Control(FSelectedItem);
      LContentKey := TNyxCodec.NodeSnapshot(LContent.Node).ToJSON;

      if (FEffectiveRoot = nil) or (FEffectiveItem.Name <> FSelectedItem.Name) or
        (FEffectiveRevision <> FRuntime.Scene.Revision) or
        (FEffectiveContent <> LContentKey) then
      begin
        LContext := NewNyxCanvasRenderContext(FSource, LContent.Node, LAuthoredRoot);
        LRuntimeRoot := RealizeNyxView(LContext, LAuthoredRoot);
        ReleaseNyxNode(FEffectiveRoot);
        FreeAndNil(FEffectiveContext);
        FEffectiveRoot := LRuntimeRoot;
        FEffectiveContext := LContext;
        FEffectiveItem := FSelectedItem;
        FEffectiveRevision := FRuntime.Scene.Revision;
        FEffectiveContent := LContentKey;
        LRuntimeRoot := nil;
        LContext := nil;
      end;
      LRuntimeRoot := FEffectiveRoot;
      LContext := FEffectiveContext;
      Collect(LRuntimeRoot);
      LNode := LRuntimeRoot.Find(FSelectedControl.ID);

      if LNode = nil then
      begin
        LNode := LRuntimeRoot;
        FSelectedControl := NyxControl(LNode.ID);
      end;
      LList := '';
      for LIndex := 0 to High(FControls) do
      begin

        if LIndex > 0 then
        begin
          LList := LList + #10;
        end;
        LList := LList + FControlLabels[LIndex];

        if FControls[LIndex].ID = FSelectedControl.ID then
        begin
          LControlChoice := FControlLabels[LIndex];
        end;
      end;
      LControls := NewNyxSelect(PartID('control')).WithText('Control');
      LControls.Configure.Items(LList).Value(LControlChoice).Done;
      LNew.Add(LControls);
      LBehavior := CurrentBehavior;
      LConnected := LBehavior <> nil;
      LSwitch := NewNyxSwitch(PartID('graphs')).WithText('Run event graphs');
      LSwitch.Configure.Enabled(LConnected).Value(LConnected and LBehavior.GraphsEnabled).Done;
      LNew.Add(LSwitch);
      LSwitch := NewNyxSwitch(PartID('ports')).WithText('Show ports');
      LSwitch.Configure.Value(FShowPorts).Done;
      LNew.Add(LSwitch);
      LActions := NewNyxRow(PartID('actions'));
      LActions.Add(NewNyxButton(PartID('refresh')).WithText('Refresh'));
      LActions.Add(NewNyxButton(PartID('cancel')).WithText('Cancel active work'));
      LNew.Add(LActions);
      LStatus := 'No runtime attachment: this item is off screen or is a design preview.';

      if LConnected then
      begin
        LCount := 0;
        LStatus := '';
        for LIndex := 0 to LBehavior.Routes.Count - 1 do
        begin
          LReport := LBehavior.Routes.Report(LIndex);
          Inc(LCount, LReport.Field('active').AsInteger);

          if LReport.Field('failure').AsText <> '' then
          begin
            LStatus := Caption(LReport.Field('failure').AsText, 256);
          end;
        end;
        LStatus := IntToStr(LBehavior.Routes.Count) + ' routes / ' + IntToStr(LCount) +
          ' active. ' + LStatus;
      end;
      LNew.Add(NewNyxLabel(PartID('status')).WithText(LStatus));
      LRows := NewNyxColumn(PartID('port-list'));
      LRows.Configure.Visible(FShowPorts).Done;
      LPorts := NyxToolControlPorts(FSelectedItem, LNode, LContext);
      LTotal := 0;
      LCount := 0;
      for LIndex := 0 to LPorts.Count - 1 do
      begin

        if not LPorts.Matches(LIndex, FQuery) then
        begin
          Continue;
        end;

        if (LTotal >= FPortOffset) and (LCount < 12) then
        begin
          LPort := LPorts.Port(LIndex);
          LRow := NewNyxCard(PartID('port-' + IntToStr(LCount)));
          LLabel := 'Output';

          if LPort.Direction = nfdInput then
          begin
            LLabel := 'Input';
          end;
          LRow.Add(NewNyxBadge(PartID('type-' + IntToStr(LCount))).WithText(
            LLabel + ' / ' + NyxFlowTypeName(LPort.ValueType)));
          LRow.Add(NewNyxLabel(PartID('name-' + IntToStr(LCount))).WithText(LPort.Title));
          LRow.Add(NewNyxLabel(PartID('meaning-' + IntToStr(LCount))).WithText(LPort.Description));
          LRows.Add(LRow);
          Inc(LCount);
        end;
        Inc(LTotal);
      end;
      LNew.Add(NewNyxInput(PartID('query')).WithText('Find a port'));
      LNew.Node.Find(PartID('query')).Configure.Value(FQuery).Done;
      LNew.Add(NewNyxButton(PartID('search')).WithText('Search ports'));
      LNew.Add(LRows);
      LActions := NewNyxRow(PartID('pages'));
      LActions.Add(NewNyxButton(PartID('previous')).WithText('Previous'));
      LActions.Node.Find(PartID('previous')).Configure.Enabled(FPortOffset > 0).Done;
      LActions.Add(NewNyxButton(PartID('next')).WithText('Next'));
      LActions.Node.Find(PartID('next')).Configure.Enabled(FPortOffset + 12 < LTotal).Done;
      LNew.Add(LActions);
      LNew.Add(NewNyxLabel(PartID('count')).WithText(
        IntToStr(LCount) + ' shown / ' + IntToStr(LTotal) + ' matching ports'));
    end;

    { Replace only the inspector's private root; all source templates/defaults
      and mounted controls remain independent. Retained old forms remain valid. }

    PublishForm(LNew);
  finally
    if LRuntimeRoot <> FEffectiveRoot then
    begin
      ReleaseNyxNode(LRuntimeRoot);
    end;
    if LContext <> FEffectiveContext then
    begin
      LContext.Free;
    end;
    FUpdating := False;
  end;
end;

function TInspector.HandleBase(ANode: TNyxNode; const AEvent: TNyxEventInfo): Boolean;
var
  LLease: INyxToolCanvasInspector;
  LIndex: Integer;
  LValue: TNyxText;
  LBehavior: INyxViewBehavior;
begin
  RequireConnected;
  LLease := Self;
  Result := False;

  if ANode = nil then
  begin
    Exit;
  end;

  if (AEvent.Trigger = ntClick) and (ANode.ID = PartID('canvas-connectors')) then
  begin
    try
      ShowControlConnectors(not FControlConnectorsVisible);
    except
      on E: Exception do
      begin
        FControlConnectorFeedback := E.Message;
        Rebuild;
      end;
    end;
    Exit(True);
  end;

  if (AEvent.Trigger = ntClick) and (ANode.ID = PartID('tab-programs')) then
  begin
    ShowPrograms;
    Exit(True);
  end;

  if (AEvent.Trigger = ntClick) and (ANode.ID = PartID('tab-logic')) then
  begin
    ShowLogic;
    Exit(True);
  end;

  if (AEvent.Trigger = ntClick) and
    (ANode.ID = PartID('tab-event-routes')) then
  begin
    ShowEventRoutes;
    Exit(True);
  end;

  if (AEvent.Trigger = ntClick) and
    ((ANode.ID = PartID('tab-ports')) or (ANode.ID = PartID('tab-connections'))) then
  begin
    ShowConnections(ANode.ID = PartID('tab-connections'));
    Exit(True);
  end;

  if (FConnections <> nil) and FConnections.Handle(ANode, AEvent) then
  begin
    Rebuild;
    Exit(True);
  end;
  LValue := ANode.Prop(NyxAttributeName(atValue));

  if (ANode.ID = PartID('item')) and (AEvent.Trigger = ntChange) then
  begin
    for LIndex := 0 to High(FItems) do
    begin

      if FItemLabels[LIndex] = LValue then
      begin
        FSelectedItem := FItems[LIndex];
        FSelectedControl := NyxControl('');
        FPortOffset := 0;
        Result := True;
        Break;
      end;
    end;
  end
  else if (ANode.ID = PartID('control')) and (AEvent.Trigger = ntChange) then
  begin
    for LIndex := 0 to High(FControls) do
    begin

      if FControlLabels[LIndex] = LValue then
      begin
        FSelectedControl := FControls[LIndex];
        FPortOffset := 0;
        Result := True;
        Break;
      end;
    end;
  end
  else if AEvent.Trigger = ntChange then
  begin

    if (ANode.ID = PartID('graphs')) and AEvent.HasValue and
      (AEvent.ValueKind = nskBoolean) then
    begin
      LBehavior := CurrentBehavior;

      if LBehavior <> nil then
      begin
        LBehavior.EnableGraphs(AEvent.Value.AsBoolean);
      end;
      Result := True;
    end
    else if (ANode.ID = PartID('ports')) and AEvent.HasValue and
      (AEvent.ValueKind = nskBoolean) then
    begin
      FShowPorts := AEvent.Value.AsBoolean;
      Result := True;
    end;
  end
  else if AEvent.Trigger = ntClick then
  begin
    Result := True;

    if ANode.ID = PartID('previous') then
    begin
      Dec(FPortOffset, 12);

      if FPortOffset < 0 then
      begin
        FPortOffset := 0;
      end;
    end
    else if ANode.ID = PartID('next') then
    begin
      Inc(FPortOffset, 12);
    end
    else if ANode.ID = PartID('search') then
    begin
      FQuery := ANode.Parent.Find(PartID('query')).Prop(NyxAttributeName(atValue));
      FPortOffset := 0;
    end
    else if ANode.ID = PartID('cancel') then
    begin
      LBehavior := CurrentBehavior;

      if LBehavior <> nil then
      begin
        for LIndex := 0 to LBehavior.Routes.Count - 1 do
        begin
          LBehavior.Routes.Cancel(LIndex);
        end;
      end;
    end
    else if ANode.ID <> PartID('refresh') then
    begin
      Result := False;
    end;
  end;

  if Result then
  begin
    Rebuild;
  end;
end;

procedure TInspector.SelectControl(const AItem: TNyxCanvasItemRef;
  const AControl: TNyxControlRef);
var
  LContent: INyxNode;
  LContext: TNyxDocument;
  LAuthored: TNyxNode;
  LRuntime: TNyxNode;
begin
  RequireConnected;
  LContent := FRuntime.Scene.Control(AItem);
  LContext := nil;
  LRuntime := nil;
  try
    if (FEffectiveRoot <> nil) and (FEffectiveItem.Name = AItem.Name) and
      (FEffectiveRevision = FRuntime.Scene.Revision) and
      (FEffectiveContent = TNyxCodec.NodeSnapshot(LContent.Node).ToJSON) then
    begin
      LRuntime := FEffectiveRoot;
    end
    else
    begin
      LContext := NewNyxCanvasRenderContext(FSource, LContent.Node, LAuthored);
      LRuntime := RealizeNyxView(LContext, LAuthored);
    end;

    if LRuntime.Find(AControl.ID) = nil then
    begin
      raise ENyxModel.Create('Tool Canvas selection requires an exact effective control');
    end;
  finally
    if LRuntime <> FEffectiveRoot then
    begin
      ReleaseNyxNode(LRuntime);
    end;
    LContext.Free;
  end;
  FSelectedItem := AItem;
  FSelectedControl := AControl;
  FPortOffset := 0;
  Rebuild;
end;

function NewNyxToolCanvasInspector(const AID: TNyxText; ADocument: TNyxDocument;
  const AView: INyxCanvasWorkspaceView): INyxToolCanvasInspector;
begin
  Result := TInspector.Create(AID, ADocument, AView);
end;

function TInspector.ConnectionEditor: INyxToolConnectionEditor;
begin
  RequireConnected;
  Result := FConnections;
end;

procedure TInspector.AddTabs(const AParent: INyxCard);
var
  LTabs: INyxRow;
  LButton: INyxButton;
begin
  LTabs := NewNyxRow(PartID('tabs'));
  LTabs.Configure.Wrap(nfwWrap).Gap(8).Done;
  LButton := NewNyxButton(PartID('tab-ports')).WithText('Control ports');

  if FTab = titPorts then
  begin
    LButton.Configure.Variant(nvPrimary).Done;
  end;
  LTabs.Add(LButton);
  LButton := NewNyxButton(PartID('tab-connections')).WithText('Connections');
  LButton.Configure.Enabled(FConnections <> nil).Done;

  if FTab = titConnections then
  begin
    LButton.Configure.Variant(nvPrimary).Done;
  end;
  LTabs.Add(LButton);
  LButton := NewNyxButton(PartID('tab-event-routes')).WithText('Event routes');
  LButton.Configure.Enabled(FRouteEditing <> nil).Done;

  if FTab = titEventRoutes then
  begin
    LButton.Configure.Variant(nvPrimary).Done;
  end;
  LTabs.Add(LButton);
  LButton := NewNyxButton(PartID('tab-logic')).WithText('Logic');
  LButton.Configure.Enabled(FFlowEditing <> nil).Done;

  if FTab = titLogic then
  begin
    LButton.Configure.Variant(nvPrimary).Done;
  end;
  LTabs.Add(LButton);
  AParent.Add(LTabs);
  LButton := NewNyxButton(PartID('tab-programs')).WithText('Pascal programs');
  LButton.Configure.Enabled(FProgramEditing <> nil).Done;

  if FTab = titPrograms then
  begin
    LButton.Configure.Variant(nvPrimary).Done;
  end;
  LTabs.Add(LButton);
  LButton := NewNyxButton(PartID('canvas-connectors')).WithText('Canvas connections');
  LButton.Configure.Enabled(FRouteEditing <> nil).Done;

  if FControlConnectorsVisible then
  begin
    LButton.Configure.Variant(nvPrimary).Done;
  end;
  AParent.Add(LButton);

  if FAdoptionHost <> nil then
  begin
    LButton := NewNyxButton(PartID('save-connections')).WithText('Save graphs and connections');
    LButton.Configure.Enabled(not GetAdoptionPending).Done;
    AParent.Add(LButton);
    AParent.Add(NewNyxLabel(PartID('save-connections-feedback')).WithText(FAdoptionFeedback));
  end;

  if FControlConnectorFeedback <> '' then
  begin
    AParent.Add(NewNyxLabel(PartID('canvas-connectors-feedback')).WithText(FControlConnectorFeedback));
  end;
end;

procedure TInspector.PublishForm(const ARoot: INyxCard);
begin

  if FRoot <> nil then
  begin
    FDocument.RemoveRoot(NyxReusableRoot(FID));
  end;
  FDocument.AddComponent(ARoot);
  FRoot := ARoot;
end;

procedure TInspector.ShowConnections(AVisible: Boolean);
begin
  RequireConnected;

  if AVisible and (FConnections = nil) then
  begin
    raise ENyxModel.Create('This canvas supports port inspection without connection editing');
  end;
  FTab := titPorts;

  if AVisible then
  begin
    FTab := titConnections;
  end;
  Rebuild;
end;

function TInspector.HandleGesture(ANode: TNyxNode; const AEvent: TNyxEventInfo;
  const AResponse: INyxGestureResponse): Boolean;
var
  LLease: INyxToolCanvasInspector;
begin
  RequireConnected;
  LLease := Self;
  Result := (FConnections <> nil) and FConnections.Handle(ANode, AEvent, AResponse);

  if Result then
  begin
    Rebuild;
  end;
end;

end.
