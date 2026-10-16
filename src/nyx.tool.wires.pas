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

unit nyx.tool.wires;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.behavior, nyx.events, nyx.gestures, nyx.canvas.types, nyx.canvas.workspace,
  nyx.canvas.routes, nyx.tool.ports, nyx.flow.types, nyx.flow.declarations,
  nyx.flow.routes, nyx.scheduler, nyx.flow.editing;

type
  { Detached view of the Inspector's single Logic proposal. Exact graph bytes,
    runtime revisions and proposal version fence later edits; none is executable
    authority. FieldsPending includes unfinished nested form text. Callers retain
    only inert values, never the Inspector's document or a mutable definition. }
  TNyxToolGraphFrame = record
    Flow: TNyxFlowRef;
    Definition: TNyxDataValue;
    SceneRevision: Integer;
    FlowRevision: Integer;
    ProposalVersion: Integer;
    Pending: Boolean;
    FieldsPending: Boolean;
    Stale: Boolean;
  end;

  { Explicit UI-only lease onto the existing Logic proposal owner. Open/Reload
    guard discarding topology and fields. Read is bounded to one open graph;
    Stage checks the exact frame and applies a typed group without executing it.
    Apply uses ordinary prepared publication. Retirement revokes the receiver,
    so retaining this interface never keeps the Inspector alive. }
  INyxToolGraphHost = interface(IInterface)
    ['{8D1EBC25-A5D3-4945-A5B2-1A1016000014}']
    function OpenGraph(const AFlow: TNyxFlowRef;
      ADiscardDraft: Boolean = False): TNyxToolGraphFrame;
    function ReadGraph(const AFlow: TNyxFlowRef): TNyxToolGraphFrame;
    { Constant-size freshness probe; -1 means a different/stale open context.
      This avoids serializing the whole graph for every displayed port. }
    function GraphVersion(const AFlow: TNyxFlowRef): Integer;
    function StageGraph(const AExpected: TNyxToolGraphFrame;
      const AChanges: array of TNyxFlowChange): TNyxToolGraphFrame;
    function ApplyGraph(const AExpected: TNyxToolGraphFrame): TNyxToolGraphFrame;
    function ReloadGraph(const AFlow: TNyxFlowRef;
      ADiscardDraft: Boolean = False): TNyxToolGraphFrame;
  end;

  { Optional shared-proposal editing for operation cards. Route mappings and
    internal graph edits have explicit modes; entering refuses route drafts.
    Complete graph ports reuse the ordinary controller input/gesture contract.
    In this mode PortCount/Port address all graph ports, Connect stages internal
    wires, and ControlCount/WireCount return zero. Dirty remains route-only.
    Leaving retains the shared proposal in Logic; only Reload may discard it.
    Applied graphs join the existing explicit paired project save. }
  INyxToolPipelineGraphEditing = interface(IInterface)
    ['{8D1EBC25-A5D3-4945-A5B2-1A1016000015}']
    procedure ConfigureGraphHost(const AHost: INyxToolGraphHost);
    procedure BeginGraphEditing;
    procedure EndGraphEditing;
    function GetGraphEditing: Boolean;
    procedure ApplyGraph;
    procedure RemoveGraphWire(AIndex: Integer; AConfirmed: Boolean = False);
    property GraphEditing: Boolean read GetGraphEditing;
  end;

  { Essential ports keep ordinary leaf-control Change values, Click signals,
    named creator events and real state inputs visible. All exposes the complete
    existing port metadata. Neither choice fabricates writable projections. }
  TNyxToolWireDiscovery = (ntwdEssential, ntwdAll);

  { Copied creator-tunable discovery limits. Limits bound metadata and Nyx face
    creation, independently of runtime route budgets. Overflow refuses the whole
    snapshot; no silently truncated list can manufacture a missing endpoint. }
  TNyxToolWireOptions = record
  private
    FControls: Integer;
    FPorts: Integer;
    FDiscovery: TNyxToolWireDiscovery;
    FPolicy: TNyxExecutionPolicy;
    FCallbacks: TNyxFlowCallbackMode;
    FConcurrency: TNyxFlowConcurrency;
    FRuns: Integer;
  public
    class function Defaults: TNyxToolWireOptions; static;
    function Controls(AValue: Integer): TNyxToolWireOptions;
    function Ports(AValue: Integer): TNyxToolWireOptions;
    function Discover(AValue: TNyxToolWireDiscovery): TNyxToolWireOptions;
    function Using(AValue: TNyxExecutionPolicy): TNyxToolWireOptions;
    function Callbacks(AValue: TNyxFlowCallbackMode): TNyxToolWireOptions;
    function Concurrent(AValue: TNyxFlowConcurrency): TNyxToolWireOptions;
    function MaximumRuns(AValue: Integer): TNyxToolWireOptions;
    procedure Validate;
    property MaximumControls: Integer read FControls;
    property MaximumPorts: Integer read FPorts;
    property Discovery: TNyxToolWireDiscovery read FDiscovery;
    property ExecutionPolicy: TNyxExecutionPolicy read FPolicy;
    property CallbackMode: TNyxFlowCallbackMode read FCallbacks;
    property Concurrency: TNyxFlowConcurrency read FConcurrency;
    property RunLimit: Integer read FRuns;
  end;

  { Closed UI endpoint origin. Control ports and pipeline endpoints retain
    distinct identities; a string caption is never used to resolve either. }
  TNyxToolWirePortKind = (ntwpkControl, ntwpkPipeline);

  { Detached address from one revision-pinned controller snapshot. ControlPort
    belongs only to ntwpkControl; Endpoint belongs only to ntwpkPipeline. This
    record retains no interface, document, runtime, widget or host authority. }
  TNyxToolWirePort = record
    Kind: TNyxToolWirePortKind;
    ControlPort: TNyxToolControlPort;
    Endpoint: TNyxFlowEndpoint;
    Direction: TNyxFlowDirection;
    ValueType: TNyxFlowType;
    Title: TNyxText;
    Description: TNyxText;
  end;

  { Detached staged edge indexes address this controller's current port window.
    They cannot survive a reload as authority; scene/route revisions fence Apply. }
  TNyxToolWire = record
    Source: Integer;
    Target: Integer;
  end;

  { Copied full pipeline port metadata. RoutePort addresses the active controller
    window, or -1 for an internally supplied input in control-route mode. The
    distinct display identity permits that connected input to remain visible
    without granting a second route source. No runtime authority is retained. }
  TNyxToolPipelinePort = record
    Endpoint: TNyxFlowEndpoint;
    Metadata: TNyxFlowPort;
    DisplayControl: TNyxControlRef;
    RoutePort: Integer;
  end;

  { Copied internal graph connection. Both endpoints retain their typed step
    and port identities; rendering it never installs or changes behavior. }
  TNyxToolPipelineWire = record
    Source: TNyxFlowEndpoint;
    Target: TNyxFlowEndpoint;
  end;

  { Optional inert per-step presentation of the same captured pipeline. Public
    Nyx cards contain copied metadata and the existing route-port button IDs,
    never a controller backreference. Internally supplied inputs are disabled in
    route mode and selectable in shared Logic graph mode. ValidateFaces
    checks the whole metadata budget before a target expands the panel. All calls
    use the controller's UI lifetime; retained cards survive retirement as data. }
  INyxToolPipelineNodes = interface(IInterface)
    ['{8D1EBC25-A5D3-4945-A5B2-1A1016000010}']
    procedure ValidateFaces(AMaximumNodes, AMaximumPorts: Integer);
    function GetNodeFlow: TNyxFlowRef;
    function GetNodeCount: Integer;
    function NodeStep(AIndex: Integer): TNyxFlowStepRef;
    function NodeDescriptor(AIndex: Integer): TNyxLogicDescriptor;
    function NodeControl(AIndex: Integer): TNyxControlRef;
    function NodePort(AIndex, APort: Integer): TNyxToolPipelinePort;
    function GetNodeWireCount: Integer;
    function NodeWire(AIndex: Integer): TNyxToolPipelineWire;
    function ModeControl: TNyxControlRef;
    function BuildNode(AIndex: Integer): INyxCard;
    function BuildNodeToolbar: INyxCard;
    property NodeFlow: TNyxFlowRef read GetNodeFlow;
    property NodeCount: Integer read GetNodeCount;
    property NodeWireCount: Integer read GetNodeWireCount;
  end;

  { Managed UI-only control/pipeline connector controller. It owns one inert
    item context and copied metadata, retaining only the canvas's revocable view.
    Connect stages typed mappings; Apply/Remove use INyxCanvasRouteEditing's one
    whole-group admission and never mutate authored source/history. Returned
    ordinary Nyx forms are independently owned and contain no reverse receiver.
    Disconnect precedes borrowed application/host retirement. All calls belong
    to UI. Stale contexts retain their unfinished proposal until explicit discard. }
  INyxToolControlWires = interface(IInterface)
    ['{91F83BBF-4A91-4164-A8E0-5C411966C702}']
    { Idempotently revoke runtime access; retained metadata grants no late use. }
    procedure Disconnect;
    { Atomically recapture current metadata. Dirty intent requires explicit
      discard; discovery failure preserves the complete previous snapshot. }
    procedure Reload(ADiscardDraft: Boolean = False);
    { Choose an existing pipeline with the same draft/discovery guards. }
    procedure SelectFlow(const AFlow: TNyxFlowRef; ADiscardDraft: Boolean = False);
    function GetItem: TNyxCanvasItemRef;
    function GetControlCount: Integer;
    function Control(AIndex: Integer): TNyxControlRef;
    function GetPortCount: Integer;
    function Port(AIndex: Integer): TNyxToolWirePort;
    { Stable face-button identity within this snapshot, never a widget handle. }
    function PortControl(AIndex: Integer): TNyxControlRef;
    function GetWireCount: Integer;
    function Wire(AIndex: Integer): TNyxToolWire;
    function GetDirty: Boolean;
    function GetStale: Boolean;
    { Stage one exact output/input pair. Occupied targets require replacement
      acknowledgement; identical pairs are no-ops. This executes no graph. }
    procedure Connect(ASource, ATarget: Integer; AReplace: Boolean = False);
    { Use the form's acknowledgement and retain a visible failure diagnostic. }
    procedure StageSelected(ASource, ATarget: Integer);
    { Change only the local source/target selection, clearing acknowledgement. }
    procedure SelectPort(AIndex: Integer);
    { Warned removal affects staged intent only, until a later Apply. }
    procedure Remove(AIndex: Integer; AConfirmed: Boolean = False);
    { Publish a complete route, preserving unrelated routes. A reused name
      requires acknowledgement; stale or invalid candidates publish nothing. }
    procedure Apply(const AID: TNyxFlowRouteRef; AReplace: Boolean = False);
    { Remove only this controller's last applied route, with acknowledgement.
      Reload drops that removal authority; ordinary callbacks stay enabled. }
    procedure RemoveApplied(AConfirmed: Boolean = False);
    { Independently owned ordinary Nyx roots; they retain no controller/view.
      Control addresses must belong to this captured discovery window. }
    function BuildControl(const AControl: TNyxControlRef): INyxRow;
    function BuildPipeline: INyxCard;
    { Accept actual form nodes at the current snapshot epoch. True requests a
      face rebuild. Accepted local text/toggles return False to retain editing;
      False therefore does not imply that no local input was captured. }
    function Handle(ANode: TNyxNode; const AEvent: TNyxEventInfo;
      const AResponse: INyxGestureResponse = nil): Boolean;
    property Item: TNyxCanvasItemRef read GetItem;
    property ControlCount: Integer read GetControlCount;
    property PortCount: Integer read GetPortCount;
    property WireCount: Integer read GetWireCount;
    property Dirty: Boolean read GetDirty;
    property Stale: Boolean read GetStale;
  end;

{ One exact runtime item supplies the authored context. No application operation
  or creator resolver runs during discovery. Options must be deliberately defined.
  AID contains 1..80 Unicode scalars, reserving room for owned face/part suffixes. }
function NewNyxToolControlWires(const AID: TNyxText;
  const AView: INyxCanvasWorkspaceView; const AItem: TNyxCanvasItemRef;
  const AOptions: TNyxToolWireOptions): INyxToolControlWires;

implementation

uses nyx.composition, nyx.canvas.scene, nyx.flow.route.model,
  nyx.state, nyx.resources, nyx.publication, nyx.logic.catalog;

const
  CWireForm = 'nyx.tool-control-wires';
  CWireTransfer = 'application/x-nyx-control-wire';

type
  { Private snapshot/unfinished mapping owner. Metadata never reconstructs host
    implementations. Target faces pass actual owned event nodes only during a
    call; no face or control is retained by this controller. }
  TControlWires = class(TInterfacedObject, INyxToolControlWires, INyxToolPipelineNodes,
    INyxToolPipelineGraphEditing)
  private
    FID: TNyxText;
    FView: INyxCanvasWorkspaceView;
    FRuntime: INyxCanvasRuntimeView;
    FEditing: INyxCanvasRouteEditing;
    FFlowEditing: INyxCanvasFlowEditing;
    FContext: TNyxDocument;
    FRoot: TNyxNode;
    FOptions: TNyxToolWireOptions;
    FItem: TNyxCanvasItemRef;
    FFlow: TNyxFlowRef;
    FDefinition: INyxFlowDefinition;
    FNodeWires: TNyxDataValue;
    FControls: array of TNyxControlRef;
    FPorts: array of TNyxToolWirePort;
    FWires: array of TNyxToolWire;
    FScene: Integer;
    FRevision: Integer;
    FEpoch: Integer;
    FDirty: Boolean;
    FSource: Integer;
    FTarget: Integer;
    FSelectedWire: Integer;
    FConfirmed: Boolean;
    FDrag: Boolean;
    FDragSource: Integer;
    FDragToken: TNyxText;
    FMessage: TNyxText;
    FName: TNyxText;
    FAppliedID: TNyxFlowRouteRef;
    FAppliedOwner: TNyxControlRef;
    FOptionsExpanded: Boolean;
    FGraphHost: INyxToolGraphHost;
    FGraphMode: Boolean;
    FGraphFrame: TNyxToolGraphFrame;
    FGraphDefinition: INyxFlowDefinition;
    FGraphPorts: array of TNyxToolWirePort;
    { Prefix offsets make full graph port lookup linear to construct and constant
      per displayed button, rather than scanning every endpoint for every face. }
    FGraphOffsets: array of Integer;
    procedure AcceptGraphFrame(const AFrame: TNyxToolGraphFrame);
    procedure RequireGraphCurrent;
    procedure ConnectGraph(ASource, ATarget: Integer; AReplace: Boolean);
    function BuildGraph: INyxCard;
    function HandleGraph(ANode: TNyxNode; const AEvent: TNyxEventInfo): Boolean;
    function NodeDefinition: INyxFlowDefinition;
    procedure RequireActive;
    procedure RequireCurrent;
    procedure Snapshot(const ARequestedFlow: TNyxFlowRef);
    procedure CheckPort(AIndex: Integer);
    function DraftRoute(const AID: TNyxFlowRouteRef; out AOwner: TNyxControlRef): TNyxFlowRoute;
    function Publish(const AOwner: TNyxControlRef; const ARoutes: INyxFlowRoutes): TNyxText;
    procedure CheckNode(AIndex: Integer);
    function PortID(AIndex: Integer): TNyxText;
    function FormFor(ANode: TNyxNode): TNyxNode;
  public
    constructor Create(const AID: TNyxText; const AView: INyxCanvasWorkspaceView;
      const AItem: TNyxCanvasItemRef; const AOptions: TNyxToolWireOptions);
    destructor Destroy; override;
    procedure Disconnect;
    procedure Reload(ADiscardDraft: Boolean);
    procedure SelectFlow(const AFlow: TNyxFlowRef; ADiscardDraft: Boolean);
    function GetItem: TNyxCanvasItemRef;
    function GetControlCount: Integer;
    function Control(AIndex: Integer): TNyxControlRef;
    function GetPortCount: Integer;
    function Port(AIndex: Integer): TNyxToolWirePort;
    function PortControl(AIndex: Integer): TNyxControlRef;
    function GetWireCount: Integer;
    function Wire(AIndex: Integer): TNyxToolWire;
    function GetDirty: Boolean;
    function GetStale: Boolean;
    procedure Connect(ASource, ATarget: Integer; AReplace: Boolean);
    procedure StageSelected(ASource, ATarget: Integer);
    procedure SelectPort(AIndex: Integer);
    procedure Remove(AIndex: Integer; AConfirmed: Boolean);
    procedure Apply(const AID: TNyxFlowRouteRef; AReplace: Boolean);
    procedure RemoveApplied(AConfirmed: Boolean);
    function BuildControl(const AControl: TNyxControlRef): INyxRow;
    function BuildPipeline: INyxCard;
    function Handle(ANode: TNyxNode; const AEvent: TNyxEventInfo;
      const AResponse: INyxGestureResponse): Boolean;
    procedure ValidateFaces(AMaximumNodes, AMaximumPorts: Integer);
    function GetNodeFlow: TNyxFlowRef;
    function GetNodeCount: Integer;
    function NodeStep(AIndex: Integer): TNyxFlowStepRef;
    function NodeDescriptor(AIndex: Integer): TNyxLogicDescriptor;
    function NodeControl(AIndex: Integer): TNyxControlRef;
    function NodePort(AIndex, APort: Integer): TNyxToolPipelinePort;
    function GetNodeWireCount: Integer;
    function NodeWire(AIndex: Integer): TNyxToolPipelineWire;
    function ModeControl: TNyxControlRef;
    function BuildNode(AIndex: Integer): INyxCard;
    function BuildNodeToolbar: INyxCard;
    procedure ConfigureGraphHost(const AHost: INyxToolGraphHost);
    procedure BeginGraphEditing;
    procedure EndGraphEditing;
    function GetGraphEditing: Boolean;
    procedure ApplyGraph;
    procedure RemoveGraphWire(AIndex: Integer; AConfirmed: Boolean);
  end;

var
  { UI-thread process-local transfer identity. It carries no execution grant and
    must match a live local drag plus the captured scene/route revision. }
  GWireSerial: Integer;

class function TNyxToolWireOptions.Defaults: TNyxToolWireOptions;
begin
  Result.FControls := 128;
  Result.FPorts := 4096;
  Result.FDiscovery := ntwdEssential;
  Result.FPolicy := neUIQueue;
  Result.FCallbacks := nfcmAfterDispatch;
  Result.FConcurrency := nfrLatest;
  Result.FRuns := 8;
end;

function TNyxToolWireOptions.Controls(AValue: Integer): TNyxToolWireOptions;
begin
  Result := Self;
  Result.FControls := AValue;
  Result.Validate;
end;

function TNyxToolWireOptions.Ports(AValue: Integer): TNyxToolWireOptions;
begin
  Result := Self;
  Result.FPorts := AValue;
  Result.Validate;
end;

function TNyxToolWireOptions.Discover(AValue: TNyxToolWireDiscovery): TNyxToolWireOptions;
begin
  Result := Self;
  Result.FDiscovery := AValue;
  Result.Validate;
end;

function TNyxToolWireOptions.Using(AValue: TNyxExecutionPolicy): TNyxToolWireOptions;
begin
  Result := Self;
  Result.FPolicy := AValue;
  Result.Validate;
end;

function TNyxToolWireOptions.Callbacks(AValue: TNyxFlowCallbackMode): TNyxToolWireOptions;
begin
  Result := Self;
  Result.FCallbacks := AValue;
  Result.Validate;
end;

function TNyxToolWireOptions.Concurrent(AValue: TNyxFlowConcurrency): TNyxToolWireOptions;
begin
  Result := Self;
  Result.FConcurrency := AValue;
  Result.Validate;
end;

function TNyxToolWireOptions.MaximumRuns(AValue: Integer): TNyxToolWireOptions;
begin
  Result := Self;
  Result.FRuns := AValue;
  Result.Validate;
end;

procedure TNyxToolWireOptions.Validate;
begin

  if (FControls < 1) or (FControls > 10000) or (FPorts < 1) or (FPorts > 65536) or
    not (FDiscovery in [ntwdEssential, ntwdAll]) or
    not (FPolicy in [neSequential, neAsynchronous, neThreaded, neUIQueue]) or
    not (FCallbacks in [nfcmExclusive, nfcmBeforeCallbacks, nfcmAfterDispatch]) or
    not (FConcurrency in [nfrLatest, nfrEvery, nfrIgnoreWhileRunning]) or
    (FRuns < 1) or (FRuns > 64) then
  begin
    raise ENyxFlow.Create('Connector options require bounded discovery, known policies and 1..64 runs');
  end;
end;

constructor TControlWires.Create(const AID: TNyxText;
  const AView: INyxCanvasWorkspaceView; const AItem: TNyxCanvasItemRef;
  const AOptions: TNyxToolWireOptions);
var
  LRuntime: INyxCanvasRuntimeView;
  LEditing: INyxCanvasRouteEditing;
  LFlowEditing: INyxCanvasFlowEditing;
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  inherited Create;
  AOptions.Validate;
  LIndex := 1;
  LCount := 0;
  while LIndex <= Length(AID) do
  begin

    if not NyxNextScalar(AID, LIndex, LScalar) then
    begin
      raise ENyxFlow.Create('Connector identity requires valid Unicode');
    end;
    Inc(LCount);
  end;

  if (LCount < 1) or (LCount > 80) or not AItem.Defined or (AView = nil) or
    not Supports(AView, INyxCanvasRuntimeView, LRuntime) or
    not Supports(AView, INyxCanvasRouteEditing, LEditing) or
    not Supports(AView, INyxCanvasFlowEditing, LFlowEditing) then
  begin
    raise ENyxFlow.Create('Control wires require an exact item and editable runtime canvas');
  end;
  FID := AID;
  FView := AView;
  FRuntime := LRuntime;
  FEditing := LEditing;
  FFlowEditing := LFlowEditing;
  FItem := AItem;
  FOptions := AOptions;

  if GWireSerial = High(Integer) then
  begin
    raise ENyxFlow.Create('Control wire session identity capacity exhausted');
  end;
  Inc(GWireSerial);
  FDragToken := TNyxText(IntToStr(GWireSerial));
  FSource := -1;
  FTarget := -1;
  FSelectedWire := -1;
  FName := 'Canvas connection';
  Snapshot(FFlow);
end;

destructor TControlWires.Destroy;
begin
  Disconnect;
  FContext.Free;
  inherited Destroy;
end;

procedure TControlWires.Disconnect;
begin
  FDrag := False;
  FGraphHost := nil;
  FView := nil;
  FRuntime := nil;
  FEditing := nil;
  FFlowEditing := nil;
end;

procedure TControlWires.RequireActive;
begin

  if (FView = nil) or not FView.Connected then
  begin
    raise ENyxFlow.Create('Control wire owner is retired');
  end;
  FEditing.GetRouteRevision;
end;

procedure TControlWires.RequireCurrent;
begin
  RequireActive;

  if GetStale then
  begin
    raise ENyxFlow.Create('This control wire proposal is stale; explicitly discard and reload');
  end;
end;

function TControlWires.GetStale: Boolean;
begin
  RequireActive;
  Result := (FRuntime.Scene.Revision <> FScene) or (FEditing.RouteRevision <> FRevision);

  if FGraphMode then
  begin
    try
      Result := FGraphHost.GraphVersion(FFlow) <> FGraphFrame.ProposalVersion;
    except
      on E: ENyxFlow do
      begin
        Result := True;
      end;
    end;
  end;
end;

function TControlWires.GetItem: TNyxCanvasItemRef;
begin
  RequireActive;
  Result := FItem;
end;

function TControlWires.GetControlCount: Integer;
begin
  RequireActive;
  Result := Length(FControls);

  if FGraphMode then
  begin
    Result := 0;
  end;
end;

function TControlWires.Control(AIndex: Integer): TNyxControlRef;
begin
  RequireActive;

  if (AIndex < 0) or (AIndex >= GetControlCount) then
  begin
    raise ENyxFlow.Create('Control connector index is out of bounds');
  end;
  Result := FControls[AIndex];
end;

function TControlWires.GetPortCount: Integer;
begin
  RequireActive;
  Result := Length(FPorts);

  if FGraphMode then
  begin
    Result := Length(FGraphPorts);
  end;
end;

procedure TControlWires.CheckPort(AIndex: Integer);
begin

  if (AIndex < 0) or (AIndex >= GetPortCount) then
  begin
    raise ENyxFlow.Create('Choose a port in the current connector snapshot');
  end;
end;

function TControlWires.Port(AIndex: Integer): TNyxToolWirePort;
begin
  RequireActive;
  CheckPort(AIndex);

  if FGraphMode then
  begin
    Result := FGraphPorts[AIndex];
  end
  else
  begin
    Result := FPorts[AIndex];
  end;
end;

function TControlWires.PortControl(AIndex: Integer): TNyxControlRef;
begin
  RequireActive;
  CheckPort(AIndex);
  Result := NyxControl(PortID(AIndex));

  if FGraphMode then
  begin
    Result := NyxControl(FID + '-graph-port-' + TNyxText(IntToStr(AIndex)));
  end;
end;

procedure TControlWires.StageSelected(ASource, ATarget: Integer);
begin
  try
    Connect(ASource, ATarget, FConfirmed);
  except
    on E: Exception do
    begin
      FMessage := E.Message;
      raise;
    end;
  end;
  FSource := ASource;
  FTarget := ATarget;
end;

procedure TControlWires.SelectPort(AIndex: Integer);
begin
  RequireCurrent;
  CheckPort(AIndex);

  if Port(AIndex).Direction = nfdOutput then
  begin
    FSource := AIndex;
  end
  else
  begin
    FTarget := AIndex;
  end;
  FConfirmed := False;
end;

function TControlWires.GetWireCount: Integer;
begin
  RequireActive;
  Result := Length(FWires);

  if FGraphMode then
  begin
    Result := 0;
  end;
end;

function TControlWires.Wire(AIndex: Integer): TNyxToolWire;
begin
  RequireActive;

  if (AIndex < 0) or (AIndex >= GetWireCount) then
  begin
    raise ENyxFlow.Create('Choose a staged wire in this snapshot');
  end;
  Result := FWires[AIndex];
end;

function TControlWires.GetDirty: Boolean;
begin
  RequireActive;
  Result := FDirty;
end;

procedure TControlWires.Reload(ADiscardDraft: Boolean);
var
  LLease: INyxToolControlWires;
begin
  RequireActive;
  LLease := Self;

  if FGraphMode then
  begin
    AcceptGraphFrame(FGraphHost.ReloadGraph(FFlow, ADiscardDraft));
    Exit;
  end;

  if FDirty and not ADiscardDraft then
  begin
    raise ENyxFlow.Create('Acknowledge discarding the unfinished control wires');
  end;
  Snapshot(FFlow);
end;

procedure TControlWires.SelectFlow(const AFlow: TNyxFlowRef; ADiscardDraft: Boolean);
var
  LFlows: INyxFlowDeclarations;
begin
  RequireCurrent;

  if FGraphMode then
  begin
    raise ENyxFlow.Create('Return to control connections before choosing another pipeline');
  end;

  if FFlow.Name = AFlow.Name then
  begin
    Exit;
  end;

  if FDirty and not ADiscardDraft then
  begin
    raise ENyxFlow.Create('Acknowledge discarding staged wires before changing the pipeline');
  end;
  LFlows := FFlowEditing.RuntimeFlows;

  if not LFlows.Contains(AFlow) then
  begin
    raise ENyxFlow.Create('Choose an existing runtime pipeline');
  end;
  Snapshot(AFlow);
end;

procedure TControlWires.Snapshot(const ARequestedFlow: TNyxFlowRef);
var
  LContext: TNyxDocument;
  LRoot: TNyxNode;
  LControls: array of TNyxControlRef;
  LPorts: array of TNyxToolWirePort;
  LFlow: TNyxFlowRef;
  LDefinition: INyxFlowDefinition;
  LFlows: INyxFlowDeclarations;
  LDescriptor: TNyxLogicDescriptor;
  LPort: TNyxToolWirePort;
  LIndex: Integer;
  LPart: Integer;
  LWires: TNyxDataValue;
  LWire: Integer;
  LTarget: TNyxFlowEndpoint;
  LConnected: Boolean;
  LScene: Integer;
  LRevision: Integer;

  procedure Add(const APort: TNyxToolWirePort);
  begin

    if Length(LPorts) = FOptions.MaximumPorts then
    begin
      raise ENyxFlow.Create('Connector port discovery exceeded the selected budget');
    end;
    SetLength(LPorts, Length(LPorts) + 1);
    LPorts[High(LPorts)] := APort;
  end;

  procedure Collect(ANode: TNyxNode);
  var
    LMetadata: INyxToolControlPorts;
    LControlPort: TNyxToolControlPort;
    LChoice: TNyxToolWirePort;
    LIndex: Integer;
    LBefore: Integer;
    LInclude: Boolean;
  begin
    LBefore := Length(LPorts);
    LMetadata := NyxToolControlPorts(FItem, ANode, LContext);
    for LIndex := 0 to LMetadata.Count - 1 do
    begin
      LControlPort := LMetadata.Port(LIndex);
      LInclude := (FOptions.Discovery = ntwdAll) or
        (LControlPort.Role = ntprBoundState) or (LControlPort.Trigger = ntNamed) or
        ((ANode.Count = 0) and
          (((LControlPort.Role = ntprEventSignal) and (LControlPort.Trigger = ntClick)) or
           ((LControlPort.Role = ntprEventValue) and (LControlPort.Trigger = ntChange))));

      if not LInclude then
      begin
        Continue;
      end;
      LChoice := Default(TNyxToolWirePort);
      LChoice.Kind := ntwpkControl;
      LChoice.ControlPort := LControlPort;
      LChoice.Direction := LControlPort.Direction;
      LChoice.ValueType := LControlPort.ValueType;
      LChoice.Title := LControlPort.Title;
      LChoice.Description := LControlPort.Description;
      Add(LChoice);
    end;

    if Length(LPorts) > LBefore then
    begin

      if Length(LControls) = FOptions.MaximumControls then
      begin
        raise ENyxFlow.Create('Connector control discovery exceeded the selected budget');
      end;
      SetLength(LControls, Length(LControls) + 1);
      LControls[High(LControls)] := NyxControl(ANode.ID);
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Collect(ANode.Children[LIndex]);
    end;
  end;

begin
  RequireActive;

  if FEpoch = High(Integer) then
  begin
    raise ENyxFlow.Create('Control connector snapshot identity capacity exhausted');
  end;
  LScene := FRuntime.Scene.Revision;
  LRevision := FEditing.RouteRevision;
  LContext := FEditing.RouteContext(FItem, LRoot);
  try
    LControls := nil;
    LPorts := nil;
    Collect(LRoot);
    LFlows := FFlowEditing.RuntimeFlows;
    LFlow := ARequestedFlow;

    if not LFlow.Defined or not LFlows.Contains(LFlow) then
    begin
      LFlow := Default(TNyxFlowRef);

      if LFlows.Count > 0 then
      begin
        LFlow := LFlows.Reference(0);
      end;
    end;
    LDefinition := nil;
    LWires := NyxArray([]);

    if LFlow.Defined then
    begin
      LDefinition := LFlows.Definition(LFlow);
      { Decode only at the graph's persistence boundary. Inputs already supplied
        by a graph edge are not writable route inputs; their public descriptor
        alone does not grant a second source. Captions never address a wire. }
      LWires := LDefinition.ToData.Field('wires');
      for LIndex := 0 to LDefinition.StepCount - 1 do
      begin
        LDescriptor := LDefinition.Descriptor(LIndex);
        for LPart := 0 to LDescriptor.PortCount - 1 do
        begin
          LPort := Default(TNyxToolWirePort);
          LPort.Kind := ntwpkPipeline;
          LPort.Endpoint := NyxFlowEndpoint(LDefinition.Step(LIndex), LDescriptor.Port(LPart).Reference);
          LPort.Direction := LDescriptor.Port(LPart).Direction;
          LConnected := False;

          if LPort.Direction = nfdInput then
          begin
            for LWire := 0 to LWires.Count - 1 do
            begin
              LTarget := NyxFlowEndpoint(
                NyxFlowStep(LWires.Item(LWire).Field('target').Field('step').AsText),
                NyxFlowPort(LWires.Item(LWire).Field('target').Field('port').AsText));
              LConnected := LConnected or LTarget.Same(LPort.Endpoint);
            end;
          end;

          if LConnected then
          begin
            Continue;
          end;
          LPort.ValueType := LDescriptor.Port(LPart).ValueType;
          LPort.Title := LDefinition.Step(LIndex).Name + ' / ' + LDescriptor.Port(LPart).Reference.Name;
          LPort.Description := LDescriptor.Port(LPart).Description;
          Add(LPort);
        end;
      end;
    end;

    if (FEditing.RouteRevision <> LRevision) or (FRuntime.Scene.Revision <> LScene) then
    begin
      raise ENyxFlow.Create('Control connector context changed during discovery');
    end;
    FContext.Free;
    FContext := LContext;
    LContext := nil;
    FRoot := LRoot;
    FControls := LControls;
    FPorts := LPorts;
    FFlow := LFlow;
    FDefinition := LDefinition;
    { Keep the admitted inert wire vector for repeated drawing. Serializing the
      entire step/configuration registry for every visible curve is unnecessary. }
    FNodeWires := LWires.Copy;
    FWires := nil;
    FDirty := False;
    FConfirmed := False;
    FDrag := False;
    FSource := -1;
    FTarget := -1;
    FSelectedWire := -1;
    FName := 'Canvas connection';
    FAppliedID := Default(TNyxFlowRouteRef);
    FAppliedOwner := NyxControl('');
    FScene := LScene;
    FRevision := LRevision;
    Inc(FEpoch);
  finally
    LContext.Free;
  end;
end;

procedure TControlWires.Connect(ASource, ATarget: Integer; AReplace: Boolean);
var
  LIndex: Integer;
  LExisting: Integer;
  LEventOwner: Integer;
begin

  if FGraphMode then
  begin
    ConnectGraph(ASource, ATarget, AReplace);
    Exit;
  end;
  RequireCurrent;
  CheckPort(ASource);
  CheckPort(ATarget);

  if (FPorts[ASource].Direction <> nfdOutput) or
    (FPorts[ATarget].Direction <> nfdInput) or
    (FPorts[ASource].Kind = FPorts[ATarget].Kind) or
    (FPorts[ASource].ValueType <> FPorts[ATarget].ValueType) then
  begin
    raise ENyxFlow.Create('Connect a control and pipeline output/input of the exact same value family');
  end;
  LExisting := -1;
  LEventOwner := -1;
  for LIndex := 0 to High(FWires) do
  begin

    if FPorts[FWires[LIndex].Source].Kind = ntwpkControl then
    begin
      LEventOwner := FWires[LIndex].Source;
    end;

    if FWires[LIndex].Target = ATarget then
    begin
      LExisting := LIndex;
    end;
  end;

  if (LExisting >= 0) and (FWires[LExisting].Source = ASource) then
  begin
    Exit;
  end;

  if (LExisting >= 0) and not AReplace then
  begin
    raise ENyxFlow.Create('Acknowledge replacing the staged input wire');
  end;

  if (FPorts[ASource].Kind = ntwpkControl) and (LEventOwner >= 0) and
    ((FPorts[ASource].ControlPort.Control.ID <> FPorts[LEventOwner].ControlPort.Control.ID) or
     (FPorts[ASource].ControlPort.Trigger <> FPorts[LEventOwner].ControlPort.Trigger) or
     (FPorts[ASource].ControlPort.EventName.Name <> FPorts[LEventOwner].ControlPort.EventName.Name)) then
  begin
    raise ENyxFlow.Create('One route has one exact control/event owner; apply or discard it before another event');
  end;

  if LExisting < 0 then
  begin

    if Length(FWires) = 256 then
    begin
      raise ENyxFlow.Create('A control route proposal admits at most 256 staged wires');
    end;
    LExisting := Length(FWires);
    SetLength(FWires, LExisting + 1);
  end;
  FWires[LExisting].Source := ASource;
  FWires[LExisting].Target := ATarget;
  FDirty := True;
  FConfirmed := False;
  FMessage := 'Wire staged. Apply when the complete pipeline inputs are connected.';
end;

procedure TControlWires.Remove(AIndex: Integer; AConfirmed: Boolean);
var
  LIndex: Integer;
begin

  if FGraphMode then
  begin
    raise ENyxFlow.Create('Use RemoveGraphWire for internal graph connections');
  end;
  RequireCurrent;
  Wire(AIndex);

  if not AConfirmed then
  begin
    raise ENyxFlow.Create('Acknowledge removing the selected staged wire');
  end;
  for LIndex := AIndex to High(FWires) - 1 do
  begin
    FWires[LIndex] := FWires[LIndex + 1];
  end;
  SetLength(FWires, Length(FWires) - 1);
  FSelectedWire := -1;
  FDirty := True;
  FConfirmed := False;
end;

function TControlWires.DraftRoute(const AID: TNyxFlowRouteRef;
  out AOwner: TNyxControlRef): TNyxFlowRoute;
var
  LIndex: Integer;
  LEvent: Integer;
  LSource: TNyxToolWirePort;
  LTarget: TNyxToolWirePort;
begin
  LEvent := -1;
  for LIndex := 0 to High(FWires) do
  begin

    if FPorts[FWires[LIndex].Source].Kind = ntwpkControl then
    begin
      LEvent := FWires[LIndex].Source;
      Break;
    end;
  end;

  if LEvent < 0 then
  begin
    raise ENyxFlow.Create('Connect a control event to a pipeline input before applying');
  end;
  LSource := FPorts[LEvent];
  AOwner := LSource.ControlPort.Control;

  if LSource.ControlPort.Trigger = ntNamed then
  begin
    Result := NyxNamedFlowRoute(AID, FFlow, LSource.ControlPort.EventName, FOptions.CallbackMode);
  end
  else
  begin
    Result := NyxFlowRoute(AID, FFlow, LSource.ControlPort.Trigger, FOptions.CallbackMode);
  end;
  Result := Result.Using(FOptions.ExecutionPolicy).Concurrent(FOptions.Concurrency)
    .MaximumRuns(FOptions.RunLimit);
  for LIndex := 0 to High(FWires) do
  begin
    LSource := FPorts[FWires[LIndex].Source];
    LTarget := FPorts[FWires[LIndex].Target];

    if LSource.Kind = ntwpkControl then
    begin
      case LSource.ControlPort.Role of
        ntprEventSignal:
          begin
            Result := Result.InputSignal(LTarget.Endpoint);
          end;
        ntprEventValue:
          begin
            Result := Result.InputEventValue(LTarget.Endpoint, LSource.ValueType);
          end;
        ntprEventDetails:
          begin
            Result := Result.InputEventDetails(LTarget.Endpoint, NyxResourcePath);
          end;
        else
          begin
            raise ENyxFlow.Create('A bound input cannot become an event output');
          end;
      end;
    end
    else
    begin
      case LTarget.ValueType of
        nftText:
          begin
            Result := Result.OutputState(LSource.Endpoint, NyxTextState(LTarget.ControlPort.StateName));
          end;
        nftBoolean:
          begin
            Result := Result.OutputState(LSource.Endpoint, NyxBooleanState(LTarget.ControlPort.StateName));
          end;
        nftInteger:
          begin
            Result := Result.OutputState(LSource.Endpoint, NyxIntegerState(LTarget.ControlPort.StateName));
          end;
        nftNumber:
          begin
            Result := Result.OutputState(LSource.Endpoint, NyxNumberState(LTarget.ControlPort.StateName));
          end;
        else
          begin
            raise ENyxFlow.Create('Choose a real existing scalar control binding');
          end;
      end;
    end;
  end;
end;

function TControlWires.Publish(const AOwner: TNyxControlRef;
  const ARoutes: INyxFlowRoutes): TNyxText;
var
  LLease: INyxToolControlWires;
  LView: INyxCanvasWorkspaceView;
  LEditing: INyxCanvasRouteEditing;
  LCandidate: TNyxDocument;
  LFormer: TNyxDocument;
  LCandidateRoot: TNyxNode;
  LRootID: TNyxText;
  LRevision: Integer;
  LChanges: TNyxCanvasRouteChanges;
  LIndex: Integer;
  LInstalled: Boolean;
begin
  RequireCurrent;
  Result := '';
  LLease := Self;
  LView := FView;
  LEditing := FEditing;
  LRevision := FRevision;
  LRootID := FRoot.ID;
  LCandidate := FContext.Clone;
  try
    SetNyxFlowRoutes(LCandidate.Find(AOwner.ID), ARoutes);
    LCandidate.Validate;
    LCandidateRoot := LCandidate.Find(LRootID);
    try
      LEditing.ApplyRouteChanges([NyxCanvasRoutes(FItem, AOwner, ARoutes)], FScene, LRevision);
    except
      on E: ENyxPublicationNotification do
      begin
        { A receiver failure may follow installation. Reconcile only an exact
          observed installed revision and route set; an exception name cannot
          manufacture successful admission. A retired view supplies no evidence. }
        LInstalled := False;

        if LView.Connected and (LEditing.RouteRevision = LRevision + 1) then
        begin
          LChanges := LEditing.CaptureRoutes;
          for LIndex := 0 to High(LChanges) do
          begin
            LInstalled := LInstalled or
              ((LChanges[LIndex].Item.Name = FItem.Name) and
               (LChanges[LIndex].Control.ID = AOwner.ID) and
               (LChanges[LIndex].Routes.ToData.ToJSON = ARoutes.ToData.ToJSON));
          end;
        end;

        if not LInstalled then
        begin
          raise;
        end;
        Result := E.Message;
      end;
    end;
    { Adopt before releasing the former context. Notifications may disconnect
      this controller, so no retained interface field is dereferenced afterward.
      A later external revision deliberately makes this proposal stale. }
    LFormer := FContext;
    FContext := LCandidate;
    LCandidate := nil;
    FRoot := LCandidateRoot;
    FRevision := LRevision + 1;
    FDirty := False;
    FConfirmed := False;
    FDrag := False;
    LFormer.Free;
  finally
    LCandidate.Free;
  end;
end;

procedure TControlWires.Apply(const AID: TNyxFlowRouteRef; AReplace: Boolean);
var
  LLease: INyxToolControlWires;
  LRoute: TNyxFlowRoute;
  LOwner: TNyxControlRef;
  LRoutes: INyxFlowRoutes;
  LIndex: Integer;
  LNotification: TNyxText;
begin

  if FGraphMode then
  begin
    raise ENyxFlow.Create('Use ApplyGraph for the shared Logic proposal');
  end;
  RequireCurrent;
  { Publication may release the caller's last interface. Retain this complete
    entry point, including its local accepted-status update after Publish. }
  LLease := Self;
  LRoute := DraftRoute(AID, LOwner);
  LRoutes := NyxControlFlowRoutes(FRoot.Find(LOwner.ID));
  for LIndex := 0 to LRoutes.Count - 1 do
  begin

    if (LRoutes.Route(LIndex).ID.Name = AID.Name) and not AReplace then
    begin
      raise ENyxFlow.Create('Acknowledge replacing the existing route with this name');
    end;
  end;
  LNotification := Publish(LOwner, LRoutes.WithRoute(LRoute));
  FAppliedID := AID;
  FAppliedOwner := LOwner;
  FMessage := 'Control route applied to this running item.';

  if LNotification <> '' then
  begin
    FMessage := FMessage + ' ' + LNotification;
  end;
end;

procedure TControlWires.RemoveApplied(AConfirmed: Boolean);
var
  LLease: INyxToolControlWires;
  LRoutes: INyxFlowRoutes;
  LNotification: TNyxText;
begin

  if FGraphMode then
  begin
    raise ENyxFlow.Create('Return to control connections before removing an applied route');
  end;
  RequireCurrent;
  LLease := Self;

  if not AConfirmed or (FAppliedID.Name = '') then
  begin
    raise ENyxFlow.Create('Acknowledge removing the route applied by this connector');
  end;
  LRoutes := NyxControlFlowRoutes(FRoot.Find(FAppliedOwner.ID));
  LNotification := Publish(FAppliedOwner, LRoutes.WithoutRoute(FAppliedID));
  FAppliedID := Default(TNyxFlowRouteRef);
  FMessage := 'Applied route removed; static callbacks remain active.';

  if LNotification <> '' then
  begin
    FMessage := FMessage + ' ' + LNotification;
  end;
end;

{$I nyx.tool.wires.forms.inc}
{$I nyx.tool.wires.nodes.inc}
{$I nyx.tool.wires.graph.inc}

function NewNyxToolControlWires(const AID: TNyxText;
  const AView: INyxCanvasWorkspaceView; const AItem: TNyxCanvasItemRef;
  const AOptions: TNyxToolWireOptions): INyxToolControlWires;
begin
  Result := TControlWires.Create(AID, AView, AItem, AOptions);
end;

end.
