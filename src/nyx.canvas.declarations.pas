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
unit nyx.canvas.declarations;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.root.types, nyx.types,
  nyx.canvas.geometry, nyx.canvas.types, nyx.canvas.route.types, nyx.flow.routes,
  nyx.flow.types, nyx.flow.declarations;

const
  NyxCanvasesWireField = 'canvases';
  MaximumNyxCanvasDefinitions = 64;
  MaximumNyxCanvasDefinitionBytes = 1048576;
  MaximumNyxCanvasConnections = 2048;
  MaximumNyxCanvasGraphNodes = 4096;

type
  { Authored content belongs to an existing document root. The canvas owns copied
    references/placement only; neither this record nor a definition retains nodes,
    a document, runtime store, callbacks, DOM or LCL handles. }
  TNyxCanvasDeclarationItem = record
    Item: TNyxCanvasItemRef;
    Content: TNyxRootRef;
    Placement: TNyxCanvasPlacement;
  end;

  { Saved world position of one named operation within one graph. This is
    canvas presentation intent, independent of graph execution and camera zoom.
    It owns only typed references and a finite point, never a graph or widget.
    All three fields must be defined. Absent graph/step names remain dormant:
    they grant no execution authority and can support Undo or later recreation. }
  TNyxCanvasGraphNode = record
    Graph: TNyxFlowRef;
    Step: TNyxFlowStepRef;
    Position: TNyxCanvasPoint;
  end;

  { Detached ordered layout window. Callers own the array; definitions copy its
    elements and never retain caller storage or a backreference to its canvas. }
  TNyxCanvasGraphNodes = array of TNyxCanvasGraphNode;

  { Immutable fluent declaration. Every builder operation returns an independent
    plan. Layer/item names are exact; duplicate names, missing layers and invalid
    geometry refuse before publishing a new plan. Empty scenes are useful.
    Document validation resolves every content root and rejects missing content.
    Saved camera viewport is an initial logical reference; mounts use actual
    allocation and independently retain runtime pan/zoom. }
  INyxCanvasDefinition = interface(IInterface)
    ['{737A7921-4621-4C6F-8C01-0CA000000020}']
    function GetCamera: TNyxCanvasCamera;
    function GetGrid: TNyxCanvasGrid;
    function GetScaling: TNyxCanvasControlScaling;
    function GetLayerCount: Integer;
    function GetLayer(AIndex: Integer): TNyxCanvasLayerInfo;
    function GetItemCount: Integer;
    function GetItem(AIndex: Integer): TNyxCanvasDeclarationItem;
    function WithCamera(const ACamera: TNyxCanvasCamera): INyxCanvasDefinition;
    function WithGrid(const AGrid: TNyxCanvasGrid): INyxCanvasDefinition;
    function WithScaling(AScaling: TNyxCanvasControlScaling): INyxCanvasDefinition;
    function WithLayer(const ALayer: TNyxCanvasLayerRef;
      AVisible: Boolean = True; APickable: Boolean = True): INyxCanvasDefinition;
    function WithItem(const AItem: TNyxCanvasItemRef; const AContent: TNyxRootRef;
      const APlacement: TNyxCanvasPlacement): INyxCanvasDefinition;
    { Strict versioned persistence/extension boundary. Registry admission
      normalizes alternate implementations once through this exact descriptor. }
    function ToData: TNyxDataValue;
    property Camera: TNyxCanvasCamera read GetCamera;
    property Grid: TNyxCanvasGrid read GetGrid;
    property Scaling: TNyxCanvasControlScaling read GetScaling;
    property LayerCount: Integer read GetLayerCount;
    property Layers[AIndex: Integer]: TNyxCanvasLayerInfo read GetLayer;
    property ItemCount: Integer read GetItemCount;
    property Items[AIndex: Integer]: TNyxCanvasDeclarationItem read GetItem;
  end;

  { Optional immutable per-item/control route masks. Entries own copied inert
    route intent, never nodes, runtime values or execution provenance. An explicit
    empty set masks inherited routes; absence inherits the referenced root.
    Original definition implementations/interfaces remain independently usable. }
  INyxCanvasDefinitionConnections = interface(IInterface)
    ['{F067763B-01DB-4751-8041-91695DE94D39}']
    function GetConnectionCount: Integer;
    function Connection(AIndex: Integer): TNyxCanvasRouteChange;
    property ConnectionCount: Integer read GetConnectionCount;
  end;

  { Optional immutable graph-node layout on the saved canvas. Absence preserves
    the original definition ABI and legacy bytes. Indices are local to this
    snapshot; graph/step references supply identity. Returned values are inert. }
  INyxCanvasDefinitionGraphLayout = interface(IInterface)
    ['{D1CF5F70-72BD-4A39-8B3E-7005E3732471}']
    function GetGraphNodeCount: Integer;
    function GraphNode(AIndex: Integer): TNyxCanvasGraphNode;
    property GraphNodeCount: Integer read GetGraphNodeCount;
  end;

  { Immutable fluent layout configuration around an independent saved plan.
    Place updates a key in place or appends it; Automatic removes an existing
    explicit position. Nodes admits a distinct-key group atomically. Earlier
    branches survive failure. Full count/byte budgets refuse without truncation.
    Dormant names remain inert until the matching operation is displayed. }
  INyxCanvasGraphLayout = interface(IInterface)
    ['{D1CF5F70-72BD-4A39-8B3E-7005E3732472}']
    function Place(const AGraph: TNyxFlowRef; const AStep: TNyxFlowStepRef;
      const APosition: TNyxCanvasPoint): INyxCanvasGraphLayout;
    function Nodes(const ANodes: array of TNyxCanvasGraphNode): INyxCanvasGraphLayout;
    function Automatic(const AGraph: TNyxFlowRef;
      const AStep: TNyxFlowStepRef): INyxCanvasGraphLayout;
    function Done: INyxCanvasDefinition;
  end;

  { Immutable fluent connection configuration around an independent saved plan.
    Each edit returns a new builder, preserving earlier branches on success or
    failure. Item/control keys are exact. Document admission later resolves the
    actual control and graph/state contracts. At most 2048 masks and the complete
    definition's byte budget apply; overflow never truncates intent. }
  INyxCanvasConnections = interface(IInterface)
    ['{10CFA1C0-ACF4-4934-8810-6094E2BE9275}']
    { One whole typed replacement; empty routes explicitly disable inheritance. }
    function ForControl(const AItem: TNyxCanvasItemRef; const AControl: TNyxControlRef;
      const ARoutes: INyxFlowRoutes): INyxCanvasConnections;
    { One admitted group of distinct item/control changes, in supplied order.
      A later invalid member refuses the complete new builder. }
    function Controls(const AChanges: array of TNyxCanvasRouteChange): INyxCanvasConnections;
    { Remove an existing mask, restoring the referenced root's route behavior. }
    function Inherit(const AItem: TNyxCanvasItemRef;
      const AControl: TNyxControlRef): INyxCanvasConnections;
    { Return an independently owned immutable definition, retaining no builder. }
    function Done: INyxCanvasDefinition;
  end;

  { Mutable document-owned registry of immutable admitted plans. References may
    precede content roots during a grouped edit; the document validates the whole
    candidate. Define stages normalization before publication. Clone copies the
    vector and shares immutable plans only, with no owner backreferences.
    Retained registry/definition interfaces may outlive their document.
    Definition/Remove refuse absent references; Reference refuses invalid indices.
    A valid existing reference keeps its position when its definition is replaced. }
  INyxCanvasDeclarations = interface(IInterface)
    ['{737A7921-4621-4C6F-8C01-0CA000000021}']
    function Define(const AReference: TNyxCanvasRef;
      const ADefinition: INyxCanvasDefinition): INyxCanvasDeclarations;
    function Remove(const AReference: TNyxCanvasRef): INyxCanvasDeclarations;
    function Contains(const AReference: TNyxCanvasRef): Boolean;
    function Definition(const AReference: TNyxCanvasRef): INyxCanvasDefinition;
    function Reference(AIndex: Integer): TNyxCanvasRef;
    function GetCount: Integer;
    function Clone: INyxCanvasDeclarations;
    function ToData: TNyxDataValue;
    property Count: Integer read GetCount;
  end;

function NewNyxCanvasDefinition: INyxCanvasDefinition;
{ Admit a copied world position. Invalid/undefined references or points refuse. }
function NyxCanvasGraphNode(const AGraph: TNyxFlowRef; const AStep: TNyxFlowStepRef;
  const APosition: TNyxCanvasPoint): TNyxCanvasGraphNode;
{ Copy/admit creator data before applying layout; other canvas intent is retained. }
function NyxCanvasGraphLayout(const ADefinition: INyxCanvasDefinition): INyxCanvasGraphLayout;
{ Detached optional layout window; definitions without the capability yield none. }
function NyxCanvasGraphNodes(const ADefinition: INyxCanvasDefinition): TNyxCanvasGraphNodes;
{ Copy/admit a definition before configuring its per-item routes. Creator getters
  grant no runtime authority. Plans without masks keep their version-one bytes. }
function NyxCanvasConnections(const ADefinition: INyxCanvasDefinition): INyxCanvasConnections;
{ Return detached intent, or an empty vector when the optional capability is
  absent. Callers must still validate scene/item/control ownership on adoption. }
function NyxCanvasConnectionChanges(const ADefinition: INyxCanvasDefinition): TNyxCanvasRouteChanges;
function NyxCanvasDefinitionFromData(const AValue: TNyxDataValue): INyxCanvasDefinition;
function NewNyxCanvasDeclarations: INyxCanvasDeclarations;
function NyxCanvasDeclarationsFromData(const AValue: TNyxDataValue): INyxCanvasDeclarations;
function NyxCanvasScalingName(AScaling: TNyxCanvasControlScaling): TNyxText;

implementation

type
  { Default immutable saved plan owns only detached geometry, root references,
    connection and graph-layout intent. Private copies never expose document/view
    ownership; all fluent branches retain independent array storage. }
  TCanvasDefinition = class(TInterfacedObject, INyxCanvasDefinition, INyxCanvasDefinitionConnections,
    INyxCanvasDefinitionGraphLayout)
  private
    FCamera: TNyxCanvasCamera;
    FGrid: TNyxCanvasGrid;
    FScaling: TNyxCanvasControlScaling;
    FLayers: array of TNyxCanvasLayerInfo;
    FItems: array of TNyxCanvasDeclarationItem;
    FConnections: TNyxCanvasRouteChanges;
    FGraphNodes: TNyxCanvasGraphNodes;
    function CopyPlan: TCanvasDefinition;
    procedure SetConnection(const AChange: TNyxCanvasRouteChange);
  public
    constructor Create;
    function GetCamera: TNyxCanvasCamera;
    function GetGrid: TNyxCanvasGrid;
    function GetScaling: TNyxCanvasControlScaling;
    function GetLayerCount: Integer;
    function GetLayer(AIndex: Integer): TNyxCanvasLayerInfo;
    function GetItemCount: Integer;
    function GetItem(AIndex: Integer): TNyxCanvasDeclarationItem;
    function WithCamera(const ACamera: TNyxCanvasCamera): INyxCanvasDefinition;
    function WithGrid(const AGrid: TNyxCanvasGrid): INyxCanvasDefinition;
    function WithScaling(AScaling: TNyxCanvasControlScaling): INyxCanvasDefinition;
    function WithLayer(const ALayer: TNyxCanvasLayerRef;
      AVisible, APickable: Boolean): INyxCanvasDefinition;
    function WithItem(const AItem: TNyxCanvasItemRef; const AContent: TNyxRootRef;
      const APlacement: TNyxCanvasPlacement): INyxCanvasDefinition;
    function ToData: TNyxDataValue;
    function GetConnectionCount: Integer;
    function Connection(AIndex: Integer): TNyxCanvasRouteChange;
    function GetGraphNodeCount: Integer;
    function GraphNode(AIndex: Integer): TNyxCanvasGraphNode;
  end;

  { Builder owns one private unreferenced plan. Edits and Done expose independent
    copies, so no raw plan is simultaneously freed and interface-owned. Geometry,
    route masks and other graph positions retain their exact prior meaning. }
  TCanvasGraphLayout = class(TInterfacedObject, INyxCanvasGraphLayout)
  private
    FPlan: TCanvasDefinition;
  public
    constructor CreateOwned(APlan: TCanvasDefinition);
    destructor Destroy; override;
    function Place(const AGraph: TNyxFlowRef; const AStep: TNyxFlowStepRef;
      const APosition: TNyxCanvasPoint): INyxCanvasGraphLayout;
    function Nodes(const ANodes: array of TNyxCanvasGraphNode): INyxCanvasGraphLayout;
    function Automatic(const AGraph: TNyxFlowRef;
      const AStep: TNyxFlowStepRef): INyxCanvasGraphLayout;
    function Done: INyxCanvasDefinition;
  end;

  { Managed builder owns a private, unreferenced raw plan. Done and edits expose
    copies only, so its plan is never simultaneously freed and interface-owned.
    No callback, document or live canvas is retained by this configuration. }
  TCanvasConnections = class(TInterfacedObject, INyxCanvasConnections)
  private
    FPlan: TCanvasDefinition;
  public
    constructor CreateOwned(APlan: TCanvasDefinition);
    destructor Destroy; override;
    function ForControl(const AItem: TNyxCanvasItemRef; const AControl: TNyxControlRef;
      const ARoutes: INyxFlowRoutes): INyxCanvasConnections;
    function Controls(const AChanges: array of TNyxCanvasRouteChange): INyxCanvasConnections;
    function Inherit(const AItem: TNyxCanvasItemRef;
      const AControl: TNyxControlRef): INyxCanvasConnections;
    function Done: INyxCanvasDefinition;
  end;

  { Document-owned membership shares admitted immutable plans. Replacing a plan
    retains registry order; no declaration retains its document or runtime. }
  TCanvasDeclarations = class(TInterfacedObject, INyxCanvasDeclarations)
  private
    FReferences: array of TNyxCanvasRef;
    FDefinitions: array of INyxCanvasDefinition;
    function IndexOf(const AReference: TNyxCanvasRef): Integer;
  public
    function Define(const AReference: TNyxCanvasRef;
      const ADefinition: INyxCanvasDefinition): INyxCanvasDeclarations;
    function Remove(const AReference: TNyxCanvasRef): INyxCanvasDeclarations;
    function Contains(const AReference: TNyxCanvasRef): Boolean;
    function Definition(const AReference: TNyxCanvasRef): INyxCanvasDefinition;
    function Reference(AIndex: Integer): TNyxCanvasRef;
    function GetCount: Integer;
    function Clone: INyxCanvasDeclarations;
    function ToData: TNyxDataValue;
  end;

procedure ExactFields(const AValue: TNyxDataValue; const AFields: array of TNyxText);
var
  LIndex: Integer;
  LField: Integer;
  LFound: Boolean;
begin

  if (AValue.Kind <> ndObject) or (AValue.Count <> Length(AFields)) then
  begin
    raise ENyxCanvasScene.Create('Canvas descriptor has missing or extra fields');
  end;
  for LIndex := 0 to AValue.Count - 1 do
  begin
    LFound := False;
    for LField := 0 to High(AFields) do
    begin

      if AValue.Key(LIndex) = AFields[LField] then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxCanvasScene.Create('Unknown canvas descriptor field');
    end;
  end;
end;

function NyxCanvasScalingName(AScaling: TNyxCanvasControlScaling): TNyxText;
begin
  case Ord(AScaling) of
    Ord(nccsReflow):
      begin
        Result := 'reflow';
      end;
    Ord(nccsLogicalSize):
      begin
        Result := 'logical-size';
      end;
  else
    raise ENyxCanvasScene.Create('Unknown canvas live control scaling');
  end;
end;

constructor TCanvasDefinition.Create;
begin
  inherited Create;
  FCamera := NyxCanvasCamera(800, 600, NyxCanvasPoint(0, 0));
  FGrid := NyxCanvasGrid;
  FScaling := nccsReflow;
end;

function NewNyxCanvasDefinition: INyxCanvasDefinition;
begin
  Result := TCanvasDefinition.Create;
end;

function TCanvasDefinition.CopyPlan: TCanvasDefinition;
var
  LIndex: Integer;
begin
  Result := TCanvasDefinition.Create;
  try
    Result.FCamera := FCamera;
    Result.FGrid := FGrid;
    Result.FScaling := FScaling;
    SetLength(Result.FLayers, Length(FLayers));
    SetLength(Result.FItems, Length(FItems));
    for LIndex := 0 to High(FLayers) do
    begin
      Result.FLayers[LIndex] := FLayers[LIndex];
    end;
    for LIndex := 0 to High(FItems) do
    begin
      Result.FItems[LIndex] := FItems[LIndex];
    end;
    SetLength(Result.FConnections, Length(FConnections));
    for LIndex := 0 to High(FConnections) do
    begin
      Result.FConnections[LIndex] := TNyxCanvasRouteChange.FromData(FConnections[LIndex].ToData);
    end;
    SetLength(Result.FGraphNodes, Length(FGraphNodes));
    for LIndex := 0 to High(FGraphNodes) do
    begin
      Result.FGraphNodes[LIndex] := NyxCanvasGraphNode(FGraphNodes[LIndex].Graph,
        FGraphNodes[LIndex].Step, FGraphNodes[LIndex].Position);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TCanvasDefinition.GetCamera: TNyxCanvasCamera;
begin
  Result := FCamera;
end;

function TCanvasDefinition.GetGrid: TNyxCanvasGrid;
begin
  Result := FGrid;
end;

function TCanvasDefinition.GetScaling: TNyxCanvasControlScaling;
begin
  Result := FScaling;
end;

function TCanvasDefinition.GetLayerCount: Integer;
begin
  Result := Length(FLayers);
end;

function TCanvasDefinition.GetLayer(AIndex: Integer): TNyxCanvasLayerInfo;
begin

  if (AIndex < 0) or (AIndex >= Length(FLayers)) then
  begin
    raise ENyxCanvasScene.Create('Canvas declaration layer index is outside its plan');
  end;
  Result := FLayers[AIndex];
end;

function TCanvasDefinition.GetItemCount: Integer;
begin
  Result := Length(FItems);
end;

function TCanvasDefinition.GetItem(AIndex: Integer): TNyxCanvasDeclarationItem;
begin

  if (AIndex < 0) or (AIndex >= Length(FItems)) then
  begin
    raise ENyxCanvasScene.Create('Canvas declaration item index is outside its plan');
  end;
  Result := FItems[AIndex];
end;

function TCanvasDefinition.WithCamera(const ACamera: TNyxCanvasCamera): INyxCanvasDefinition;
var
  LPlan: TCanvasDefinition;
begin

  if not ACamera.Defined then
  begin
    raise ENyxCanvasScene.Create('Saved canvas camera is absent');
  end;
  LPlan := CopyPlan;
  Result := LPlan;
  LPlan.FCamera := ACamera;
end;

function TCanvasDefinition.WithGrid(const AGrid: TNyxCanvasGrid): INyxCanvasDefinition;
var
  LPlan: TCanvasDefinition;
begin

  if not AGrid.Defined then
  begin
    raise ENyxCanvasScene.Create('Saved canvas grid is absent');
  end;
  LPlan := CopyPlan;
  Result := LPlan;
  LPlan.FGrid := AGrid;
end;

function TCanvasDefinition.WithScaling(AScaling: TNyxCanvasControlScaling): INyxCanvasDefinition;
var
  LPlan: TCanvasDefinition;
begin
  NyxCanvasScalingName(AScaling);
  LPlan := CopyPlan;
  Result := LPlan;
  LPlan.FScaling := AScaling;
end;

function TCanvasDefinition.WithLayer(const ALayer: TNyxCanvasLayerRef;
  AVisible, APickable: Boolean): INyxCanvasDefinition;
var
  LPlan: TCanvasDefinition;
  LIndex: Integer;
  LName: TNyxText;
begin
  LName := ALayer.Name;

  if Length(FLayers) = MaximumNyxCanvasLayers then
  begin
    raise ENyxCanvasScene.Create('Saved canvas layer capacity is exhausted');
  end;
  for LIndex := 0 to High(FLayers) do
  begin

    if FLayers[LIndex].Layer.Name = LName then
    begin
      raise ENyxCanvasScene.Create('Saved canvas layer is duplicate');
    end;
  end;
  LPlan := CopyPlan;
  Result := LPlan;
  SetLength(LPlan.FLayers, Length(FLayers) + 1);
  LPlan.FLayers[High(LPlan.FLayers)].Layer := ALayer;
  LPlan.FLayers[High(LPlan.FLayers)].IsVisible := AVisible;
  LPlan.FLayers[High(LPlan.FLayers)].IsPickable := APickable;
  LPlan.FLayers[High(LPlan.FLayers)].PaintOrder := Length(FLayers);
end;

function TCanvasDefinition.WithItem(const AItem: TNyxCanvasItemRef;
  const AContent: TNyxRootRef; const APlacement: TNyxCanvasPlacement): INyxCanvasDefinition;
var
  LPlan: TCanvasDefinition;
  LIndex: Integer;
  LFound: Boolean;
  LName: TNyxText;
begin
  LName := AItem.Name;
  NyxRoot(AContent.Kind, AContent.Name);

  if not APlacement.Bounds.Defined or (Length(FItems) = MaximumNyxCanvasItems) then
  begin
    raise ENyxCanvasScene.Create('Saved canvas item requires admitted bounds and available capacity');
  end;
  LFound := False;
  for LIndex := 0 to High(FLayers) do
  begin
    LFound := LFound or (FLayers[LIndex].Layer.Name = APlacement.Layer.Name);
  end;

  if not LFound then
  begin
    raise ENyxCanvasScene.Create('Saved canvas item layer does not exist');
  end;
  for LIndex := 0 to High(FItems) do
  begin

    if FItems[LIndex].Item.Name = LName then
    begin
      raise ENyxCanvasScene.Create('Saved canvas item is duplicate');
    end;
  end;
  LPlan := CopyPlan;
  Result := LPlan;
  SetLength(LPlan.FItems, Length(FItems) + 1);
  LPlan.FItems[High(LPlan.FItems)].Item := AItem;
  LPlan.FItems[High(LPlan.FItems)].Content := AContent;
  LPlan.FItems[High(LPlan.FItems)].Placement := APlacement;
end;

function TCanvasDefinition.ToData: TNyxDataValue;
var
  LLayers: array of TNyxDataValue;
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LGridStyle: TNyxText;
  LSnap: TNyxText;
  LFields: array of TNyxDataField;
  LConnections: array of TNyxDataValue;
  LGraphNodes: array of TNyxDataValue;
begin
  SetLength(LLayers, Length(FLayers));
  SetLength(LItems, Length(FItems));
  for LIndex := 0 to High(FLayers) do
  begin
    LLayers[LIndex] := NyxObject([
      NyxField('name', NyxData(FLayers[LIndex].Layer.Name)),
      NyxField('visible', NyxData(FLayers[LIndex].IsVisible)),
      NyxField('pickable', NyxData(FLayers[LIndex].IsPickable))]);
  end;
  for LIndex := 0 to High(FItems) do
  begin
    LItems[LIndex] := NyxObject([
      NyxField('name', NyxData(FItems[LIndex].Item.Name)),
      NyxField('rootKind', NyxData(NyxRootKindName(FItems[LIndex].Content.Kind))),
      NyxField('root', NyxData(FItems[LIndex].Content.Name)),
      NyxField('layer', NyxData(FItems[LIndex].Placement.Layer.Name)),
      NyxField('left', NyxData(FItems[LIndex].Placement.Bounds.Left)),
      NyxField('top', NyxData(FItems[LIndex].Placement.Bounds.Top)),
      NyxField('width', NyxData(FItems[LIndex].Placement.Bounds.Width)),
      NyxField('height', NyxData(FItems[LIndex].Placement.Bounds.Height)),
      NyxField('visible', NyxData(FItems[LIndex].Placement.IsVisible)),
      NyxField('pickable', NyxData(FItems[LIndex].Placement.IsPickable))]);
  end;
  case FGrid.Style of
    ncgsNone:
      begin
        LGridStyle := 'none';
      end;
    ncgsDots:
      begin
        LGridStyle := 'dots';
      end;
    ncgsLines:
      begin
        LGridStyle := 'lines';
      end;
  end;
  LSnap := 'free';

  if FGrid.SnapMode = ncsmGrid then
  begin
    LSnap := 'grid';
  end;
  Result := NyxObject([
    NyxField('version', NyxData(1)),
    NyxField('camera', NyxObject([
      NyxField('width', NyxData(FCamera.ViewportWidth)),
      NyxField('height', NyxData(FCamera.ViewportHeight)),
      NyxField('centerX', NyxData(FCamera.WorldCenter.X)),
      NyxField('centerY', NyxData(FCamera.WorldCenter.Y)),
      NyxField('zoom', NyxData(FCamera.Zoom)),
      NyxField('minimumZoom', NyxData(FCamera.Limits.Minimum)),
      NyxField('maximumZoom', NyxData(FCamera.Limits.Maximum))])),
    NyxField('grid', NyxObject([
      NyxField('style', NyxData(LGridStyle)),
      NyxField('snap', NyxData(LSnap)),
      NyxField('spacing', NyxData(FGrid.Spacing)),
      NyxField('originX', NyxData(FGrid.Origin.X)),
      NyxField('originY', NyxData(FGrid.Origin.Y))])),
    NyxField('scaling', NyxData(NyxCanvasScalingName(FScaling))),
    NyxField('layers', NyxArray(LLayers)),
    NyxField('items', NyxArray(LItems))]);

  if (Length(FConnections) > 0) or (Length(FGraphNodes) > 0) then
  begin
    SetLength(LFields, 7 + Ord(Length(FGraphNodes) > 0));
    for LIndex := 0 to 5 do
    begin
      LFields[LIndex] := NyxField(Result.Key(LIndex), Result.Field(Result.Key(LIndex)));
    end;
    LFields[0] := NyxField('version', NyxData(2));
    SetLength(LConnections, Length(FConnections));
    for LIndex := 0 to High(FConnections) do
    begin
      LConnections[LIndex] := FConnections[LIndex].ToData;
    end;
    LFields[6] := NyxField('connections', NyxArray(LConnections));

    if Length(FGraphNodes) > 0 then
    begin
      LFields[0] := NyxField('version', NyxData(3));
      SetLength(LGraphNodes, Length(FGraphNodes));
      for LIndex := 0 to High(FGraphNodes) do
      begin
        LGraphNodes[LIndex] := NyxObject([
          NyxField('graph', NyxData(FGraphNodes[LIndex].Graph.Name)),
          NyxField('step', NyxData(FGraphNodes[LIndex].Step.Name)),
          NyxField('x', NyxData(FGraphNodes[LIndex].Position.X)),
          NyxField('y', NyxData(FGraphNodes[LIndex].Position.Y))]);
      end;
      LFields[7] := NyxField('graphNodes', NyxArray(LGraphNodes));
    end;
    Result := NyxObject(LFields);
  end;
end;

function NyxCanvasDefinitionFromData(const AValue: TNyxDataValue): INyxCanvasDefinition;
var
  LCamera: TNyxDataValue;
  LGrid: TNyxDataValue;
  LLayers: TNyxDataValue;
  LItems: TNyxDataValue;
  LValue: TNyxDataValue;
  LIndex: Integer;
  LStyle: TNyxCanvasGridStyle;
  LSnap: TNyxCanvasSnapMode;
  LScaling: TNyxCanvasControlScaling;
  LRoot: TNyxRootRef;
  LText: TNyxText;
  LVersion: Integer;
  LHasConnections: Boolean;
  LHasGraphNodes: Boolean;
  LConnectionData: TNyxDataValue;
  LConnections: TNyxCanvasRouteChanges;
  LGraphData: TNyxDataValue;
  LGraphNodes: TNyxCanvasGraphNodes;
begin
  LHasConnections := (AValue.Kind = ndObject) and (AValue.Count in [7, 8]);
  LHasGraphNodes := (AValue.Kind = ndObject) and (AValue.Count = 8);

  if LHasGraphNodes then
  begin
    ExactFields(AValue, ['version', 'camera', 'grid', 'scaling', 'layers', 'items',
      'connections', 'graphNodes']);
  end
  else if LHasConnections then
  begin
    ExactFields(AValue, ['version', 'camera', 'grid', 'scaling', 'layers', 'items', 'connections']);
  end
  else
  begin
    ExactFields(AValue, ['version', 'camera', 'grid', 'scaling', 'layers', 'items']);
  end;
  LVersion := AValue.Field('version').AsInteger;

  if (NyxUTF8ByteCount(AValue.ToJSON) > MaximumNyxCanvasDefinitionBytes) or
    not (LVersion in [1, 2, 3]) or (LHasConnections <> (LVersion >= 2)) or
    (LHasGraphNodes <> (LVersion = 3)) then
  begin
    raise ENyxCanvasScene.Create('Saved canvas descriptor exceeds budget or version support');
  end;
  LCamera := AValue.Field('camera');
  ExactFields(LCamera, ['width', 'height', 'centerX', 'centerY', 'zoom',
    'minimumZoom', 'maximumZoom']);
  LGrid := AValue.Field('grid');
  ExactFields(LGrid, ['style', 'snap', 'spacing', 'originX', 'originY']);
  LText := LGrid.Field('style').AsText;
  LStyle := ncgsNone;

  if LText = 'dots' then
  begin
    LStyle := ncgsDots;
  end
  else if LText = 'lines' then
  begin
    LStyle := ncgsLines;
  end
  else if LText <> 'none' then
  begin
    raise ENyxCanvasScene.Create('Unknown saved grid style');
  end;
  LText := LGrid.Field('snap').AsText;
  LSnap := ncsmFree;

  if LText = 'grid' then
  begin
    LSnap := ncsmGrid;
  end
  else if LText <> 'free' then
  begin
    raise ENyxCanvasScene.Create('Unknown saved grid snapping');
  end;
  LText := AValue.Field('scaling').AsText;
  LScaling := nccsReflow;

  if LText = 'logical-size' then
  begin
    LScaling := nccsLogicalSize;
  end
  else if LText <> 'reflow' then
  begin
    raise ENyxCanvasScene.Create('Unknown saved live control scaling');
  end;
  Result := NewNyxCanvasDefinition
    .WithCamera(NyxCanvasCamera(LCamera.Field('width').AsNumber, LCamera.Field('height').AsNumber,
      NyxCanvasPoint(LCamera.Field('centerX').AsNumber, LCamera.Field('centerY').AsNumber),
      LCamera.Field('zoom').AsNumber,
      NyxCanvasZoomLimits(LCamera.Field('minimumZoom').AsNumber, LCamera.Field('maximumZoom').AsNumber)))
    .WithGrid(NyxCanvasGrid(LGrid.Field('spacing').AsNumber, LStyle, LSnap)
      .AtOrigin(NyxCanvasPoint(LGrid.Field('originX').AsNumber, LGrid.Field('originY').AsNumber)))
    .WithScaling(LScaling);
  LLayers := AValue.Field('layers');
  LItems := AValue.Field('items');

  if (LLayers.Kind <> ndArray) or (LItems.Kind <> ndArray) or
    (LLayers.Count > MaximumNyxCanvasLayers) or (LItems.Count > MaximumNyxCanvasItems) then
  begin
    raise ENyxCanvasScene.Create('Saved canvas arrays exceed their declared capacities');
  end;
  for LIndex := 0 to LLayers.Count - 1 do
  begin
    LValue := LLayers.Item(LIndex);
    ExactFields(LValue, ['name', 'visible', 'pickable']);
    Result := Result.WithLayer(NyxCanvasLayer(LValue.Field('name').AsText),
      LValue.Field('visible').AsBoolean, LValue.Field('pickable').AsBoolean);
  end;
  for LIndex := 0 to LItems.Count - 1 do
  begin
    LValue := LItems.Item(LIndex);
    ExactFields(LValue, ['name', 'rootKind', 'root', 'layer', 'left', 'top', 'width',
      'height', 'visible', 'pickable']);
    LText := LValue.Field('rootKind').AsText;

    if LText = 'page' then
    begin
      LRoot := NyxPageRoot(LValue.Field('root').AsText);
    end
    else if LText = 'component' then
    begin
      LRoot := NyxReusableRoot(LValue.Field('root').AsText);
    end
    else
    begin
      raise ENyxCanvasScene.Create('Saved canvas content requires an exact root partition');
    end;
    Result := Result.WithItem(NyxCanvasItem(LValue.Field('name').AsText), LRoot,
      NyxCanvasPlacement(NyxCanvasLayer(LValue.Field('layer').AsText),
        NyxCanvasBounds(LValue.Field('left').AsNumber, LValue.Field('top').AsNumber,
          LValue.Field('width').AsNumber, LValue.Field('height').AsNumber))
        .Visible(LValue.Field('visible').AsBoolean).Pickable(LValue.Field('pickable').AsBoolean));
  end;

  if LHasConnections then
  begin
    LConnectionData := AValue.Field('connections');

    if (LConnectionData.Kind <> ndArray) or
      (LConnectionData.Count > MaximumNyxCanvasConnections) then
    begin
      raise ENyxCanvasScene.Create('Saved canvas connection array exceeds its declared capacity');
    end;
    SetLength(LConnections, LConnectionData.Count);
    for LIndex := 0 to High(LConnections) do
    begin
      LConnections[LIndex] := TNyxCanvasRouteChange.FromData(LConnectionData.Item(LIndex));
    end;

    if Length(LConnections) > 0 then
    begin
      Result := NyxCanvasConnections(Result).Controls(LConnections).Done;
    end;
  end;

  if LHasGraphNodes then
  begin
    LGraphData := AValue.Field('graphNodes');

    if (LGraphData.Kind <> ndArray) or (LGraphData.Count > MaximumNyxCanvasGraphNodes) then
    begin
      raise ENyxCanvasScene.Create('Saved graph-node array exceeds its declared capacity');
    end;
    SetLength(LGraphNodes, LGraphData.Count);
    for LIndex := 0 to High(LGraphNodes) do
    begin
      LValue := LGraphData.Item(LIndex);
      ExactFields(LValue, ['graph', 'step', 'x', 'y']);
      LGraphNodes[LIndex] := NyxCanvasGraphNode(NyxFlowRef(LValue.Field('graph').AsText),
        NyxFlowStep(LValue.Field('step').AsText),
        NyxCanvasPoint(LValue.Field('x').AsNumber, LValue.Field('y').AsNumber));
    end;

    if Length(LGraphNodes) > 0 then
    begin
      Result := NyxCanvasGraphLayout(Result).Nodes(LGraphNodes).Done;
    end;
  end;
end;

function NewNyxCanvasDeclarations: INyxCanvasDeclarations;
begin
  Result := TCanvasDeclarations.Create;
end;

function TCanvasDeclarations.IndexOf(const AReference: TNyxCanvasRef): Integer;
var
  LName: TNyxText;
begin
  LName := AReference.Name;
  for Result := 0 to High(FReferences) do
  begin

    if FReferences[Result].Name = LName then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

function TCanvasDeclarations.Define(const AReference: TNyxCanvasRef;
  const ADefinition: INyxCanvasDefinition): INyxCanvasDeclarations;
var
  LDefinition: INyxCanvasDefinition;
  LIndex: Integer;
  LReferences: array of TNyxCanvasRef;
  LDefinitions: array of INyxCanvasDefinition;
begin
  LIndex := IndexOf(AReference);

  if ADefinition = nil then
  begin
    raise ENyxCanvasScene.Create('Saved canvas definition is required');
  end;

  if (LIndex < 0) and (Length(FReferences) = MaximumNyxCanvasDefinitions) then
  begin
    raise ENyxCanvasScene.Create('Canvas definition registry is full');
  end;
  LDefinition := NyxCanvasDefinitionFromData(ADefinition.ToData);

  if LIndex >= 0 then
  begin
    FDefinitions[LIndex] := LDefinition;
  end
  else
  begin
    LReferences := Copy(FReferences, 0, Length(FReferences));
    LDefinitions := Copy(FDefinitions, 0, Length(FDefinitions));
    SetLength(LReferences, Length(LReferences) + 1);
    SetLength(LDefinitions, Length(LDefinitions) + 1);
    LReferences[High(LReferences)] := AReference;
    LDefinitions[High(LDefinitions)] := LDefinition;
    FReferences := LReferences;
    FDefinitions := LDefinitions;
  end;
  Result := Self;
end;

function TCanvasDeclarations.Remove(const AReference: TNyxCanvasRef): INyxCanvasDeclarations;
var
  LIndex: Integer;
  LPosition: Integer;
  LReferences: array of TNyxCanvasRef;
  LDefinitions: array of INyxCanvasDefinition;
begin
  LPosition := IndexOf(AReference);

  if LPosition < 0 then
  begin
    raise ENyxCanvasScene.Create('Saved canvas does not exist');
  end;
  SetLength(LReferences, Length(FReferences) - 1);
  SetLength(LDefinitions, Length(FDefinitions) - 1);
  for LIndex := 0 to LPosition - 1 do
  begin
    LReferences[LIndex] := FReferences[LIndex];
    LDefinitions[LIndex] := FDefinitions[LIndex];
  end;
  for LIndex := LPosition to High(LReferences) do
  begin
    LReferences[LIndex] := FReferences[LIndex + 1];
    LDefinitions[LIndex] := FDefinitions[LIndex + 1];
  end;
  FReferences := LReferences;
  FDefinitions := LDefinitions;
  Result := Self;
end;

function TCanvasDeclarations.Contains(const AReference: TNyxCanvasRef): Boolean;
begin
  Result := IndexOf(AReference) >= 0;
end;

function TCanvasDeclarations.Definition(const AReference: TNyxCanvasRef): INyxCanvasDefinition;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AReference);

  if LIndex < 0 then
  begin
    raise ENyxCanvasScene.Create('Saved canvas does not exist');
  end;
  Result := FDefinitions[LIndex];
end;

function TCanvasDeclarations.Reference(AIndex: Integer): TNyxCanvasRef;
begin

  if (AIndex < 0) or (AIndex >= Length(FReferences)) then
  begin
    raise ENyxCanvasScene.Create('Canvas registry index is outside its definitions');
  end;
  Result := FReferences[AIndex];
end;

function TCanvasDeclarations.GetCount: Integer;
begin
  Result := Length(FReferences);
end;

function TCanvasDeclarations.Clone: INyxCanvasDeclarations;
var
  LClone: TCanvasDeclarations;
  LIndex: Integer;
begin
  LClone := TCanvasDeclarations.Create;
  Result := LClone;
  SetLength(LClone.FReferences, Length(FReferences));
  SetLength(LClone.FDefinitions, Length(FDefinitions));
  for LIndex := 0 to High(FReferences) do
  begin
    LClone.FReferences[LIndex] := FReferences[LIndex];
    LClone.FDefinitions[LIndex] := FDefinitions[LIndex];
  end;
end;

function TCanvasDeclarations.ToData: TNyxDataValue;
var
  LValues: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LValues, Length(FReferences));
  for LIndex := 0 to High(FReferences) do
  begin
    LValues[LIndex] := NyxObject([
      NyxField('name', NyxData(FReferences[LIndex].Name)),
      NyxField('definition', FDefinitions[LIndex].ToData)]);
  end;
  Result := NyxArray(LValues);
end;

function NyxCanvasDeclarationsFromData(const AValue: TNyxDataValue): INyxCanvasDeclarations;
var
  LIndex: Integer;
  LValue: TNyxDataValue;
  LReference: TNyxCanvasRef;
begin

  if (AValue.Kind <> ndArray) or (AValue.Count > MaximumNyxCanvasDefinitions) then
  begin
    raise ENyxCanvasScene.Create('Saved canvases require a bounded declaration array');
  end;
  Result := NewNyxCanvasDeclarations;
  for LIndex := 0 to AValue.Count - 1 do
  begin
    LValue := AValue.Item(LIndex);
    ExactFields(LValue, ['name', 'definition']);
    LReference := NyxCanvasRef(LValue.Field('name').AsText);

    if Result.Contains(LReference) then
    begin
      raise ENyxCanvasScene.Create('Duplicate saved canvas identity');
    end;
    Result.Define(LReference, NyxCanvasDefinitionFromData(LValue.Field('definition')));
  end;
end;

{$I nyx.canvas.connections.inc}
{$I nyx.canvas.graph.layout.inc}

end.
