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


unit nyx.tool.canvas.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, JS, Web, nyx.text, nyx.model, nyx.state, nyx.theme,
  nyx.behavior, nyx.canvas.workspace, nyx.tool.canvas, nyx.render.browser,
  nyx.scheduler, nyx.content.mount, nyx.events, nyx.gestures, nyx.gestures.browser, nyx.types,
  nyx.canvas.observers, nyx.canvas.geometry, nyx.canvas.browser, nyx.tool.wires,
  nyx.drawing, nyx.drawing.browser, nyx.drawing.view, nyx.design.tokens,
  nyx.data, nyx.flow.types, nyx.flow.declarations, nyx.canvas.adoption, nyx.controls;

type
  { Target objects are borrowed until Disconnect. The public controller/form
    owns no renderer and is shared by both adapters. }
  INyxBrowserToolCanvasView = interface(INyxToolCanvasView)
    ['{D1411014-6120-4C84-A708-000000000022}']
    function GetRenderer: TNyxBrowserRenderer;
    property Renderer: TNyxBrowserRenderer read GetRenderer;
  end;

  { Optional target inspection for independently mounted ordinary Nyx connector
    faces. Returned renderers are borrowed until the next face rebuild or
    Disconnect; queued geometry updates retain the existing renderer/editor.
    These target handles grant no document or application execution authority. }
  INyxBrowserToolCanvasConnectors = interface(IInterface)
    ['{CC4D1722-D0CC-456D-B71A-ED92E42DA4F4}']
    function ControlConnectorRenderer(const AControl: TNyxControlRef): TNyxBrowserRenderer;
    function PipelineConnectorRenderer: TNyxBrowserRenderer;
    function GetConnectorExecution: INyxExecution;
    property ConnectorExecution: INyxExecution read GetConnectorExecution;
  end;


  { Borrowed DOM-backed renderer inspection for one mounted operation face. The
    renderer and its controls expire on face rebuild or Disconnect; this grants
    neither graph mutation nor document/application lifetime authority. }
  INyxBrowserToolPipelineNodes = interface(IInterface)
    ['{8D1EBC25-A5D3-4945-A5B2-1A1016000013}']
    function PipelineNodeRenderer(const AStep: TNyxFlowStepRef): TNyxBrowserRenderer;
  end;

function MountNyxBrowserToolCanvas(ADocument: TNyxDocument;
  const ACanvas: INyxCanvasWorkspaceView; AHost: TJSHTMLElement;
  AState: TNyxState; ATheme: TNyxTheme): INyxBrowserToolCanvasView; overload;
{ Explicit project persistence host. The original overload remains runtime-only;
  source/history authority is never inferred from the borrowed widget host. }
function MountNyxBrowserToolCanvas(ADocument: TNyxDocument;
  const ACanvas: INyxCanvasWorkspaceView; AHost: TJSHTMLElement;
  AState: TNyxState; ATheme: TNyxTheme; const AOwner: TNyxControlRef;
  const AAdoptionHost: INyxCanvasRouteAdoptionHost): INyxBrowserToolCanvasView; overload;

implementation

type
  { UI-owned face and revocable callbacks. Gesture subscriptions borrow this
    owner; Disconnect revokes them before any physical renderer is released. }
  TToolView = class(TInterfacedObject, INyxToolCanvasView, INyxBrowserToolCanvasView,
    INyxBrowserToolCanvasConnectors, INyxToolCanvasConnectorPresentation,
    INyxToolCanvasPipelineNodes, INyxBrowserToolPipelineNodes)
  private
    FInspector: INyxToolCanvasInspector;
    FRenderer: TNyxBrowserRenderer;
    FHost: TJSHTMLElement;
    FState: TNyxState;
    FConnected: Boolean;
    FConstructing: Boolean;
    FUpdating: Boolean;
    FRefreshWork: INyxContentWork;
    FRefreshExecution: INyxExecution;
    FQueuedUpdate: TNyxToolCanvasUpdate;
    FRenderedBlueprint: INyxNode;
    FGestureCallbacks: array of TNyxEventCallback;
    FGestureSubscriptions: array of INyxEventSubscription;

    FCanvas: INyxBrowserCanvasWorkspaceView;
    FConnectorGeometry: INyxCanvasControlGeometry;
    FConnectorObserver: TObject;
    FConnectorObserverLease: INyxCanvasPresentationObserver;
    FConnectorScope: INyxCancellationScope;
    FConnectorWork: INyxContentWork;
    FConnectorExecution: INyxExecution;
    FConnectorFaces: array of TObject;
    FConnectorDocument: TNyxDocument;
    FConnectorController: INyxToolControlWires;
    FConnectorNodeMode: Boolean;
    FConnectorNodeMaximum: Integer;
    FConnectorNodePortMaximum: Integer;
    FConnectorNodes: INyxToolPipelineNodes;
    FConnectorScheduler: INyxScheduler;
    FConnectorNodePositions: array of TNyxToolPipelineNodePosition;
    FConnectorNodeMessage: TNyxText;
    FConnectorPointerNode: TNyxFlowStepRef;
    FConnectorNodeStart: TNyxCanvasPoint;
    FConnectorNodeCamera: TNyxCanvasCamera;
    FConnectorTheme: TNyxTheme;
    FConnectorRebuild: Boolean;
    FConnectorUpdating: Boolean;
    FConnectorRects: array of TNyxCanvasBounds;
    FConnectorPointerActive: Boolean;
    FConnectorPointerID: Integer;
    FConnectorPointerOrigin: TNyxText;
    FConnectorPointerSource: Integer;
    FConnectorPointerMoved: Boolean;
    FConnectorPointerPosition: TNyxCanvasPoint;
    FConnectorPointerStart: TNyxCanvasPoint;
    FConnectorPanelStart: TNyxCanvasPoint;
    FConnectorPanelPosition: TNyxCanvasPoint;
    FConnectorPanelBounds: TNyxCanvasBounds;
    FConnectorRestoreFocus: TNyxText;
    FConnectorLineHost: TJSHTMLElement;
    FConnectorDrawing: TNyxBrowserDrawing;
    procedure InitializeConnectors(const ACanvas: INyxCanvasWorkspaceView;
      ADocument: TNyxDocument; ATheme: TNyxTheme);
    procedure DisconnectConnectors;
    procedure QueueConnectors(ARebuild: Boolean);
    procedure RefreshConnectors;
    procedure ClearConnectorFaces;
    procedure ReleaseConnectorPointer;
    procedure PlaceConnectorFaces;
    procedure ConnectorFaceSize(AFace: TObject; out AWidth, AHeight: Double);
    procedure PaintConnectors;
    procedure ConnectorEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure ConnectorGesture(AFace: TObject; const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution);
    function NodePosition(AIndex: Integer): Integer;
    procedure RequirePipelineUI;
    procedure NodeGesture(AFace: TObject; const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution);
    function NodeEndpointBounds(const AEndpoint: TNyxFlowEndpoint;
      out ABounds: TNyxCanvasBounds): Boolean;
    procedure ClearGestures;
    procedure BindGestures;
    procedure QueueRefresh;
    procedure AdoptionChanged;
    procedure Gesture(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  public
    constructor Create(ADocument: TNyxDocument; const ACanvas: INyxCanvasWorkspaceView;
      AHost: TJSHTMLElement; AState: TNyxState; ATheme: TNyxTheme;
      const AOwner: TNyxControlRef; const AAdoptionHost: INyxCanvasRouteAdoptionHost);
    destructor Destroy; override;
    procedure Disconnect;
    procedure Refresh;
    function GetInspector: INyxToolCanvasInspector;
    function GetRefreshExecution: INyxExecution;
    function GetRenderer: TNyxBrowserRenderer;
    function ControlConnectorRenderer(const AControl: TNyxControlRef): TNyxBrowserRenderer;
    function PipelineConnectorRenderer: TNyxBrowserRenderer;
    function GetConnectorExecution: INyxExecution;
    procedure MovePipeline(const APosition: TNyxCanvasPoint);
    procedure ResetPipelinePosition;
    function GetPipelineBounds: TNyxCanvasBounds;
    procedure ShowPipelineNodes(AVisible: Boolean = True;
      AMaximumNodes: Integer = 128; AMaximumPorts: Integer = 4096);
    function GetPipelineNodesVisible: Boolean;
    procedure MovePipelineNode(const AStep: TNyxFlowStepRef; const AWorld: TNyxCanvasPoint);
    procedure ResetPipelineNode(const AStep: TNyxFlowStepRef);
    function PipelineNodeBounds(const AStep: TNyxFlowStepRef): TNyxCanvasBounds;
    function ExportPipelineNodePositions: TNyxDataValue;
    function PipelineNodeRenderer(const AStep: TNyxFlowStepRef): TNyxBrowserRenderer;
  end;

{$I nyx.tool.canvas.gestures.inc}
{$I nyx.tool.canvas.connectors.inc}
{$I nyx.tool.canvas.adoption.view.inc}

constructor TToolView.Create(ADocument: TNyxDocument;
  const ACanvas: INyxCanvasWorkspaceView; AHost: TJSHTMLElement;
  AState: TNyxState; ATheme: TNyxTheme; const AOwner: TNyxControlRef;
  const AAdoptionHost: INyxCanvasRouteAdoptionHost);
var
  LID: TNyxText;
  LSerial: Integer;
  LPersistence: INyxToolCanvasRoutePersistence;
begin
  inherited Create;
  FConstructing := True;

  if (ADocument = nil) or (ACanvas = nil) or (AHost = nil) or (AState = nil) then
  begin
    raise ENyxModel.Create('Tool Canvas face requires source, canvas, host and runtime state');
  end;
  FHost := AHost;
  FState := AState;
  LID := 'tool-port-inspector';
  LSerial := 0;
  while ADocument.Find(LID) <> nil do
  begin
    Inc(LSerial);
    LID := 'tool-port-inspector-' + IntToStr(LSerial);
  end;
  FInspector := NewNyxToolCanvasInspector(LID, ADocument, ACanvas);
  FRenderer := TNyxBrowserRenderer.Create(ATheme);
  FRenderer.OnEvent := @Event;
  FConnected := True;

  if (AAdoptionHost <> nil) and
    Supports(FInspector, INyxToolCanvasRoutePersistence, LPersistence) then
  begin
    LPersistence.ConfigureAdoption(AOwner, AAdoptionHost, @AdoptionChanged);
  end;
  Refresh;
  FConstructing := False;
  InitializeConnectors(ACanvas, ADocument, ATheme);
end;

destructor TToolView.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TToolView.Disconnect;
begin
  FConnected := False;
  DisconnectConnectors;
  ClearGestures;

  if FRefreshWork <> nil then
  begin
    FRefreshWork.Retire;
    FRefreshWork := nil;
  end;

  if FRefreshExecution <> nil then
  begin
    FRefreshExecution.Cancel;
  end;

  if FInspector <> nil then
  begin
    FInspector.Disconnect;
  end;
  FreeAndNil(FRenderer);
  FInspector := nil;
  FRenderedBlueprint := nil;
  FHost := nil;
  FState := nil;
end;

procedure TToolView.Refresh;
var
  LLease: INyxToolCanvasView;
  LUpdate: TNyxToolCanvasUpdate;
  LRoot: INyxNode;
begin

  if not FConnected or FUpdating then
  begin
    Exit;
  end;

  if not FConstructing then
  begin
    LLease := Self;
  end;
  FUpdating := True;
  LUpdate := FQueuedUpdate;
  FQueuedUpdate := ntcuNone;
  try
    LRoot := FInspector.Root;

    if (LUpdate = ntcuSynchronize) and ((FRenderedBlueprint = nil) or
      (FRenderedBlueprint.Node <> LRoot.Node)) then
    begin
      LUpdate := ntcuRebuild;
    end;

    if LUpdate = ntcuSynchronize then
    begin
      FRenderer.Sync;
    end
    else
    begin
      ClearGestures;
      FRenderer.Render(FInspector.Document, LRoot.Node, FHost, False, FState);
      FRenderedBlueprint := LRoot;
    end;
  finally
    try

      if LUpdate <> ntcuSynchronize then
      begin
        BindGestures;
      end;
    finally
      FUpdating := False;
      FConnectorRebuild := FConnectorRebuild or (LUpdate <> ntcuSynchronize);
      RefreshConnectors;
    end;
  end;
end;

procedure TToolView.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LLease: INyxToolCanvasView;
  LInput: INyxToolCanvasInput;
  LUpdate: TNyxToolCanvasUpdate;
begin

  if not FConnected or FUpdating then
  begin
    Exit;
  end;
  LLease := Self;
  LUpdate := ntcuNone;

  if Supports(FInspector, INyxToolCanvasInput, LInput) then
  begin
    LUpdate := LInput.HandleInput(ANode, FRenderer.Root, AEvent);
  end
  else if FInspector.Handle(ANode, AEvent) then
  begin
    LUpdate := ntcuRebuild;
  end;

  if (LUpdate <> ntcuNone) and FConnected then
  begin

    if Ord(LUpdate) > Ord(FQueuedUpdate) then
    begin
      FQueuedUpdate := LUpdate;
    end;
    { Checkbox/select notifications still use their control after OnChange.
      Publish on a real later UI turn, coalescing related form edits. The queue
      holds a revocable work port, never this view/renderer or its widgets. }

    if FRefreshWork = nil then
    begin
      FRefreshWork := NewNyxContentWork(@Refresh);
    end;

    if (FRefreshExecution = nil) or
      not (FRefreshExecution.Status in [nesPending, nesRunning]) then
    begin
      FRefreshExecution := FRenderer.Events.Scheduler.Submit(FRefreshWork, neUIQueue);
    end;
  end;
end;

function TToolView.GetRefreshExecution: INyxExecution;
begin
  Result := FRefreshExecution;
end;

function TToolView.GetInspector: INyxToolCanvasInspector;
begin

  if not FConnected then
  begin
    raise ENyxModel.Create('Tool Canvas inspector is retired');
  end;
  Result := FInspector;
end;

function TToolView.GetRenderer: TNyxBrowserRenderer;
begin

  if not FConnected then
  begin
    raise ENyxModel.Create('Tool Canvas renderer is retired');
  end;
  Result := FRenderer;
end;

function MountNyxBrowserToolCanvas(ADocument: TNyxDocument;
  const ACanvas: INyxCanvasWorkspaceView; AHost: TJSHTMLElement;
  AState: TNyxState; ATheme: TNyxTheme): INyxBrowserToolCanvasView;
begin
  Result := TToolView.Create(ADocument, ACanvas, AHost, AState, ATheme,
    Default(TNyxControlRef), nil);
end;

function MountNyxBrowserToolCanvas(ADocument: TNyxDocument;
  const ACanvas: INyxCanvasWorkspaceView; AHost: TJSHTMLElement;
  AState: TNyxState; ATheme: TNyxTheme; const AOwner: TNyxControlRef;
  const AAdoptionHost: INyxCanvasRouteAdoptionHost): INyxBrowserToolCanvasView;
begin
  Result := TToolView.Create(ADocument, ACanvas, AHost, AState, ATheme, AOwner, AAdoptionHost);
end;

end.
