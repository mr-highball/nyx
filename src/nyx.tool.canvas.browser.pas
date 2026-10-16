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
  SysUtils, JS, Web, nyx.text, nyx.model, nyx.state, nyx.theme,
  nyx.behavior, nyx.canvas.workspace, nyx.tool.canvas, nyx.render.browser,
  nyx.scheduler, nyx.content.mount, nyx.events, nyx.gestures, nyx.types;

type
  { Target objects are borrowed until Disconnect. The public controller/form
    owns no renderer and is shared by both adapters. }
  INyxBrowserToolCanvasView = interface(INyxToolCanvasView)
    ['{D1411014-6120-4C84-A708-000000000022}']
    function GetRenderer: TNyxBrowserRenderer;
    property Renderer: TNyxBrowserRenderer read GetRenderer;
  end;

function MountNyxBrowserToolCanvas(ADocument: TNyxDocument;
  const ACanvas: INyxCanvasWorkspaceView; AHost: TJSHTMLElement;
  AState: TNyxState; ATheme: TNyxTheme): INyxBrowserToolCanvasView;

implementation

type
  { UI-owned face and revocable callbacks. Gesture subscriptions borrow this
    owner; Disconnect revokes them before any physical renderer is released. }
  TToolView = class(TInterfacedObject, INyxToolCanvasView, INyxBrowserToolCanvasView)
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
    procedure ClearGestures;
    procedure BindGestures;
    procedure QueueRefresh;
    procedure Gesture(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  public
    constructor Create(ADocument: TNyxDocument; const ACanvas: INyxCanvasWorkspaceView;
      AHost: TJSHTMLElement; AState: TNyxState; ATheme: TNyxTheme);
    destructor Destroy; override;
    procedure Disconnect;
    procedure Refresh;
    function GetInspector: INyxToolCanvasInspector;
    function GetRefreshExecution: INyxExecution;
    function GetRenderer: TNyxBrowserRenderer;
  end;

{$I nyx.tool.canvas.gestures.inc}

constructor TToolView.Create(ADocument: TNyxDocument;
  const ACanvas: INyxCanvasWorkspaceView; AHost: TJSHTMLElement;
  AState: TNyxState; ATheme: TNyxTheme);
var
  LID: TNyxText;
  LSerial: Integer;
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
  Refresh;
  FConstructing := False;
end;

destructor TToolView.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TToolView.Disconnect;
begin
  FConnected := False;
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
  Result := TToolView.Create(ADocument, ACanvas, AHost, AState, ATheme);
end;

end.
