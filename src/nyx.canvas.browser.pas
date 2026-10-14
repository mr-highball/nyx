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
unit nyx.canvas.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, JS, Web, nyx.text, nyx.types, nyx.model, nyx.state,
  nyx.theme, nyx.events, nyx.scheduler, nyx.render.browser,
  nyx.canvas.geometry, nyx.canvas.scene, nyx.canvas.workspace;

type
  TNyxCanvasBrowserRendererSetup = procedure(ARenderer: TNyxBrowserRenderer) of object;

  INyxBrowserCanvasWorkspaceView = interface(INyxCanvasWorkspaceView)
    ['{737A7921-4621-4C6F-8C01-0CA000000013}']
    function GetSurface: TJSHTMLElement;
    function RendererFor(const AItem: TNyxCanvasItemRef): TNyxBrowserRenderer;
    function InputFor(const AItem: TNyxCanvasItemRef;
      const AControl: TNyxControlRef): TJSHTMLElement;
    { Borrow DOM/renderer only until Disconnect. Surface is a keyboard-accessible
      region; ordinary embedded controls retain their own native browser input. }
    property Surface: TJSHTMLElement read GetSurface;
  end;

{ Same ownership and live-control scaling contract as the LCL mount. Document/
  theme are copied; host/store/setup receiver are borrowed. An explicit mount
  is required for this runtime composition until document/source integration. }
function MountNyxBrowserCanvasWorkspace(const AWorkspace: INyxCanvasWorkspace;
  ADocument: TNyxDocument; AHost: TJSHTMLElement; AState: TNyxState = nil;
  ATheme: TNyxTheme = nil;
  ASetup: TNyxCanvasBrowserRendererSetup = nil): INyxBrowserCanvasWorkspaceView;

implementation

uses
  nyx.design.tokens, nyx.behavior, nyx.gestures.browser;

type
  TCanvasItemMount = class
  public
    Reference: TNyxCanvasItemRef;
    Content: INyxNode;
    Host: TJSHTMLElement;
    Document: TNyxDocument;
    Renderer: TNyxBrowserRenderer;
    destructor Destroy; override;
  end;

  TCanvasMounts = array of TCanvasItemMount;
  TCanvasFrames = array of TNyxCanvasControlFrame;

  TCanvasContact = record
    ID: NativeInt;
    Start: TNyxCanvasPoint;
    Current: TNyxCanvasPoint;
  end;

  TCanvasView = class(TInterfacedObject,
    INyxCanvasWorkspaceView, INyxBrowserCanvasWorkspaceView)
  private
    FWorkspace: INyxCanvasWorkspace;
    FDocument: TNyxDocument;
    FState: TNyxState;
    FOwnState: Boolean;
    FTheme: TNyxTheme;
    FSetup: TNyxCanvasBrowserRendererSetup;
    FChrome: TNyxBrowserRenderer;
    FScheduler: INyxScheduler;
    FSurface: TJSHTMLElement;
    FBackground: TJSHTMLElement;
    FObserver: TJSHTMLResizeObserver;
    FCamera: TNyxCanvasCamera;
    FGrid: TNyxCanvasGrid;
    FScaling: TNyxCanvasControlScaling;
    FSnapshot: INyxCanvasSnapshot;
    FMounts: TCanvasMounts;
    FConnected: Boolean;
    FConstructing: Boolean;
    { A pas2js factory owns Self only after constructor staging has finished. }
    FUpdating: Boolean;
    FDisconnectPending: Boolean;
    FContacts: array[0..1] of TCanvasContact;
    FContactCount: Integer;
    FStartCamera: TNyxCanvasCamera;
    FGestureCamera: TNyxCanvasCamera;
    function FindMount(const AItem: TNyxCanvasItemRef): Integer;
    function PrepareMount(const AItem: TNyxCanvasItemRef;
      const AContent: INyxNode; const AFrame: TNyxCanvasControlFrame): TCanvasItemMount;
    procedure Present(const ACamera: TNyxCanvasCamera; ARefresh: Boolean);
    procedure ChromeEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure BoundsChanged(AEntries: TJSHTMLResizeObserverEntryArray;
      AObserver: TJSHTMLResizeObserver);
    function LocalPoint(AX, AY: Double): TNyxCanvasPoint;
    function Down(AEvent: TJSPointerEvent): Boolean;
    function Move(AEvent: TJSPointerEvent): Boolean;
    function Up(AEvent: TJSPointerEvent): Boolean;
    function Cancel(AEvent: TJSPointerEvent): Boolean;
    function Wheel(AEvent: TJSWheelEvent): Boolean;
    function Keyboard(AEvent: TJSKeyboardEvent): Boolean;
    procedure CancelPan;
    procedure RequireConnected;
    procedure PaintGrid;
  public
    constructor Create(const AWorkspace: INyxCanvasWorkspace;
      ADocument: TNyxDocument; AHost: TJSHTMLElement; AState: TNyxState;
      ATheme: TNyxTheme; ASetup: TNyxCanvasBrowserRendererSetup);
    destructor Destroy; override;
    function GetSurface: TJSHTMLElement;
    function GetConnected: Boolean;
    function GetCamera: TNyxCanvasCamera;
    function GetMountedCount: Integer;
    procedure SetCamera(const ACamera: TNyxCanvasCamera);
    procedure PanBy(AX, AY: Double);
    procedure ZoomAt(AZoom: Double; const AAnchor: TNyxCanvasPoint);
    procedure Execute(ACommand: TNyxCanvasCommand);
    procedure Refresh;
    function EventsFor(const AItem: TNyxCanvasItemRef): INyxEvents;
    function RendererFor(const AItem: TNyxCanvasItemRef): TNyxBrowserRenderer;
    function InputFor(const AItem: TNyxCanvasItemRef;
      const AControl: TNyxControlRef): TJSHTMLElement;
    procedure Disconnect;
  end;

function Metric(AValue: Double): TNyxText;
var
  LFormat: TFormatSettings;
begin
  LFormat := FormatSettings;
  LFormat.DecimalSeparator := '.';
  Result := FloatToStr(AValue, LFormat) + 'px';
end;

procedure SetVisible(AHost: TJSHTMLElement; AVisible: Boolean);
begin

  if AVisible then
  begin
    AHost.style.setProperty('display', 'block');
  end
  else
  begin
    AHost.style.setProperty('display', 'none');
  end;
end;

procedure SetFrame(AHost: TJSHTMLElement; const AFrame: TNyxCanvasControlFrame);
begin
  AHost.style.setProperty('left', IntToStr(AFrame.Left) + 'px');
  AHost.style.setProperty('top', IntToStr(AFrame.Top) + 'px');
  AHost.style.setProperty('width', IntToStr(AFrame.Width) + 'px');
  AHost.style.setProperty('height', IntToStr(AFrame.Height) + 'px');
end;

destructor TCanvasItemMount.Destroy;
begin
  Renderer.Free;
  Document.Free;

  if Host <> nil then
  begin
    Host.remove;
  end;
  Content := nil;
  inherited Destroy;
end;

constructor TCanvasView.Create(const AWorkspace: INyxCanvasWorkspace;
  ADocument: TNyxDocument; AHost: TJSHTMLElement; AState: TNyxState;
  ATheme: TNyxTheme; ASetup: TNyxCanvasBrowserRendererSetup);
var
  LHost: TJSHTMLElement;
  LRoot: TNyxNode;
begin
  inherited Create;
  FConstructing := True;

  if (AWorkspace = nil) or (ADocument = nil) or (AHost = nil) then
  begin
    raise ENyxCanvasScene.Create('Browser workspace mount requires workspace, document and host');
  end;
  FWorkspace := AWorkspace;
  FDocument := NewNyxCanvasRenderContext(ADocument, AWorkspace.Node, LRoot);
  FTheme := NewNyxDocumentTheme(FDocument, ATheme);
  FState := AState;
  FOwnState := AState = nil;

  if FOwnState then
  begin
    FState := FDocument.State.Clone;
  end;
  FSetup := ASetup;
  FGrid := AWorkspace.Grid;
  FScaling := AWorkspace.Scaling;
  FChrome := TNyxBrowserRenderer.Create(FTheme);
  FScheduler := FChrome.Events.Scheduler;
  FScheduler.RequireUI;
  FChrome.OnEvent := @ChromeEvent;
  FChrome.Render(FDocument, LRoot, AHost, False, FState);
  LHost := FChrome.ElementFor(NyxCanvasWorkspacePart(AWorkspace.ID, ncwpSurface).ID);
  LHost.style.setProperty('position', 'relative');
  FSurface := TJSHTMLElement(document.createElement('div'));
  FSurface.style.setProperty('position', 'absolute');
  FSurface.style.setProperty('inset', '0');
  FSurface.style.setProperty('overflow', 'hidden');
  FSurface.style.setProperty('touch-action', 'auto');
  FSurface.style.setProperty('outline-offset', '-3px');
  FSurface.tabIndex := 0;
  FSurface.setAttribute('role', 'region');
  FSurface.setAttribute('aria-label',
    'Canvas workspace. Arrow keys pan, plus and minus zoom, Home centers.');
  LHost.appendChild(FSurface);
  { touch-action intersects ancestor policies. Put none on the background
    sibling only; applying it to the common surface would suppress normal
    touch scrolling and selection inside embedded native browser controls. }
  FBackground := TJSHTMLElement(document.createElement('div'));
  FBackground.style.setProperty('position', 'absolute');
  FBackground.style.setProperty('inset', '0');
  FBackground.style.setProperty('touch-action', 'none');
  FBackground.setAttribute('aria-hidden', 'true');
  FSurface.appendChild(FBackground);
  FSurface.addEventListener('pointerdown', @Down);
  FSurface.addEventListener('pointermove', @Move);
  FSurface.addEventListener('pointerup', @Up);
  FSurface.addEventListener('pointercancel', @Cancel);
  FSurface.addEventListener('lostpointercapture', @Cancel);
  FSurface.addEventListener('wheel', @Wheel);
  FSurface.addEventListener('keydown', @Keyboard);
  FCamera := AWorkspace.Camera;
  FConnected := True;
  Refresh;
  FObserver := TJSHTMLResizeObserver.new(@BoundsChanged);
  FObserver.observe(FSurface);
  FConstructing := False;
end;

destructor TCanvasView.Destroy;
begin
  Disconnect;
  FDocument.Free;

  if FOwnState then
  begin
    FState.Free;
  end;
  FTheme.Free;
  FWorkspace := nil;
  inherited Destroy;
end;

function MountNyxBrowserCanvasWorkspace(const AWorkspace: INyxCanvasWorkspace;
  ADocument: TNyxDocument; AHost: TJSHTMLElement; AState: TNyxState;
  ATheme: TNyxTheme; ASetup: TNyxCanvasBrowserRendererSetup): INyxBrowserCanvasWorkspaceView;
begin
  Result := TCanvasView.Create(AWorkspace, ADocument, AHost, AState, ATheme, ASetup);
end;

procedure TCanvasView.RequireConnected;
begin
  FScheduler.RequireUI;

  if not FConnected then
  begin
    raise ENyxCanvasScene.Create('Browser workspace view is disconnected');
  end;
end;

function TCanvasView.FindMount(const AItem: TNyxCanvasItemRef): Integer;
var
  LName: TNyxText;
begin
  LName := AItem.Name;
  for Result := 0 to High(FMounts) do
  begin

    if FMounts[Result].Reference.Name = LName then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

function TCanvasView.PrepareMount(const AItem: TNyxCanvasItemRef;
  const AContent: INyxNode; const AFrame: TNyxCanvasControlFrame): TCanvasItemMount;
var
  LRoot: TNyxNode;
begin
  Result := TCanvasItemMount.Create;
  try
    Result.Reference := AItem;
    Result.Content := AContent;
    Result.Host := TJSHTMLElement(document.createElement('div'));
    Result.Host.style.setProperty('position', 'absolute');
    Result.Host.style.setProperty('overflow', 'hidden');
    Result.Host.style.setProperty('box-sizing', 'border-box');
    { Hidden staging still needs an actual logical allocation. display:none
      would make the normal renderer measure a zero-width host. }
    Result.Host.style.setProperty('visibility', 'hidden');
    SetFrame(Result.Host, AFrame);
    FSurface.appendChild(Result.Host);
    Result.Renderer := TNyxBrowserRenderer.Create(FTheme);

    if Assigned(FSetup) then
    begin
      FSetup(Result.Renderer);
    end;
    Result.Document := NewNyxCanvasRenderContext(FDocument, AContent.Node, LRoot);
    Result.Renderer.Render(Result.Document, LRoot, Result.Host, False, FState);
  except
    Result.Free;
    raise;
  end;
end;

procedure TCanvasView.Present(const ACamera: TNyxCanvasCamera; ARefresh: Boolean);
var
  LKeepAlive: INyxCanvasWorkspaceView;
  LCamera: TNyxCanvasCamera;
  LSnapshot: INyxCanvasSnapshot;
  LFrames: TCanvasFrames;
  LMounts: TCanvasMounts;
  LNew: array of Boolean;
  LRetained: array of Boolean;
  LItem: TNyxCanvasItemInfo;
  LContent: INyxNode;
  LIndex: Integer;
  LFormer: Integer;
  LPublished: Boolean;
  LKeepFocus: Boolean;
begin
  RequireConnected;

  if FUpdating then
  begin
    raise ENyxCanvasScene.Create('Browser workspace presentation is already updating');
  end;

  if (FSurface.clientWidth <= 0) or (FSurface.clientHeight <= 0) then
  begin
    Exit;
  end;

  if not FConstructing then
  begin
    LKeepAlive := Self;
  end;
  LCamera := ACamera.WithViewport(FSurface.clientWidth, FSurface.clientHeight);
  LSnapshot := FSnapshot;

  if ARefresh or (LSnapshot = nil) then
  begin
    LSnapshot := FWorkspace.Scene.Snapshot;
  end;
  SetLength(LFrames, LSnapshot.Count);
  SetLength(LMounts, LSnapshot.Count);
  SetLength(LNew, LSnapshot.Count);
  SetLength(LRetained, Length(FMounts));
  LPublished := False;
  FUpdating := True;
  try
    { Admit every live allocation before changing the camera or any accepted
      face. Only newly needed/replaced mounts stage hidden physical controls. }
    for LIndex := 0 to LSnapshot.Count - 1 do
    begin
      LItem := LSnapshot.Items[LIndex];
      LFrames[LIndex] := NyxCanvasControlFrame(LCamera, LItem.Placement.Bounds,
        FScaling, LItem.Placement.IsVisible);
    end;
    for LIndex := 0 to LSnapshot.Count - 1 do
    begin
      LItem := LSnapshot.Items[LIndex];
      LFormer := FindMount(LItem.Item);
      LContent := FWorkspace.Scene.Control(LItem.Item);

      if (LFormer >= 0) and (FMounts[LFormer].Content.Node = LContent.Node) then
      begin
        LMounts[LIndex] := FMounts[LFormer];
        LRetained[LFormer] := True;
      end
      else if LFrames[LIndex].Visible then
      begin
        LMounts[LIndex] := PrepareMount(LItem.Item, LContent, LFrames[LIndex]);
        LNew[LIndex] := True;
      end;
    end;
    for LIndex := 0 to High(FMounts) do
    begin

      if not LRetained[LIndex] then
      begin
        FMounts[LIndex].Free;
      end;
    end;
    { Keep absent off-screen entries out of the mount vector. Existing faces
      stay retained when culled, preserving field drafts/subscriptions/focus. }
    SetLength(FMounts, 0);
    for LIndex := 0 to High(LMounts) do
    begin

      if LMounts[LIndex] <> nil then
      begin
        SetLength(FMounts, Length(FMounts) + 1);
        FMounts[High(FMounts)] := LMounts[LIndex];
      end;
    end;
    FSnapshot := LSnapshot;
    FCamera := LCamera;
    LPublished := True;
    for LIndex := 0 to High(LMounts) do
    begin

      if LMounts[LIndex] = nil then
      begin
        Continue;
      end;

      if LFrames[LIndex].Visible then
      begin
        SetFrame(LMounts[LIndex].Host, LFrames[LIndex]);

        if not FConnected then
        begin
          Exit;
        end;
        LMounts[LIndex].Host.style.setProperty('z-index', IntToStr(LIndex));
      LMounts[LIndex].Host.style.setProperty('visibility', 'visible');
      end;
      LKeepFocus := LSnapshot.Items[LIndex].Placement.IsVisible and
        LMounts[LIndex].Host.contains(document.activeElement);

      if not LFrames[LIndex].Visible and LKeepFocus then
      begin
        LMounts[LIndex].Host.style.setProperty('left',
          IntToStr(-MaximumNyxCanvasControlExtent) + 'px');
      end;
      SetVisible(LMounts[LIndex].Host, LFrames[LIndex].Visible or LKeepFocus);

      if not FConnected then
      begin
        Exit;
      end;
    end;
    PaintGrid;
  finally

    if not LPublished then
    begin
      for LIndex := 0 to High(LMounts) do
      begin

        if LNew[LIndex] then
        begin
          LMounts[LIndex].Free;
        end;
      end;
    end;
    FUpdating := False;

    if FDisconnectPending then
    begin
      Disconnect;
    end;
  end;
end;


procedure TCanvasView.BoundsChanged(AEntries: TJSHTMLResizeObserverEntryArray;
  AObserver: TJSHTMLResizeObserver);
begin

  if FConnected and not FUpdating then
  begin
    Present(FCamera, False);
  end;
end;

procedure TCanvasView.PaintGrid;
var
  LSpacing: Double;
  LX: Double;
  LY: Double;
begin
  FSurface.style.setProperty('background-color', FTheme.Background);
  FSurface.style.setProperty('background-image', 'none');

  if FGrid.Style = ncgsNone then
  begin
    Exit;
  end;
  LSpacing := FGrid.VisibleSpacing(FCamera) * FCamera.Zoom;
  LX := FCamera.ViewportWidth / 2 +
    (FGrid.Origin.X - FCamera.WorldCenter.X) * FCamera.Zoom;
  LY := FCamera.ViewportHeight / 2 +
    (FGrid.Origin.Y - FCamera.WorldCenter.Y) * FCamera.Zoom;
  LX := LX - Floor(LX / LSpacing) * LSpacing;
  LY := LY - Floor(LY / LSpacing) * LSpacing;

  if FGrid.Style = ncgsLines then
  begin
    FSurface.style.setProperty('background-image',
      'linear-gradient(to right, ' + FTheme.Border + ' 1px, transparent 1px),' +
      'linear-gradient(to bottom, ' + FTheme.Border + ' 1px, transparent 1px)');
  end
  else
  begin
    FSurface.style.setProperty('background-image',
      'radial-gradient(circle at 1px 1px, ' + FTheme.Border + ' 1px, transparent 1.5px)');
  end;
  FSurface.style.setProperty('background-size', Metric(LSpacing) + ' ' + Metric(LSpacing));
  FSurface.style.setProperty('background-position', Metric(LX) + ' ' + Metric(LY));
end;

procedure TCanvasView.ChromeEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LCommand: TNyxCanvasCommand;
begin

  if FConnected and (AEvent.Trigger = ntClick) and
    TryNyxCanvasCommand(FWorkspace.ID, ANode.SourceID, LCommand) then
  begin
    Execute(LCommand);
  end;
end;

function TCanvasView.LocalPoint(AX, AY: Double): TNyxCanvasPoint;
var
  LBounds: TJSDOMRect;
begin
  LBounds := FSurface.getBoundingClientRect;

  if (LBounds.width <= 0) or (LBounds.height <= 0) then
  begin
    raise ENyxCanvasScene.Create('Canvas pointer mapping requires a visible allocation');
  end;
  Result := NyxCanvasPoint((AX - LBounds.left) * FSurface.clientWidth / LBounds.width,
    (AY - LBounds.top) * FSurface.clientHeight / LBounds.height);
end;

function TCanvasView.Down(AEvent: TJSPointerEvent): Boolean;
var
  LKeepAlive: INyxCanvasWorkspaceView;
  LIndex: Integer;
begin
  Result := True;

  if not FConnected or ((AEvent.target <> FSurface) and (AEvent.target <> FBackground)) or
    not (AEvent.button in [0, 1]) or (FContactCount = 2) then
  begin
    Exit;
  end;
  LKeepAlive := Self;
  for LIndex := 0 to FContactCount - 1 do
  begin

    if FContacts[LIndex].ID = AEvent.pointerId then
    begin
      Exit;
    end;
  end;

  if FContactCount = 0 then
  begin
    FGestureCamera := FCamera;
  end;
  FContacts[FContactCount].ID := AEvent.pointerId;
  FContacts[FContactCount].Current := LocalPoint(AEvent.clientX, AEvent.clientY);
  Inc(FContactCount);
  FStartCamera := FCamera;
  for LIndex := 0 to FContactCount - 1 do
  begin
    FContacts[LIndex].Start := FContacts[LIndex].Current;
  end;
  FSurface.focus;

  if not FConnected then
  begin
    Exit;
  end;
  { Synthetic qualification events have no active hardware pointer to capture.
    Trusted active input uses the platform capture contract directly. }

  if AEvent.isTrusted then
  begin
    FSurface.setPointerCapture(AEvent.pointerId);
  end;
  AEvent.preventDefault;
end;

function TCanvasView.Move(AEvent: TJSPointerEvent): Boolean;
var
  LIndex: Integer;
begin
  Result := True;

  if not FConnected then
  begin
    Exit;
  end;
  for LIndex := 0 to FContactCount - 1 do
  begin

    if FContacts[LIndex].ID = AEvent.pointerId then
    begin
      FContacts[LIndex].Current := LocalPoint(AEvent.clientX, AEvent.clientY);

      if FContactCount = 1 then
      begin
        SetCamera(FStartCamera.PanBy(
          FContacts[0].Current.X - FContacts[0].Start.X,
          FContacts[0].Current.Y - FContacts[0].Start.Y));
      end
      else if Sqr(FContacts[0].Current.X - FContacts[1].Current.X) +
        Sqr(FContacts[0].Current.Y - FContacts[1].Current.Y) > 0 then
      begin
        SetCamera(FStartCamera.Pinch(FContacts[0].Start, FContacts[1].Start,
          FContacts[0].Current, FContacts[1].Current));
      end;
      AEvent.preventDefault;
      Exit;
    end;
  end;
end;

function TCanvasView.Up(AEvent: TJSPointerEvent): Boolean;
var
  LIndex: Integer;
begin
  Result := True;

  if not FConnected then
  begin
    Exit;
  end;
  for LIndex := 0 to FContactCount - 1 do
  begin

    if FContacts[LIndex].ID = AEvent.pointerId then
    begin
      Dec(FContactCount);

      if LIndex < FContactCount then
      begin
        FContacts[LIndex] := FContacts[FContactCount];
      end;

      if TNyxGestureElement(FSurface).hasPointerCapture(AEvent.pointerId) then
      begin
        FSurface.releasePointerCapture(AEvent.pointerId);
      end;
      FStartCamera := FCamera;

      if FContactCount > 0 then
      begin
        FContacts[0].Start := FContacts[0].Current;
      end;
      AEvent.preventDefault;
      Exit;
    end;
  end;
end;

function TCanvasView.Cancel(AEvent: TJSPointerEvent): Boolean;
var
  LIndex: Integer;
begin
  Result := True;

  if not FConnected then
  begin
    Exit;
  end;
  for LIndex := 0 to FContactCount - 1 do
  begin

    if FContacts[LIndex].ID = AEvent.pointerId then
    begin
      CancelPan;
      Exit;
    end;
  end;
end;

procedure TCanvasView.CancelPan;
var
  LIDs: array[0..1] of NativeInt;
  LCount: Integer;
  LIndex: Integer;
begin
  LCount := FContactCount;
  for LIndex := 0 to LCount - 1 do
  begin
    LIDs[LIndex] := FContacts[LIndex].ID;
  end;
  FContactCount := 0;
  for LIndex := 0 to LCount - 1 do
  begin

    if TNyxGestureElement(FSurface).hasPointerCapture(LIDs[LIndex]) then
    begin
      FSurface.releasePointerCapture(LIDs[LIndex]);
    end;
  end;

  if FConnected and (LCount > 0) then
  begin
    SetCamera(FGestureCamera);
  end;
end;

function TCanvasView.Wheel(AEvent: TJSWheelEvent): Boolean;
var
  LMultiplier: Double;
  LDelta: Double;
begin
  Result := True;

  if not FConnected or ((AEvent.target <> FSurface) and
    (AEvent.target <> FBackground) and not AEvent.ctrlKey) then
  begin
    Exit;
  end;
  LMultiplier := 1;
  case AEvent.deltaMode of
    1: LMultiplier := 16;
    2: LMultiplier := FCamera.ViewportHeight;
  end;
  LDelta := AEvent.deltaY * LMultiplier;

  if AEvent.ctrlKey then
  begin
    ZoomAt(FCamera.Zoom * Power(1.1, Max(-10, Min(10, -LDelta / 100))),
      LocalPoint(AEvent.clientX, AEvent.clientY));
  end
  else
  begin
    PanBy(-AEvent.deltaX * LMultiplier, -LDelta);
  end;
  AEvent.preventDefault;
end;

function TCanvasView.Keyboard(AEvent: TJSKeyboardEvent): Boolean;
begin
  Result := True;

  if not FConnected or (AEvent.target <> FSurface) then
  begin
    Exit;
  end;
  case AEvent.key of
    'ArrowLeft': PanBy(40, 0);
    'ArrowRight': PanBy(-40, 0);
    'ArrowUp': PanBy(0, 40);
    'ArrowDown': PanBy(0, -40);
    '+', '=': Execute(nccZoomIn);
    '-': Execute(nccZoomOut);
    'Home': Execute(nccHome);
    'Escape': CancelPan;
  else
    Exit;
  end;
  AEvent.preventDefault;
end;

function TCanvasView.GetConnected: Boolean;
begin
  Result := FConnected;
end;

function TCanvasView.GetSurface: TJSHTMLElement;
begin
  RequireConnected;
  Result := FSurface;
end;

function TCanvasView.GetCamera: TNyxCanvasCamera;
begin
  RequireConnected;
  Result := FCamera;
end;

function TCanvasView.GetMountedCount: Integer;
begin
  FScheduler.RequireUI;
  Result := Length(FMounts);
end;

procedure TCanvasView.SetCamera(const ACamera: TNyxCanvasCamera);
begin
  Present(ACamera, False);
end;

procedure TCanvasView.PanBy(AX, AY: Double);
begin
  SetCamera(GetCamera.PanBy(AX, AY));
end;

procedure TCanvasView.ZoomAt(AZoom: Double; const AAnchor: TNyxCanvasPoint);
begin
  SetCamera(GetCamera.ZoomAt(AZoom, AAnchor));
end;

procedure TCanvasView.Execute(ACommand: TNyxCanvasCommand);
var
  LCamera: TNyxCanvasCamera;
begin
  LCamera := GetCamera;
  case Ord(ACommand) of
    Ord(nccZoomOut): SetCamera(LCamera.ZoomAt(LCamera.Zoom / 1.25,
      NyxCanvasPoint(LCamera.ViewportWidth / 2, LCamera.ViewportHeight / 2)));
    Ord(nccZoomIn): SetCamera(LCamera.ZoomAt(LCamera.Zoom * 1.25,
      NyxCanvasPoint(LCamera.ViewportWidth / 2, LCamera.ViewportHeight / 2)));
    Ord(nccFit): SetCamera(FitNyxCanvasScene(LCamera, FSnapshot));
    Ord(nccHome): SetCamera(LCamera.AtCenter(NyxCanvasPoint(0, 0)));
  else
    raise ENyxCanvasScene.Create('Unknown canvas navigation command');
  end;
end;

procedure TCanvasView.Refresh;
begin
  Present(FCamera, True);
end;

function TCanvasView.EventsFor(const AItem: TNyxCanvasItemRef): INyxEvents;
begin
  Result := RendererFor(AItem).Events;
end;

function TCanvasView.RendererFor(const AItem: TNyxCanvasItemRef): TNyxBrowserRenderer;
var
  LIndex: Integer;
begin
  RequireConnected;
  LIndex := FindMount(AItem);

  if LIndex < 0 then
  begin
    raise ENyxCanvasScene.Create('Canvas item has no mounted live controls');
  end;
  Result := FMounts[LIndex].Renderer;
end;

function TCanvasView.InputFor(const AItem: TNyxCanvasItemRef;
  const AControl: TNyxControlRef): TJSHTMLElement;
begin
  Result := RendererFor(AItem).InputFor(AControl.ID);
end;

procedure TCanvasView.Disconnect;
var
  LIndex: Integer;
begin

  if FScheduler <> nil then
  begin
    FScheduler.RequireUI;
  end;
  FConnected := False;
  FContactCount := 0;

  if FUpdating then
  begin
    FDisconnectPending := True;
    Exit;
  end;
  FDisconnectPending := False;

  if FObserver <> nil then
  begin
    FObserver.disconnect;
    FObserver := nil;
  end;

  if FSurface <> nil then
  begin
    FSurface.removeEventListener('pointerdown', @Down);
    FSurface.removeEventListener('pointermove', @Move);
    FSurface.removeEventListener('pointerup', @Up);
    FSurface.removeEventListener('pointercancel', @Cancel);
    FSurface.removeEventListener('lostpointercapture', @Cancel);
    FSurface.removeEventListener('wheel', @Wheel);
    FSurface.removeEventListener('keydown', @Keyboard);
  end;
  for LIndex := 0 to High(FMounts) do
  begin
    FMounts[LIndex].Free;
  end;
  SetLength(FMounts, 0);

  if FSurface <> nil then
  begin
    FSurface.remove;
    FSurface := nil;
    FBackground := nil;
  end;
  FreeAndNil(FChrome);
  FSnapshot := nil;
end;

end.
