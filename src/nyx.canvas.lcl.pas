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
unit nyx.canvas.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, Math, Types, Controls, Forms, Graphics, ExtCtrls,
  nyx.text, nyx.types, nyx.model, nyx.state, nyx.theme, nyx.events,
  nyx.render.lcl, nyx.scheduler, nyx.canvas.geometry, nyx.canvas.scene, nyx.canvas.workspace;

type
  { Configure creator factories/events before each independent item renderer
    mounts. The receiver is borrowed and must outlive this view. }
  TNyxCanvasLCLRendererSetup = procedure(ARenderer: TNyxLCLRenderer) of object;

  INyxLCLCanvasWorkspaceView = interface(INyxCanvasWorkspaceView)
    ['{737A7921-4621-4C6F-8C01-0CA000000012}']
    function GetSurface: TWinControl;
    function RendererFor(const AItem: TNyxCanvasItemRef): TNyxLCLRenderer;
    function InputFor(const AItem: TNyxCanvasItemRef;
      const AControl: TNyxControlRef): TControl;
    { Actual native handles are borrowed until Disconnect. Never Free/reparent
      them. Tests and application adapters can qualify live nested input. }
    property Surface: TWinControl read GetSurface;
  end;

{ Mount ordinary Nyx chrome and live item renderers. Host and a supplied store
  are borrowed; document/theme context is copied. Caller disconnects before host
  or borrowed store disposal. Distinct views own independent cameras and an
  independent default store when no application store is supplied. }
function MountNyxLCLCanvasWorkspace(const AWorkspace: INyxCanvasWorkspace;
  ADocument: TNyxDocument; AHost: TWinControl; AState: TNyxState = nil;
  ATheme: TNyxTheme = nil; ASetup: TNyxCanvasLCLRendererSetup = nil;
  ADesignMode: Boolean = False): INyxLCLCanvasWorkspaceView;

implementation

uses
  nyx.design.tokens, nyx.interaction.policy, nyx.behavior, LCLType, LMessages;
type
  TCanvasView = class;

  TCanvasSurface = class(TCustomControl)
  private
    FView: TCanvasView;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure WndProc(var AMessage: TLMessage); override;
  public
    constructor CreateFor(AOwner: TComponent; AView: TCanvasView);
  end;

  TCanvasItemMount = class
  public
    Reference: TNyxCanvasItemRef;
    Content: INyxNode;
    Host: TPanel;
    Document: TNyxDocument;
    Renderer: TNyxLCLRenderer;
    destructor Destroy; override;
  end;

  TCanvasMounts = array of TCanvasItemMount;
  TCanvasFrames = array of TNyxCanvasControlFrame;

  TCanvasView = class(TInterfacedObject, INyxCanvasWorkspaceView, INyxLCLCanvasWorkspaceView)
  private
    FWorkspace: INyxCanvasWorkspace;
    FDocument: TNyxDocument;
    FState: TNyxState;
    FOwnState: Boolean;
    FDesignMode: Boolean;
    FGate: INyxCanvasInteractionGate;
    FTheme: TNyxTheme;
    FSetup: TNyxCanvasLCLRendererSetup;
    FChrome: TNyxLCLRenderer;
    FScheduler: INyxScheduler;
    FSurface: TCanvasSurface;
    FCamera: TNyxCanvasCamera;
    FGrid: TNyxCanvasGrid;
    FScaling: TNyxCanvasControlScaling;
    FSnapshot: INyxCanvasSnapshot;
    FMounts: TCanvasMounts;
    FConnected: Boolean;
    FConstructing: Boolean;
    { Keep constructor staging free of transient Self interface owners. }
    FUpdating: Boolean;
    FDisconnectPending: Boolean;
    FDragging: Boolean;
    FStartCamera: TNyxCanvasCamera;
    FStartPoint: TPoint;
    function FindMount(const AItem: TNyxCanvasItemRef): Integer;
    function PrepareMount(const AItem: TNyxCanvasItemRef;
      const AContent: INyxNode; const AFrame: TNyxCanvasControlFrame): TCanvasItemMount;
    procedure Present(const ACamera: TNyxCanvasCamera; ARefresh: Boolean);
    procedure ChromeEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Down(ASender: TObject; AButton: TMouseButton;
      AShift: TShiftState; AX, AY: Integer);
    procedure Move(ASender: TObject; AShift: TShiftState; AX, AY: Integer);
    procedure Up(ASender: TObject; AButton: TMouseButton;
      AShift: TShiftState; AX, AY: Integer);
    procedure Wheel(ASender: TObject; AShift: TShiftState; AWheelDelta: Integer;
      AMousePos: TPoint; var AHandled: Boolean);
    procedure Keyboard(ASender: TObject; var AKey: Word; AShift: TShiftState);
    procedure CancelPan;
    procedure RequireConnected;
    procedure Resized;
    procedure PaintGrid(ACanvas: TCanvas);
  public
    constructor Create(const AWorkspace: INyxCanvasWorkspace;
      ADocument: TNyxDocument; AHost: TWinControl; AState: TNyxState;
      ATheme: TNyxTheme; ASetup: TNyxCanvasLCLRendererSetup; ADesignMode: Boolean);
    destructor Destroy; override;
    function GetSurface: TWinControl;
    function GetConnected: Boolean;
    function GetCamera: TNyxCanvasCamera;
    function GetMountedCount: Integer;
    procedure SetCamera(const ACamera: TNyxCanvasCamera);
    procedure PanBy(AX, AY: Double);
    procedure ZoomAt(AZoom: Double; const AAnchor: TNyxCanvasPoint);
    procedure Execute(ACommand: TNyxCanvasCommand);
    procedure Refresh;
    procedure Constrain(AEnabled, AReadOnly: Boolean);
    function EventsFor(const AItem: TNyxCanvasItemRef): INyxEvents;
    function RendererFor(const AItem: TNyxCanvasItemRef): TNyxLCLRenderer;
    function InputFor(const AItem: TNyxCanvasItemRef;
      const AControl: TNyxControlRef): TControl;
    procedure Disconnect;
  end;

constructor TCanvasSurface.CreateFor(AOwner: TComponent; AView: TCanvasView);
begin
  inherited Create(AOwner);
  FView := AView;
  TabStop := True;
  AccessibleName := 'Canvas workspace';
  AccessibleDescription := 'Arrow keys pan, plus and minus zoom, Home centers.';
  ControlStyle := ControlStyle + [csOpaque];
  DoubleBuffered := True;
end;

procedure TCanvasSurface.Paint;
begin

  if FView <> nil then
  begin
    FView.PaintGrid(Canvas);
  end;
end;

procedure TCanvasSurface.Resize;
begin
  inherited Resize;

  if FView <> nil then
  begin
    FView.Resized;
  end;
end;

procedure TCanvasSurface.WndProc(var AMessage: TLMessage);
begin

  if (AMessage.Msg = LM_CAPTURECHANGED) and (FView <> nil) and FView.FDragging then
  begin
    FView.CancelPan;
  end;
  inherited WndProc(AMessage);
end;

destructor TCanvasItemMount.Destroy;
begin
  Renderer.Free;
  Document.Free;
  Host.Free;
  Content := nil;
  inherited Destroy;
end;

constructor TCanvasView.Create(const AWorkspace: INyxCanvasWorkspace;
  ADocument: TNyxDocument; AHost: TWinControl; AState: TNyxState;
  ATheme: TNyxTheme; ASetup: TNyxCanvasLCLRendererSetup; ADesignMode: Boolean);
var
  LHost: TControl;
  LRoot: TNyxNode;
begin
  inherited Create;
  FConstructing := True;

  if (AWorkspace = nil) or (ADocument = nil) or (AHost = nil) then
  begin
    raise ENyxCanvasScene.Create('Native workspace mount requires workspace, document and host');
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
  FDesignMode := ADesignMode;
  FGate := NewNyxCanvasInteractionGate;
  FGrid := AWorkspace.Grid;
  FScaling := AWorkspace.Scaling;
  FChrome := TNyxLCLRenderer.Create(FTheme);
  FScheduler := FChrome.Events.Scheduler;
  FScheduler.RequireUI;
  FChrome.OnEvent := ChromeEvent;
  FChrome.Render(FDocument, LRoot, AHost, False, FState);
  FChrome.Root.AttachEnabledPolicy(FGate.EnabledPolicy);
  LHost := FChrome.ControlFor(NyxCanvasWorkspacePart(AWorkspace.ID, ncwpSurface).ID);

  if not (LHost is TWinControl) then
  begin
    raise ENyxCanvasScene.Create('Workspace surface must be a native Nyx layout host');
  end;
  FSurface := TCanvasSurface.CreateFor(nil, Self);
  FSurface.Parent := TWinControl(LHost);
  FSurface.Align := alClient;
  FSurface.OnMouseDown := Down;
  FSurface.OnMouseMove := Move;
  FSurface.OnMouseUp := Up;
  FSurface.OnMouseWheel := Wheel;
  FSurface.OnKeyDown := Keyboard;
  FCamera := AWorkspace.Camera;
  FConnected := True;
  Refresh;
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

function MountNyxLCLCanvasWorkspace(const AWorkspace: INyxCanvasWorkspace;
  ADocument: TNyxDocument; AHost: TWinControl; AState: TNyxState;
  ATheme: TNyxTheme; ASetup: TNyxCanvasLCLRendererSetup;
  ADesignMode: Boolean): INyxLCLCanvasWorkspaceView;
begin
  Result := TCanvasView.Create(AWorkspace, ADocument, AHost, AState, ATheme, ASetup, ADesignMode);
end;

procedure TCanvasView.RequireConnected;
begin
  FScheduler.RequireUI;

  if not FConnected then
  begin
    raise ENyxCanvasScene.Create('Native workspace view is disconnected');
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
    Result.Host := TPanel.Create(nil);
    Result.Host.BevelOuter := bvNone;
    Result.Host.Visible := False;
    Result.Host.Parent := FSurface;
    Result.Host.SetBounds(AFrame.Left, AFrame.Top, AFrame.Width, AFrame.Height);
    Result.Renderer := TNyxLCLRenderer.Create(FTheme);

    if Assigned(FSetup) then
    begin
      FSetup(Result.Renderer);
    end;
    Result.Document := NewNyxCanvasRenderContext(FDocument, AContent.Node, LRoot);
    Result.Renderer.Render(Result.Document, LRoot, Result.Host, FDesignMode, FState);
    Result.Renderer.Root.AttachEnabledPolicy(FGate.EnabledPolicy);
    Result.Renderer.Root.AttachReadOnlyPolicy(FGate.ReadOnlyPolicy);
    Result.Renderer.Sync;
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
  LFocus: TWinControl;
  LKeepFocus: Boolean;
begin
  RequireConnected;

  if FUpdating then
  begin
    raise ENyxCanvasScene.Create('Native workspace presentation is already updating');
  end;

  if (FSurface.ClientWidth <= 0) or (FSurface.ClientHeight <= 0) then
  begin
    Exit;
  end;

  if not FConstructing then
  begin
    LKeepAlive := Self;
  end;
  LCamera := ACamera.WithViewport(FSurface.ClientWidth, FSurface.ClientHeight);
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
        LMounts[LIndex].Host.SetBounds(LFrames[LIndex].Left, LFrames[LIndex].Top,
          LFrames[LIndex].Width, LFrames[LIndex].Height);

        if not FConnected then
        begin
          Exit;
        end;
        LMounts[LIndex].Host.BringToFront;
      end;
      LKeepFocus := False;
      LFocus := Screen.ActiveControl;
      while LFocus <> nil do
      begin

        if LFocus = LMounts[LIndex].Host then
        begin
          LKeepFocus := LSnapshot.Items[LIndex].Placement.IsVisible;
          Break;
        end;
        LFocus := LFocus.Parent;
      end;

      if not LFrames[LIndex].Visible and LKeepFocus then
      begin
        { Keep a focused editor alive beyond the clipped viewport, with a safe
          negative allocation. Hiding its parent would transfer native focus. }
        LMounts[LIndex].Host.Left := -MaximumNyxCanvasControlExtent;
      end;
      LMounts[LIndex].Host.Visible := LFrames[LIndex].Visible or LKeepFocus;

      if not FConnected then
      begin
        Exit;
      end;
    end;
    FSurface.Invalidate;
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

procedure TCanvasView.Resized;
begin

  { A hidden ordinary-render candidate can receive synchronous WM_SIZE while
    its parent publishes final bounds. Before its first explicit presentation,
    keep construction on Refresh's caller stack: LCL otherwise catches a
    component-factory exception as a host error instead of refusing the candidate.
    Successful presentation publishes FSnapshot even for an empty scene. }
  if FConnected and not FUpdating and (FSnapshot <> nil) then
  begin
    Present(FCamera, False);
  end;
end;

procedure TCanvasView.PaintGrid(ACanvas: TCanvas);
var
  LSpacing: Double;
  LX: Double;
  LY: Double;
  LStartX: Double;
  LStartY: Double;
  LCamera: TNyxCanvasCamera;

  function ThemeColor(const AValue: TNyxText): TColor;
  var
    LRGB: Integer;
  begin
    LRGB := NyxThemeRGB(AValue);
    Result := RGBToColor((LRGB shr 16) and $FF, (LRGB shr 8) and $FF, LRGB and $FF);
  end;

begin
  ACanvas.Brush.Color := ThemeColor(FTheme.Background);
  ACanvas.FillRect(Rect(0, 0, FSurface.Width, FSurface.Height));

  if not FConnected or (FGrid.Style = ncgsNone) then
  begin
    Exit;
  end;
  LCamera := FCamera;
  LSpacing := FGrid.VisibleSpacing(LCamera) * LCamera.Zoom;
  LStartX := LCamera.ViewportWidth / 2 +
    (FGrid.Origin.X - LCamera.WorldCenter.X) * LCamera.Zoom;
  LStartY := LCamera.ViewportHeight / 2 +
    (FGrid.Origin.Y - LCamera.WorldCenter.Y) * LCamera.Zoom;
  LStartX := LStartX - Floor(LStartX / LSpacing) * LSpacing;
  LStartY := LStartY - Floor(LStartY / LSpacing) * LSpacing;
  ACanvas.Pen.Color := ThemeColor(FTheme.Border);

  if FGrid.Style = ncgsLines then
  begin
    LX := LStartX;
    while LX < FSurface.Width do
    begin
      ACanvas.Line(Floor(LX + 0.5), 0, Floor(LX + 0.5), FSurface.Height);
      LX := LX + LSpacing;
    end;
    LY := LStartY;
    while LY < FSurface.Height do
    begin
      ACanvas.Line(0, Floor(LY + 0.5), FSurface.Width, Floor(LY + 0.5));
      LY := LY + LSpacing;
    end;
  end
  else
  begin
    ACanvas.Brush.Color := ACanvas.Pen.Color;
    LX := LStartX;
    while LX < FSurface.Width do
    begin
      LY := LStartY;
      while LY < FSurface.Height do
      begin
        ACanvas.FillRect(Rect(Floor(LX), Floor(LY), Floor(LX) + 2, Floor(LY) + 2));
        LY := LY + LSpacing;
      end;
      LX := LX + LSpacing;
    end;
  end;
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

procedure TCanvasView.Down(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  LKeepAlive: INyxCanvasWorkspaceView;
begin

  if not FConnected or not FGate.EnabledPolicy.Allowed or not (AButton in [mbLeft, mbMiddle]) then
  begin
    Exit;
  end;
  LKeepAlive := Self;
  FStartCamera := FCamera;
  FStartPoint := Point(AX, AY);
  FDragging := True;
  FSurface.SetFocus;

  if not FConnected then
  begin
    Exit;
  end;
  FSurface.MouseCapture := True;
end;

procedure TCanvasView.Move(ASender: TObject; AShift: TShiftState; AX, AY: Integer);
begin

  if FConnected and FDragging then
  begin

    if not FSurface.MouseCapture then
    begin
      CancelPan;
      Exit;
    end;
    SetCamera(FStartCamera.PanBy(AX - FStartPoint.X, AY - FStartPoint.Y));
  end;
end;

procedure TCanvasView.Up(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
begin
  FDragging := False;

  if FSurface <> nil then
  begin
    FSurface.MouseCapture := False;
  end;
end;

procedure TCanvasView.CancelPan;
var
  LCamera: TNyxCanvasCamera;
begin

  if not FDragging then
  begin
    Exit;
  end;
  LCamera := FStartCamera;
  FDragging := False;
  FSurface.MouseCapture := False;

  if FConnected then
  begin
    SetCamera(LCamera);
  end;
end;

procedure TCanvasView.Wheel(ASender: TObject; AShift: TShiftState;
  AWheelDelta: Integer; AMousePos: TPoint; var AHandled: Boolean);
var
  LPoint: TPoint;
begin

  if not FConnected or not FGate.EnabledPolicy.Allowed then
  begin
    Exit;
  end;
  LPoint := FSurface.ScreenToClient(AMousePos);

  if not (ssCtrl in AShift) and (FSurface.ControlAtPos(LPoint, True, True) <> nil) then
  begin
    { A nested editor keeps ordinary wheel/scroll defaults. Ctrl-wheel opts
      into workspace zoom, matching the browser descendant policy. }
    Exit;
  end;

  if ssCtrl in AShift then
  begin
    ZoomAt(FCamera.Zoom * Power(1.1, Max(-10, Min(10, AWheelDelta / 120))),
      NyxCanvasPoint(LPoint.X, LPoint.Y));
  end
  else
  begin
    PanBy(0, AWheelDelta / 3);
  end;
  AHandled := True;
end;

procedure TCanvasView.Keyboard(ASender: TObject; var AKey: Word; AShift: TShiftState);
begin

  if not FConnected or not FGate.EnabledPolicy.Allowed then
  begin
    Exit;
  end;
  case AKey of
    VK_LEFT: PanBy(40, 0);
    VK_RIGHT: PanBy(-40, 0);
    VK_UP: PanBy(0, 40);
    VK_DOWN: PanBy(0, -40);
    VK_HOME: Execute(nccHome);
    VK_ADD, VK_OEM_PLUS: Execute(nccZoomIn);
    VK_SUBTRACT, VK_OEM_MINUS: Execute(nccZoomOut);
    VK_ESCAPE: CancelPan;
  else
    Exit;
  end;
  AKey := 0;
end;

function TCanvasView.GetConnected: Boolean;
begin
  Result := FConnected;
end;

function TCanvasView.GetSurface: TWinControl;
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
    Ord(nccFit): SetCamera(FitNyxCanvasScene(LCamera, FSnapshot, FScaling));
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

function TCanvasView.RendererFor(const AItem: TNyxCanvasItemRef): TNyxLCLRenderer;
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
  const AControl: TNyxControlRef): TControl;
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

  if FGate <> nil then
  begin
    FGate.Update(False, True);
  end;
  FDragging := False;

  if FUpdating then
  begin
    { A callback may disconnect while native sizing is still on its stack.
      Revoke publication now, then retire physical faces at that frame's end. }
    FDisconnectPending := True;
    Exit;
  end;
  FDisconnectPending := False;

  if FSurface <> nil then
  begin
    FSurface.FView := nil;
    FSurface.OnMouseDown := nil;
    FSurface.OnMouseMove := nil;
    FSurface.OnMouseUp := nil;
    FSurface.OnMouseWheel := nil;
    FSurface.OnKeyDown := nil;
    FSurface.MouseCapture := False;
  end;
  for LIndex := 0 to High(FMounts) do
  begin
    FMounts[LIndex].Free;
  end;
  SetLength(FMounts, 0);
  FreeAndNil(FSurface);
  FreeAndNil(FChrome);
  FSnapshot := nil;
end;

procedure TCanvasView.Constrain(AEnabled, AReadOnly: Boolean);
var
  LIndex: Integer;
begin
  RequireConnected;

  if ((FGate.EnabledPolicy).Allowed = AEnabled) and
    ((FGate.ReadOnlyPolicy).ReadOnly = AReadOnly) then
  begin
    Exit;
  end;
  FGate.Update(AEnabled, AReadOnly);
  FChrome.Sync;

  if not AEnabled then
  begin
    CancelPan;
  end;
  for LIndex := 0 to High(FMounts) do
  begin
    FMounts[LIndex].Renderer.Sync;
  end;
end;

end.
