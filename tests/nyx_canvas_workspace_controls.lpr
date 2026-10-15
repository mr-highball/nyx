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
program nyx_canvas_workspace_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, Math, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.state,
  nyx.events, nyx.scheduler, nyx.behavior, nyx.codec, nyx.canvas.geometry,
  nyx.canvas.scene, nyx.canvas.workspace
  {$ifdef NYX_CANVAS_PROGRAM_TESTS}, nyx.programs, nyx.programs.types,
    nyx.programs.contract, nyx.programs.canvas, nyx.programs.canvas.publication,
    nyx.programs.execution, nyx.application.context, nyx.collections.registry,
    nyx.publication, nyx.resources, nyx.linq, nyx.linq.execution{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.canvas.browser
  {$else}, Interfaces, Classes, Types, Windows, Forms, Controls, StdCtrls, LCLType,
    nyx.widgets.lcl, nyx.canvas.lcl
    {$ifdef NYX_CANVAS_PROGRAM_TESTS}, Graphics{$endif}{$endif};

type
  TOrderedCallback = class(TNyxEventCallback)
  private
    FMarker: Integer;
  public
    constructor Create(AMarker: Integer);
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  { Uses the harness-owned global handle; the callback does not retain its view
    and therefore cannot form renderer/router/callback/view ownership cycles. }
  TDisconnectCallback = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  {$ifdef PAS2JS}
  TKeyOptions = class external name 'Object'(TJSObject)
    key: String;
    bubbles: Boolean;
    cancelable: Boolean;
  end;
  TKeyEvent = class external name 'KeyboardEvent'(TJSKeyboardEvent)
    constructor new(const AType: String; AOptions: TKeyOptions);
  end;
  TPointerOptions = class external name 'Object'(TJSObject)
    pointerId: NativeInt;
    pointerType: String;
    clientX: Double;
    clientY: Double;
    button: Integer;
    bubbles: Boolean;
    cancelable: Boolean;
  end;
  TPointerEvent = class external name 'PointerEvent'(TJSPointerEvent)
    constructor new(const AType: String; AOptions: TPointerOptions);
  end;
  TWheelOptions = class external name 'Object'(TJSObject)
    deltaX: Double;
    deltaY: Double;
    ctrlKey: Boolean;
    clientX: Double;
    clientY: Double;
    bubbles: Boolean;
    cancelable: Boolean;
  end;
  TWheelEvent = class external name 'WheelEvent'(TJSWheelEvent)
    constructor new(const AType: String; AOptions: TWheelOptions);
  end;
  {$else}
  TControlAccess = class(TWinControl);
  TWorkerProbe = class(TThread)
  private
    FView: INyxCanvasWorkspaceView;
  protected
    procedure Execute; override;
  public
    Refused: Boolean;
    constructor Create(const AView: INyxCanvasWorkspaceView);
  end;
  {$endif}

var
  GDocument: TNyxDocument;
  GScene: INyxCanvasScene;
  GWorkspace: INyxCanvasWorkspace;
  GOtherWorkspace: INyxCanvasWorkspace;
  GState: TNyxState;
  GLayer: TNyxCanvasLayerRef;
  GReply: TNyxCanvasItemRef;
  GBefore: TNyxText;
  GOrder: TNyxText;
  GChecks: Integer;
  GFirst: INyxEventSubscription;
  GSecond: INyxEventSubscription;
  {$ifdef PAS2JS}
  GHost: TJSHTMLElement;
  GOtherHost: TJSHTMLElement;
  GView: INyxBrowserCanvasWorkspaceView;
  GOtherView: INyxBrowserCanvasWorkspaceView;
  GInput: TJSHTMLTextAreaElement;
  {$else}
  GHost: TForm;
  GOtherHost: TForm;
  GView: INyxLCLCanvasWorkspaceView;
  GOtherView: INyxLCLCanvasWorkspaceView;
  GInput: TMemo;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Canvas workspace controls: ' + AReason);
  end;
  Inc(GChecks);
end;

constructor TOrderedCallback.Create(AMarker: Integer);
begin
  inherited Create;
  FMarker := AMarker;
end;

procedure TOrderedCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  GOrder := GOrder + TNyxText(IntToStr(FMarker));
end;

procedure TDisconnectCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  GOtherView.Disconnect;
end;

{$ifndef PAS2JS}
constructor TWorkerProbe.Create(const AView: INyxCanvasWorkspaceView);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FView := AView;
end;

procedure TWorkerProbe.Execute;
begin
  try
    FView.PanBy(1, 1);
  except
    on ENyxSchedule do
    begin
      Refused := True;
    end;
  end;
end;
{$endif}

procedure Fixture;
var
  LCard: INyxCard;
  LHeading: INyxHeading;
  LMemo: INyxMemo;
  LButton: INyxButton;
  LSidebar: INyxCard;
  LLabel: INyxLabel;
begin
  GDocument := TNyxDocument.Create;
  GDocument.Title := 'Canvas workshop';
  GDocument.AddPage(NewNyxPage('home'));
  GDocument.State.SetValue(NyxTextState('reply'), 'Start here');
  {$ifdef NYX_CANVAS_PROGRAM_TESTS}
  GDocument.State.SetValue(NyxTextState('layout-summary'), 'Ready to arrange');
  GDocument.State.SetValue(NyxIntegerState('layout-count'), 0);
  GDocument.State.SetValue(NyxBooleanState('layout-ready'), False);
  GDocument.State.SetValue(NyxNumberState('layout-zoom'), 0.0);
  GDocument.State.SetValue(NyxTextState('layout-payload'), 'No payload');
  {$endif}
  GState := GDocument.State.Clone;
  GBefore := TNyxCodec.Encode(GDocument);
  GScene := NewNyxCanvasScene;
  GLayer := NyxCanvasLayer('Controls');
  GReply := NyxCanvasItem('Reply editor');
  GScene.AddLayer(GLayer);
  LCard := NewNyxCard('reply-card');
  LCard.Configure.Padding(16).Gap(12).HeightSizing(nsFill);
  LHeading := NewNyxHeading('reply-heading');
  LHeading.Text := 'Build something useful';
  LCard.Add(LHeading);
  LMemo := NewNyxMemo('reply-memo');
  LMemo.Text := 'Write a reply';
  LMemo.Binds.Value(NyxTextState('reply'));
  LCard.Add(LMemo);
  LButton := NewNyxButton('send-button');
  LButton.Text := 'Post reply';
  LCard.Add(LButton);
  GScene.AddControl(GReply, NyxCanvasPlacement(GLayer,
    NyxCanvasBounds(-270, -140, 360, 300)), LCard);
  LSidebar := NewNyxCard('tools-card');
  LSidebar.Configure.Padding(16).Gap(12).HeightSizing(nsFill);
  LHeading := NewNyxHeading('tools-heading');
  LHeading.Text := 'Your workspace';
  LSidebar.Add(LHeading);
  LLabel := NewNyxLabel('tools-help');
  LLabel.Text := 'Pan the background. Zoom around a point. Your controls stay live.';
  LSidebar.Add(LLabel);
  GScene.AddControl(NyxCanvasItem('Workspace guide'), NyxCanvasPlacement(GLayer,
    NyxCanvasBounds(130, -140, 240, 300)), LSidebar);
  LLabel := NewNyxLabel('far-label');
  LLabel.Text := 'A distant control';
  GScene.AddControl(NyxCanvasItem('Distant item'), NyxCanvasPlacement(GLayer,
    NyxCanvasBounds(1000000000000.0, 0, 0, 0)), LLabel);
  GWorkspace := NewNyxCanvasWorkspace('workshop', GScene);
  GOtherWorkspace := NewNyxCanvasWorkspace('independent', GScene)
    .WithScaling(nccsLogicalSize);
end;

{$ifdef PAS2JS}
procedure Key(const AKey: TNyxText; ATarget: TJSHTMLElement);
var
  LOptions: TKeyOptions;
begin
  LOptions := TKeyOptions.new;
  LOptions.key := AKey;
  LOptions.bubbles := True;
  LOptions.cancelable := True;
  ATarget.dispatchEvent(TKeyEvent.new('keydown', LOptions));
end;

procedure Pointer(const AType: TNyxText; AID: Integer; AX, AY: Double);
var
  LOptions: TPointerOptions;
  LBounds: TJSDOMRect;
begin
  LBounds := GView.Surface.getBoundingClientRect;
  LOptions := TPointerOptions.new;
  LOptions.pointerId := AID;
  LOptions.pointerType := 'touch';
  LOptions.clientX := LBounds.left + AX;
  LOptions.clientY := LBounds.top + AY;
  LOptions.button := 0;
  LOptions.bubbles := True;
  LOptions.cancelable := True;
  GView.Surface.dispatchEvent(TPointerEvent.new(AType, LOptions));
end;
{$else}
function FindButton(AParent: TWinControl; const ACaption: TNyxText): TNyxLCLButton;
var
  LIndex: Integer;
begin
  Result := nil;
  for LIndex := 0 to AParent.ControlCount - 1 do
  begin

    if (AParent.Controls[LIndex] is TNyxLCLButton) and
      (TNyxText(TNyxLCLButton(AParent.Controls[LIndex]).Caption) = ACaption) then
    begin
      Exit(TNyxLCLButton(AParent.Controls[LIndex]));
    end;

    if AParent.Controls[LIndex] is TWinControl then
    begin
      Result := FindButton(TWinControl(AParent.Controls[LIndex]), ACaption);

      if Result <> nil then
      begin
        Exit;
      end;
    end;
  end;
end;
{$endif}

{$ifdef NYX_CANVAS_PROGRAM_TESTS}
{$i nyx_canvas_program_controls.inc}
{$i nyx_canvas_frame_controls.inc}
{$endif}

{$ifdef PAS2JS}
function Run: JSValue; async;
{$else}
procedure Run;
{$endif}
var
  LCamera: TNyxCanvasCamera;
  LOtherCamera: TNyxCanvasCamera;
  LOldRevision: Integer;
  LRefused: Boolean;
  LMountCount: Integer;
  LFrame: TNyxCanvasControlFrame;
  LOffscreen: TNyxCanvasControlFrame;
  LControl: INyxLabel;
  LReplyContent: INyxNode;
  LEvents: INyxEvents;
  LClose: INyxEventSubscription;
  {$ifdef PAS2JS}
  LFormerInput: TJSHTMLTextAreaElement;
  LAncestor: TJSHTMLElement;
  LButton: TJSHTMLButtonElement;
  LBox: TJSDOMRect;
  LWheelOptions: TWheelOptions;
  {$else}
  LFormerInput: TMemo;
  LButton: TNyxLCLButton;
  LKey: Word;
  LWorker: TWorkerProbe;
  LHandled: Boolean;
  {$endif}
begin
  {$ifdef PAS2JS}Result := Undefined;{$endif}
  Fixture;
  LOldRevision := GScene.Revision;
  LRefused := False;
  try
    NyxCanvasWorkspacePart('', ncwpSurface);
  except
    on ENyxCanvasScene do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (GScene.Revision = LOldRevision),
    'absent workspace identity refuses before authoring mutation');
  LRefused := False;
  try
    NyxCanvasWorkspacePart(StringOfChar('x', 128), ncwpSurface);
  except
    on ENyxCanvasScene do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (GScene.Revision = LOldRevision),
    'oversized chrome identity refuses before partial component construction');
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  GHost.style.setProperty('height', '650px');
  GHost.style.setProperty('width', '100%');
  TJSHTMLElement(document.body).style.setProperty('margin', '0');
  document.body.appendChild(GHost);
  GOtherHost := TJSHTMLElement(document.createElement('div'));
  GOtherHost.style.setProperty('width', '800px');
  GOtherHost.style.setProperty('height', '600px');
  GOtherHost.style.setProperty('position', 'absolute');
  GOtherHost.style.setProperty('left', '-10000px');
  document.body.appendChild(GOtherHost);
  GView := MountNyxBrowserCanvasWorkspace(GWorkspace, GDocument, GHost, GState);
  GOtherView := MountNyxBrowserCanvasWorkspace(GOtherWorkspace, GDocument, GOtherHost);
  GInput := TJSHTMLTextAreaElement(GView.InputFor(GReply, NyxControl('reply-memo')));
  {$else}
  Application.Initialize;
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(40, 40, 1000, 750);
  GHost.Show;
  GOtherHost := TForm.CreateNew(nil);
  GOtherHost.SetBounds(60, 60, 900, 700);
  GOtherHost.Show;
  GView := MountNyxLCLCanvasWorkspace(GWorkspace, GDocument, GHost, GState);
  GOtherView := MountNyxLCLCanvasWorkspace(GOtherWorkspace, GDocument, GOtherHost);
  Application.ProcessMessages;
  GInput := TMemo(GView.InputFor(GReply, NyxControl('reply-memo')));
  {$endif}
  Check(GView.Connected and GOtherView.Connected, 'both live workspace mounts connect');
  {$ifndef PAS2JS}
  LCamera := GView.Camera;
  LWorker := TWorkerProbe.Create(GView);
  try
    LWorker.Start;
    LWorker.WaitFor;
    Check(LWorker.Refused and (GView.Camera.WorldCenter.X = LCamera.WorldCenter.X),
      'real worker UI mutation refuses before touching camera or native controls');
  finally
    LWorker.Free;
  end;
  {$endif}
  Check(GView.MountedCount = 2, 'off-screen point is not physically mounted');
  LFrame := NyxCanvasControlFrame(GView.Camera, NyxCanvasBounds(-270, -140, 360, 300),
    nccsReflow);
  Check(LFrame.Visible, 'embedded compound is allocated in actual viewport');
  {$ifdef PAS2JS}
  Check(GInput.value = 'Start here', 'actual browser memo receives shared typed binding');
  GInput.focus;
  GInput.value := 'A live edited reply';
  GInput.dispatchEvent(TJSEvent.new('input'));
  {$else}
  Check(TNyxText(GInput.Text) = 'Start here', 'actual native memo receives shared typed binding');
  GHost.Show;
  GInput.SetFocus;
  GInput.Text := 'A live edited reply';
  Application.ProcessMessages;
  {$endif}
  Check(GState.GetValue(NyxTextState('reply')) = 'A live edited reply',
    'actual input publishes through ordinary two-way Nyx binding');
  {$ifdef PAS2JS}
  Check(TJSHTMLTextAreaElement(GOtherView.InputFor(GReply, NyxControl('reply-memo')))
    .value = 'Start here', 'second browser view owns an independent default store');
  {$else}
  Check(TNyxText(TMemo(GOtherView.InputFor(GReply, NyxControl('reply-memo')))
    .Text) = 'Start here', 'second native view owns an independent default store');
  Check(GView.Surface.AccessibleName = 'Canvas workspace',
    'native keyboard surface has an accessible name');
  {$endif}
  LFormerInput := GInput;
  LOtherCamera := GOtherView.Camera;
  LCamera := GView.Camera;
  GView.PanBy(25, 10);
  Check(GView.InputFor(GReply, NyxControl('reply-memo')) = LFormerInput,
    'camera pan preserves the same actual memo face');
  Check((GOtherView.Camera.WorldCenter.X = LOtherCamera.WorldCenter.X) and
    (GOtherView.Camera.Zoom = LOtherCamera.Zoom), 'independent view camera is unchanged');
  GView.ZoomAt(1.25, NyxCanvasPoint(LCamera.ViewportWidth / 2, LCamera.ViewportHeight / 2));
  Check(GView.InputFor(GReply, NyxControl('reply-memo')) = LFormerInput,
    'live reflow zoom retains the same memo and router');
  {$ifdef PAS2JS}
  Check((GInput.value = 'A live edited reply') and (document.activeElement = GInput),
    'browser zoom retains edit and focus');
  LAncestor := GInput;
  while LAncestor <> nil do
  begin
    Check(window.getComputedStyle(LAncestor).getPropertyValue('touch-action') <> 'none',
      'embedded input ancestors preserve default touch editing and scrolling');
    LAncestor := TJSHTMLElement(LAncestor.parentElement);
  end;
  Key('ArrowRight', GInput);
  Check(GView.Camera.WorldCenter.X = LCamera.WorldCenter.X - 25 / LCamera.Zoom,
    'embedded editor keyboard does not pan the workspace');
  LButton := TJSHTMLButtonElement(GView.RendererFor(GReply).ElementFor('send-button'));
  {$else}
  Check((TNyxText(GInput.Text) = 'A live edited reply') and GInput.Focused,
    'native zoom retains edit and focus');
  LButton := TNyxLCLButton(GView.RendererFor(GReply).ControlFor('send-button'));
  {$endif}
  GFirst := GView.EventsFor(GReply).On(NyxControlEvents('send-button'), ntClick)
    .Subscribe(TOrderedCallback.Create(1));
  GSecond := GView.EventsFor(GReply).On(NyxControlEvents('send-button'), ntClick)
    .Subscribe(TOrderedCallback.Create(2));
  LButton.Click;
  Check(GOrder = '12', 'embedded button dispatches ordered callbacks through ordinary Nyx events');
  GView.PanBy(-25, -10);
  LButton.Click;
  Check(GOrder = '1212', 'camera changes retain both subscriptions');
  Check(GFirst.Active and GSecond.Active, 'registrations remain live after navigation');
  {$ifdef PAS2JS}
  GInput.focus;
  {$else}
  GInput.SetFocus;
  {$endif}
  GView.PanBy(10000, 0);
  Check(GView.InputFor(GReply, NyxControl('reply-memo')) = LFormerInput,
    'off-screen culling retains the same mounted editor');
  {$ifdef PAS2JS}
  Check(document.activeElement = GInput, 'browser focused editor survives viewport culling');
  {$else}
  Check(GInput.Focused, 'native focused editor survives viewport culling');
  {$endif}
  GView.SetCamera(LCamera);
  {$ifdef PAS2JS}
  TJSHTMLButtonElement(GHost.querySelector('[data-runtime-id="' +
    NyxCanvasWorkspacePart(GWorkspace.ID, ncwpZoomIn).ID + '"]')).click;
  {$else}
  FindButton(GHost, 'Zoom in').Click;
  {$endif}
  Check(Abs(GView.Camera.Zoom - LCamera.Zoom * 1.25) < 1e-9,
    'actual Nyx chrome button invokes typed zoom command');
  GView.SetCamera(LCamera);
  {$ifdef PAS2JS}
  Key('ArrowRight', GView.Surface);
  Check(GView.Camera.WorldCenter.X > LCamera.WorldCenter.X,
    'actual browser background keyboard pans');
  GView.SetCamera(LCamera);
  Pointer('pointerdown', 1, 50, 50);
  Pointer('pointermove', 1, 80, 70);
  Check(Abs(GView.Camera.WorldCenter.X - (LCamera.WorldCenter.X - 30 / LCamera.Zoom)) < 1e-8,
    'synthetic background pointer pans from initial camera');
  Pointer('pointercancel', 1, 80, 70);
  Check(GView.Camera.WorldCenter.X = LCamera.WorldCenter.X, 'pointer cancellation restores gesture camera');
  Pointer('pointerdown', 1, 50, 50);
  Pointer('pointerdown', 2, 150, 50);
  Pointer('pointermove', 2, 250, 50);
  Check(Abs(GView.Camera.Zoom - LCamera.Zoom * 2) < 1e-8, 'two background contacts pinch anchored camera');
  Pointer('pointerup', 2, 250, 50);
  Pointer('pointerup', 1, 50, 50);
  {$else}
  LKey := VK_RIGHT;
  TControlAccess(GView.Surface).OnKeyDown(GView.Surface, LKey, []);
  Check((LKey = 0) and (GView.Camera.WorldCenter.X > LCamera.WorldCenter.X),
    'actual native background keyboard pans and consumes its default');
  GView.SetCamera(LCamera);
  TControlAccess(GView.Surface).OnMouseDown(GView.Surface, mbLeft, [], 50, 50);
  TControlAccess(GView.Surface).OnMouseMove(GView.Surface, [ssLeft], 80, 70);
  Check(Abs(GView.Camera.WorldCenter.X - (LCamera.WorldCenter.X - 30 / LCamera.Zoom)) < 1e-8,
    'actual native captured background pointer pans');
  LKey := VK_ESCAPE;
  TControlAccess(GView.Surface).OnKeyDown(GView.Surface, LKey, []);
  Check(GView.Camera.WorldCenter.X = LCamera.WorldCenter.X, 'native cancel restores gesture camera');
  {$endif}
  GView.SetCamera(LCamera);
  {$ifdef PAS2JS}
  LWheelOptions := TWheelOptions.new;
  LWheelOptions.deltaY := 100;
  LWheelOptions.bubbles := True;
  LWheelOptions.cancelable := True;
  GInput.dispatchEvent(TWheelEvent.new('wheel', LWheelOptions));
  Check(GView.Camera.WorldCenter.Y = LCamera.WorldCenter.Y,
    'embedded browser editor wheel does not pan the workspace');
  GView.Surface.dispatchEvent(TWheelEvent.new('wheel', LWheelOptions));
  Check(GView.Camera.WorldCenter.Y > LCamera.WorldCenter.Y,
    'browser background wheel pans in logical pixels');
  {$else}
  LHandled := False;
  TControlAccess(GView.Surface).OnMouseWheel(GView.Surface, [], 120,
    GInput.ClientToScreen(Types.Point(5, 5)), LHandled);
  Check(not LHandled and (GView.Camera.WorldCenter.Y = LCamera.WorldCenter.Y),
    'embedded native editor wheel keeps its ordinary default');
  TControlAccess(GView.Surface).OnMouseWheel(GView.Surface, [], 120,
    GView.Surface.ClientToScreen(Types.Point(1, 1)), LHandled);
  Check(LHandled and (GView.Camera.WorldCenter.Y < LCamera.WorldCenter.Y),
    'native background wheel pans and consumes only its own request');
  {$endif}
  GView.SetCamera(LCamera);
  LOldRevision := GScene.Revision;
  GScene.Place(GReply, NyxCanvasPlacement(GLayer, NyxCanvasBounds(-250, -130, 360, 300)));
  GView.Refresh;
  Check((GScene.Revision = LOldRevision + 1) and
    (GView.InputFor(GReply, NyxControl('reply-memo')) = LFormerInput),
    'explicit scene placement refresh retains live input');
  LMountCount := GView.MountedCount;
  LRefused := False;
  try
    GView.ZoomAt(1000000, NyxCanvasPoint(100, 100));
  except
    on ENyxCanvasScene do
    begin
      LRefused := True;
    end;
  end;
  { Normal camera limits clamp this request, so the shared explicit frame
    budget is exercised separately with admitted wider limits. }
  Check(not LRefused, 'ordinary extreme requested zoom clamps to configured limits');
  GView.SetCamera(LCamera);
  LRefused := False;
  try
    GView.SetCamera(LCamera.WithLimits(NyxCanvasZoomLimits(0.001, 1000)).ZoomAt(1000,
      NyxCanvasPoint(LCamera.ViewportWidth / 2, LCamera.ViewportHeight / 2)));
  except
    on ENyxCanvasScene do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (GView.Camera.Zoom = LCamera.Zoom) and
    (GView.MountedCount = LMountCount), 'oversized visible live allocation refuses before camera/mount mutation');
  LOffscreen := NyxCanvasControlFrame(LCamera,
    NyxCanvasBounds(1000000000000.0, 0, 0, 0), nccsReflow);
  Check(not LOffscreen.Visible, 'far geometry culls before integer projection');
  GOtherView.SetCamera(GOtherView.Camera.ZoomAt(2,
    NyxCanvasPoint(GOtherView.Camera.ViewportWidth / 2, GOtherView.Camera.ViewportHeight / 2)));
  LFrame := NyxCanvasControlFrame(GOtherView.Camera,
    NyxCanvasBounds(-270, -140, 360, 300), nccsLogicalSize);
  Check(LFrame.Width = 360, 'logical-size policy keeps live allocation independent of zoom');
  Check(TNyxCodec.Encode(GDocument) = GBefore, 'runtime cameras/input preserve exact document defaults and source');
  LEvents := GView.EventsFor(GReply);
  LReplyContent := GScene.Control(GReply);
  GScene.Remove(GReply);
  GView.Refresh;
  Check(not GFirst.Active and not GSecond.Active, 'removed item retires its subscriptions');
  Check(GView.MountedCount = 1, 'removed compound retires only its own mount');
  Check(GOtherView.Connected, 'another view remains independent until explicit refresh');
  LClose := GOtherView.EventsFor(GReply).On(NyxControlEvents('send-button'), ntClick)
    .Subscribe(TDisconnectCallback.Create);
  {$ifdef PAS2JS}
  TJSHTMLButtonElement(GOtherView.RendererFor(GReply).ElementFor('send-button')).click;
  {$else}
  TNyxLCLButton(GOtherView.RendererFor(GReply).ControlFor('send-button')).Click;
  {$endif}
  Check(not GOtherView.Connected and not LClose.Active,
    'actual embedded callback can disconnect its own view and retire registration');
  LControl := NewNyxLabel('replacement');
  LControl.Text := 'Ready for your next idea';
  GScene.AddControl(GReply, NyxCanvasPlacement(GLayer,
    NyxCanvasBounds(-220, -100, 400, 180)), LControl);
  GView.Refresh;
  GView.Execute(nccHome);
  Check(GView.Connected and (GView.MountedCount = 2), 'new independent control admits after removal');
  GScene.Remove(GReply);
  GScene.AddControl(GReply, NyxCanvasPlacement(GLayer,
    NyxCanvasBounds(-270, -140, 360, 300)), LReplyContent);
  GView.Refresh;
  Check(GView.InputFor(GReply, NyxControl('reply-memo')) <> nil,
    'retained authored compound can be admitted again after runtime replacement');

  if GView.Camera.ViewportWidth < 500 then
  begin
    GView.SetCamera(GView.Camera.AtCenter(NyxCanvasPoint(-90, 10)));
  end;
  {$ifdef NYX_CANVAS_PROGRAM_TESTS}
  ProgramLayouts;
  {$ifdef PAS2JS}await(ProgramFrames);{$else}ProgramFrames;{$endif}
  {$endif}
  {$ifdef PAS2JS}
  GInput := TJSHTMLTextAreaElement(GView.InputFor(GReply, NyxControl('reply-memo')));
  Check(GInput.value = 'A live edited reply', 'restored live compound reads the retained application store');
  LBox := GView.Surface.getBoundingClientRect;
  Check((LBox.width > 200) and (LBox.height >= 200), 'actual browser surface keeps usable allocation');
  document.body.setAttribute('data-canvas-workspace', 'passed');
  document.body.setAttribute('data-canvas-checks', IntToStr(GChecks));
  {$endif}
  WriteLn('PASS ', GChecks, ' actual canvas workspace checks');
end;

{$ifdef PAS2JS}
function Start: JSValue; async;
{$else}
procedure Start;
{$endif}
begin
  {$ifdef PAS2JS}Result := Undefined;{$endif}
  try
    {$ifdef PAS2JS}await(Run);{$else}Run;{$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-workspace', 'failed');
      document.body.setAttribute('data-canvas-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      {$endif}
      ExitCode := 1;
    end;
  end;
  {$ifndef PAS2JS}

  if GView <> nil then
  begin
    GView.Disconnect;
  end;
  GView := nil;
  GOtherView := nil;
  GWorkspace := nil;
  GOtherWorkspace := nil;
  GScene := nil;
  GFirst := nil;
  GSecond := nil;
  GHost.Free;
  GOtherHost.Free;
  GState.Free;
  GDocument.Free;
  {$endif}
end;

begin
  Start;
end.
