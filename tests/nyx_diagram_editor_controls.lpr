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
program nyx_diagram_editor_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, Math, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model, nyx.controls, nyx.events,
  nyx.behavior, nyx.scheduler, nyx.state, nyx.diagram, nyx.diagram.drawing,
  nyx.diagram.editor, nyx.diagram.manipulation, nyx.drawing, nyx.drawing.view, nyx.canvas.geometry,
  nyx.canvas.types, nyx.canvas.workspace, nyx.canvas.observers, nyx.test.diagram,
  nyx.diagram.declarations, nyx.codec
  {$ifdef PAS2JS}, JS, Web, nyx.diagram.editor.browser, nyx.canvas.browser
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, Graphics, Types,
    nyx.diagram.editor.lcl, nyx.canvas.lcl, nyx.drawing.lcl, nyx.gestures.lcl{$endif};

type
  { Maintained host-input phases. Native phases invoke installed mouse hooks;
    browser phases construct standard events, without claiming hardware capture. }
  TDragSample = (ddsDown, ddsMove, ddsUp, ddsLostCapture);

  { Caller snapshot behaviors deliberately reenter the mounted editor boundary.
    They are test choices, never persisted execution-policy strings. }
  TCreatorMode = (dcmOrdinary, dcmProperties, dcmDisconnect);

  { Alternative complete interface forwards immutable graph behavior to Base.
    Only ToData injects a synchronous caller side effect. Target is retained for
    this short qualification call; no router, tree or accepted model owns this
    creator, and the production view must capture once into its private plan. }
  TCreatorDiagram = class(TInterfacedObject, INyxDiagram)
  public
    Base: INyxDiagram;
    Target: INyxDiagramEditorView;
    Mode: TCreatorMode;
    Calls: Integer;
    function GetVertexCount: Integer;
    function GetEdgeCount: Integer;
    function Vertex(AIndex: Integer): TNyxDiagramVertex;
    function Edge(AIndex: Integer): TNyxDiagramEdge;
    function FindVertex(const AReference: TNyxDiagramVertexRef): TNyxDiagramVertex;
    function FindEdge(const AReference: TNyxDiagramEdgeRef): TNyxDiagramEdge;
    function WithVertex(const AVertex: TNyxDiagramVertex): INyxDiagram;
    function WithEdge(const AEdge: TNyxDiagramEdge): INyxDiagram;
    function WithoutVertex(const AReference: TNyxDiagramVertexRef;
      APolicy: TNyxDiagramRemoval): INyxDiagram;
    function WithoutEdge(const AReference: TNyxDiagramEdgeRef): INyxDiagram;
    function Route(const AReference: TNyxDiagramEdgeRef): TNyxDiagramRoute;
    function PickVertex(const APoint: TNyxCanvasPoint): TNyxDiagramVertexRef;
    function PickEdge(const APoint: TNyxCanvasPoint; ATolerance: Double): TNyxDiagramEdgeRef;
    function ToData: TNyxDataValue;
  end;

  { Fixture-owned callback borrows the global mounted handle during invocation.
    It intentionally disconnects from a later ordinary registration, testing the
    native/browser renderer's guarded event retirement without a retention cycle. }
  TDisconnect = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  {$ifdef PAS2JS}
  { Test-created KeyboardEvent uses the browser API only for this dispatch.
    The adapter copies its key/default response; no host event is retained.
    An HTTP browser supporting standard keyboard construction is required. }
  TDiagramKeyEvent = class external name 'KeyboardEvent'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject);
  end;
  { Test-created PointerEvent supplies outer-face logical coordinates to the
    ordinary browser adapter; it proves host synthesis, never hardware capture.
    Modern PointerEvent availability is required by this maintained consumer. }
  TDiagramPointerEvent = class external name 'PointerEvent'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject);
  end;
  {$else}
  { Protected-hook access invokes the actual mounted native mouse/key handlers.
    No widget is allocated by this alias or retained beyond the fixture host.
    These are native host dispatches, not claimed physical-device input. }
  TInputAccess = class(TControl)
  public
    procedure PrimaryDown(AX, AY: Integer);
    procedure PrimaryUp(AX, AY: Integer);
    procedure PrimaryMove(AX, AY: Integer);
  end;
  { Borrow the native key surface solely to invoke its installed keyboard hook. }
  TKeyAccess = class(TWinControl);
  { Unexpected native framework errors remain test-owned until idle retirement;
    they fail the consumer instead of opening unattended modal dialogs. }
  TFailure = class
  public
    Message: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;
  { Joined worker retains the view while the main UI owner stays alive. Every
    attempted live method must refuse before controls, diagram or draft change.
    The main thread releases this retained authority after joining. }
  TWorker = class(TThread)
  public
    View: INyxDiagramEditorView;
    Refusals: Integer;
    procedure Execute; override;
  end;
  {$endif}

var
  GChecks: Integer;
  GDocument: TNyxDocument;
  GEditor: INyxDiagramEditor;
  GInitial: INyxDiagram;
  GInitialBytes: TNyxText;
  GView: INyxDiagramEditorView;
  GOther: INyxDiagramEditorView;
  GDisconnect: INyxEventSubscription;
  {$ifdef PAS2JS}
  GHost: TJSHTMLElement;
  GOtherHost: TJSHTMLElement;
  GTarget: INyxBrowserDiagramEditorView;
  GCanvas: INyxBrowserCanvasWorkspaceView;
  {$else}
  GHost: TForm;
  GOtherHost: TForm;
  GTarget: INyxLCLDiagramEditorView;
  GCanvas: INyxLCLCanvasWorkspaceView;
  GFailure: TFailure;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Diagram editor: ' + AReason);
  end;
  Inc(GChecks);
end;

function ID(APart: TNyxDiagramEditorPart): TNyxText;
begin
  Result := NyxDiagramEditorPart('workflow', APart).ID;
end;

procedure Pump;
begin
  {$ifndef PAS2JS}
  Application.ProcessMessages;
  CheckSynchronize;
  {$endif}
end;

procedure TDisconnect.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  GView.Disconnect;
end;

{$ifndef PAS2JS}
procedure TInputAccess.PrimaryDown(AX, AY: Integer);
begin
  MouseDown(mbLeft, [], AX, AY);
end;

procedure TInputAccess.PrimaryUp(AX, AY: Integer);
begin
  MouseUp(mbLeft, [], AX, AY);
end;

procedure TInputAccess.PrimaryMove(AX, AY: Integer);
begin
  MouseMove([ssLeft], AX, AY);
end;

procedure TFailure.Failed(ASender: TObject; AError: Exception);
begin
  Message := AError.Message;
end;

procedure TWorker.Execute;
var
  LHistory: INyxDiagramHistoryView;
begin
  try
    View.GetDiagram;
  except
    on E: ENyxSchedule do
    begin
      Inc(Refusals);
    end;
  end;
  try
    View.ShowTools(True);
  except
    on E: ENyxSchedule do
    begin
      Inc(Refusals);
    end;
  end;
  try
    View.NudgeSelection(10, 0, 0);
  except
    on E: ENyxSchedule do
    begin
      Inc(Refusals);
    end;
  end;
  LHistory := View as INyxDiagramHistoryView;
  try
    LHistory.GetUndoCount;
  except
    on E: ENyxSchedule do
    begin
      Inc(Refusals);
    end;
  end;
  try
    LHistory.Undo(0);
  except
    on E: ENyxSchedule do
    begin
      Inc(Refusals);
    end;
  end;
  try
    LHistory.ClearHistory;
  except
    on E: ENyxSchedule do
    begin
      Inc(Refusals);
    end;
  end;
end;
{$endif}

procedure Click(APart: TNyxDiagramEditorPart);
begin
  {$ifdef PAS2JS}
  GTarget.Renderer.ElementFor(ID(APart)).click;
  {$else}
  TButton(GTarget.Renderer.ControlFor(ID(APart))).Click;
  {$endif}
  Pump;
end;

procedure WriteInput(APart: TNyxDiagramEditorPart; const AText: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GTarget.Renderer.InputFor(ID(APart))).value := AText;
  GTarget.Renderer.InputFor(ID(APart)).dispatchEvent(TJSEvent.new('input'));
  {$else}
  TEdit(GTarget.Renderer.InputFor(ID(APart))).Text := AText;
  {$endif}
  Pump;
end;

function TCreatorDiagram.GetVertexCount: Integer;
begin
  Result := Base.VertexCount;
end;

function TCreatorDiagram.GetEdgeCount: Integer;
begin
  Result := Base.EdgeCount;
end;

function TCreatorDiagram.Vertex(AIndex: Integer): TNyxDiagramVertex;
begin
  Result := Base.Vertex(AIndex);
end;

function TCreatorDiagram.Edge(AIndex: Integer): TNyxDiagramEdge;
begin
  Result := Base.Edge(AIndex);
end;

function TCreatorDiagram.FindVertex(const AReference: TNyxDiagramVertexRef): TNyxDiagramVertex;
begin
  Result := Base.FindVertex(AReference);
end;

function TCreatorDiagram.FindEdge(const AReference: TNyxDiagramEdgeRef): TNyxDiagramEdge;
begin
  Result := Base.FindEdge(AReference);
end;

function TCreatorDiagram.WithVertex(const AVertex: TNyxDiagramVertex): INyxDiagram;
begin
  Result := Base.WithVertex(AVertex);
end;

function TCreatorDiagram.WithEdge(const AEdge: TNyxDiagramEdge): INyxDiagram;
begin
  Result := Base.WithEdge(AEdge);
end;

function TCreatorDiagram.WithoutVertex(const AReference: TNyxDiagramVertexRef; APolicy: TNyxDiagramRemoval): INyxDiagram;
begin
  Result := Base.WithoutVertex(AReference, APolicy);
end;

function TCreatorDiagram.WithoutEdge(const AReference: TNyxDiagramEdgeRef): INyxDiagram;
begin
  Result := Base.WithoutEdge(AReference);
end;

function TCreatorDiagram.Route(const AReference: TNyxDiagramEdgeRef): TNyxDiagramRoute;
begin
  Result := Base.Route(AReference);
end;

function TCreatorDiagram.PickVertex(const APoint: TNyxCanvasPoint): TNyxDiagramVertexRef;
begin
  Result := Base.PickVertex(APoint);
end;

function TCreatorDiagram.PickEdge(const APoint: TNyxCanvasPoint; ATolerance: Double): TNyxDiagramEdgeRef;
begin
  Result := Base.PickEdge(APoint, ATolerance);
end;

function TCreatorDiagram.ToData: TNyxDataValue;
begin
  Inc(Calls);
  Result := Base.ToData;
  case Mode of
    dcmOrdinary:
      begin
        { An ordinary alternative implementation has no callout. }
      end;
    dcmProperties: WriteInput(ndepX, '-created');
    dcmDisconnect: Target.Disconnect;
  end;
end;

function ReadInput(APart: TNyxDiagramEditorPart): TNyxText;
var
  LControl: INyxControl;
begin
  LControl := RetainNyxControl(GTarget.Renderer.Root.Find(ID(APart)));
  Result := (LControl as INyxTextInput).Value;
end;

procedure SelectPointer(const AWorldPoint: TNyxCanvasPoint);
var
  LMapping: TNyxDrawingMapping;
  LLocal: TNyxCanvasPoint;
  {$ifdef PAS2JS}
  LFace: TJSHTMLElement;
  LOptions: TJSObject;
  LRect: TJSDOMRect;
  {$else}
  LFace: TControl;
  {$endif}
begin
  {$ifdef PAS2JS}
  LFace := GCanvas.RendererFor(NyxCanvasItem('Diagram picture')).ElementFor('workflow-diagram-picture');
  LMapping := NyxDrawingMapping(DrawNyxDiagramEditor(GView.DisplayDiagram, GView.Selection,
    GView.SelectedEdge, 8 / GView.Canvas.Camera.Zoom), LFace.clientWidth, LFace.clientHeight);
  LLocal := LMapping.Point(AWorldPoint);
  LRect := LFace.getBoundingClientRect;
  LOptions := TJSObject.new;
  LOptions['clientX'] := LRect.left + LLocal.X;
  LOptions['clientY'] := LRect.top + LLocal.Y;
  LOptions['button'] := 0;
  LOptions['buttons'] := 1;
  LOptions['pointerId'] := 1;
  LOptions['pointerType'] := 'mouse';
  LOptions['isPrimary'] := True;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LFace.dispatchEvent(TDiagramPointerEvent.new('pointerdown', LOptions));
  LOptions['buttons'] := 0;
  LFace.dispatchEvent(TDiagramPointerEvent.new('pointerup', LOptions));
  {$else}
  LFace := GCanvas.RendererFor(NyxCanvasItem('Diagram picture')).ControlFor('workflow-diagram-picture');
  LMapping := NyxDrawingMapping(TNyxLCLDrawing(LFace).Plan, LFace.Width, LFace.Height);
  LLocal := LMapping.Point(AWorldPoint);
  TInputAccess(LFace).PrimaryDown(Round(LLocal.X), Round(LLocal.Y));
  TInputAccess(LFace).PrimaryUp(Round(LLocal.X), Round(LLocal.Y));
  {$endif}
  Pump;
end;

procedure ArrowRight;
var
  {$ifdef PAS2JS}
  LOptions: TJSObject;
  LEvent: TJSEvent;
  {$else}
  LKey: Word;
  LFace: TWinControl;
  {$endif}
begin
  {$ifdef PAS2JS}
  LOptions := TJSObject.new;
  LOptions['key'] := 'ArrowRight';
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LEvent := TDiagramKeyEvent.new('keydown', LOptions);
  GTarget.Renderer.ElementFor(ID(ndepSelection)).dispatchEvent(LEvent);
  Check(LEvent.defaultPrevented, 'keyboard nudge consumes the browser default');
  {$else}
  LFace := TWinControl(GTarget.Renderer.ControlFor(ID(ndepSelection)));
  Check(Assigned(TKeyAccess(LFace).OnKeyDown), 'ordinary selection button has a native key hook');
  LKey := $27;
  TKeyAccess(LFace).OnKeyDown(LFace, LKey, []);
  Check(LKey = 0, 'keyboard nudge consumes the native default');
  {$endif}
  Pump;
end;

procedure PictureMatches;
var
  LProjection: INyxDrawingPlan;
  {$ifndef PAS2JS}
  LFace: TNyxLCLDrawing;
  LMapping: TNyxDrawingMapping;
  {$endif}
begin
  LProjection := DrawNyxDiagramEditor(GView.DisplayDiagram, GView.Selection,
    GView.SelectedEdge, 8 / GView.Canvas.Camera.Zoom);
  {$ifdef PAS2JS}
  Check(GCanvas.RendererFor(NyxCanvasItem('Diagram picture'))
    .ElementFor('workflow-diagram-picture').querySelector('svg') <> nil,
    'ordinary browser canvas owns the diagram SVG');
  {$else}
  LFace := TNyxLCLDrawing(GCanvas.RendererFor(NyxCanvasItem('Diagram picture'))
    .ControlFor('workflow-diagram-picture'));
  Check(LFace.Plan.ToData.ToJSON = LProjection.ToData.ToJSON,
    'complete actual native face equals selected graph projection');

  if Abs(GView.Canvas.Camera.Zoom - 1) < 0.000001 then
  begin
    LMapping := NyxDrawingMapping(LFace.Plan, LFace.Width, LFace.Height);
    Check(Abs(LMapping.Metric(14) - 14) < 0.1, 'actual-size captions retain readable logical font size');
  end;
  {$endif}
end;

procedure Setup;
var
  LPart: TNyxDiagramEditorPart;
  LFailed: Boolean;
  LOptions: TNyxDiagramEditorOptions;
  LFluent: INyxDiagramEditor;
  LSaved: TNyxDocument;
begin
  GInitial := DiagramFixture;
  GInitialBytes := GInitial.ToData.ToJSON;
  GDocument := TNyxDocument.Create;
  GDocument.Title := 'Diagram workshop';
  GDocument.AddPage(NewNyxPage('home'));
  GDocument.Diagrams.Define(NyxDiagramRef('Workflow'), GInitial);
  LSaved := TNyxCodec.Decode(TNyxCodec.Encode(GDocument));
  try
    GEditor := NewNyxDiagramEditor('workflow', LSaved, NyxDiagramRef('Workflow'));
  finally
    LSaved.Free;
  end;
  Check(GEditor.InitialDiagram.ToData.ToJSON = GInitialBytes,
    'actual mounted editor reopens saved intent after its borrowed document retires');
  LFluent := NewNyxDiagramEditor('fluent-example', GInitial)
    .Captioned('Workflow designer').WithOptions(NyxDiagramEditorOptions.Nudge(5));
  Check((LFluent.Text = 'Workflow designer') and (LFluent.Options.NudgeDistance = 5),
    'caption/configuration chaining retains the specialized editor interface');
  for LPart := Low(TNyxDiagramEditorPart) to High(TNyxDiagramEditorPart) do
  begin
    Check(GEditor.Node.Find(ID(LPart)) <> nil, 'public typed composition part');
  end;
  LOptions := GEditor.Options;
  LFailed := False;
  try
    GEditor.WithOptions(Default(TNyxDiagramEditorOptions));
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GEditor.Options.NudgeDistance = LOptions.NudgeDistance),
    'invalid options preserve the component');
  LFailed := False;
  try
    NyxDiagramEditorOptions.Nudge(0);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'zero nudge refuses explicitly');
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  GHost.style.setProperty('height', '100dvh');
  document.body.appendChild(GHost);
  GOtherHost := TJSHTMLElement(document.createElement('div'));
  GOtherHost.style.setProperty('width', '400px');
  GOtherHost.style.setProperty('height', '500px');
  GOtherHost.style.setProperty('position', 'absolute');
  GOtherHost.style.setProperty('left', '-2000px');
  document.body.appendChild(GOtherHost);
  GTarget := MountNyxBrowserDiagramEditor(GEditor, GDocument, GHost);
  GEditor.WithOptions(LOptions.HistoryLimit(2)
    .HistoryByteBudget(3 * NyxUTF8ByteCount(GInitialBytes) + 512));
  GOther := MountNyxBrowserDiagramEditor(GEditor, GDocument, GOtherHost);
  GEditor.WithOptions(LOptions);
  GCanvas := GTarget.Canvas as INyxBrowserCanvasWorkspaceView;
  {$else}
  GHost := TForm.CreateNew(nil);
  GHost.Caption := 'Nyx diagram editor';
  GHost.SetBounds(20, 20, 1040, 720);

  if ParamStr(2) = '--compact' then
  begin
    GHost.SetBounds(20, 20, 390, 780);
  end;
  GHost.Show;
  GOtherHost := TForm.CreateNew(nil);
  GOtherHost.SetBounds(1100, 20, 400, 500);
  GOtherHost.Show;
  GTarget := MountNyxLCLDiagramEditor(GEditor, GDocument, GHost);
  GEditor.WithOptions(LOptions.HistoryLimit(2)
    .HistoryByteBudget(3 * NyxUTF8ByteCount(GInitialBytes) + 512));
  GOther := MountNyxLCLDiagramEditor(GEditor, GDocument, GOtherHost);
  GEditor.WithOptions(LOptions);
  GCanvas := GTarget.Canvas as INyxLCLCanvasWorkspaceView;
  {$endif}
  GView := GTarget;
  Pump;
end;

procedure Capture(const ASuffix: String);
{$ifndef PAS2JS}
var
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
{$endif}
begin
  {$ifndef PAS2JS}

  { Native input qualification supplies --input-only instead of a capture
    prefix; the second argument still selects compact geometry. Do not rely on
    an empty Windows argument, which the Pascal command-line parser may retain
    as a quote. Ordinary explicit capture prefixes keep their existing behavior. }
  if (ParamStr(1) <> '') and (ParamStr(1) <> '--input-only') then
  begin
    LBitmap := GHost.GetFormImage;
    LPNG := TPortableNetworkGraphic.Create;
    try
      LPNG.Assign(LBitmap);
      LPNG.SaveToFile(ParamStr(1) + ASuffix + '.png');
    finally
      LPNG.Free;
      LBitmap.Free;
    end;
  end;
  {$endif}
end;

procedure DragSample(ASample: TDragSample; const AWorldPoint: TNyxCanvasPoint;
  APump: Boolean = True);
{$ifdef PAS2JS}
const
  CEvents: array[TDragSample] of String = ('pointerdown', 'pointermove', 'pointerup', 'lostpointercapture');
{$endif}
var
  LGeometry: INyxCanvasControlGeometry;
  LBounds: TNyxCanvasBounds;
  LLocal: TNyxCanvasPoint;
  {$ifdef PAS2JS}
  LFace: TJSHTMLElement;
  LOptions: TJSObject;
  LRect: TJSDOMRect;
  {$else}
  LFace: TControl;
  {$endif}
begin
  Check(Supports(GView.Canvas, INyxCanvasControlGeometry, LGeometry) and
    LGeometry.TryControlBounds(NyxCanvasItem('Diagram picture'),
      NyxControl('workflow-diagram-picture'), LBounds), 'drag driver reads its owned current viewport plane');
  LLocal := GView.Canvas.Camera.WorldToViewport(AWorldPoint).Offset(-LBounds.Left, -LBounds.Top);
  {$ifdef PAS2JS}
  LFace := GCanvas.RendererFor(NyxCanvasItem('Diagram picture')).ElementFor('workflow-diagram-picture');
  LRect := LFace.getBoundingClientRect;
  LOptions := TJSObject.new;
  LOptions['clientX'] := LRect.left + LLocal.X;
  LOptions['clientY'] := LRect.top + LLocal.Y;
  LOptions['pointerId'] := 1;
  LOptions['pointerType'] := 'mouse';
  LOptions['isPrimary'] := True;
  LOptions['button'] := 0;
  LOptions['buttons'] := 1;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;

  if ASample = ddsUp then
  begin
    LOptions['buttons'] := 0;
  end;
  LFace.dispatchEvent(TDiagramPointerEvent.new(CEvents[ASample], LOptions));
  {$else}
  LFace := GCanvas.RendererFor(NyxCanvasItem('Diagram picture')).ControlFor('workflow-diagram-picture');
  case ASample of
    ddsDown: TInputAccess(LFace).PrimaryDown(Round(LLocal.X), Round(LLocal.Y));
    ddsMove: TInputAccess(LFace).PrimaryMove(Round(LLocal.X), Round(LLocal.Y));
    ddsUp: TInputAccess(LFace).PrimaryUp(Round(LLocal.X), Round(LLocal.Y));
    ddsLostCapture:
      begin
        ReleaseNyxLCLPointer(LFace);
        TInputAccess(LFace).PrimaryMove(Round(LLocal.X), Round(LLocal.Y));
      end;
  end;
  {$endif}

  if APump then
  begin
    Pump;
  end;
end;

procedure EscapeManipulation;
{$ifdef PAS2JS}
var
  LOptions: TJSObject;
  LEvent: TJSEvent;
{$else}
var
  LKey: Word;
  LFace: TWinControl;
{$endif}
begin
  {$ifdef PAS2JS}
  LOptions := TJSObject.new;
  LOptions['key'] := 'Escape';
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LEvent := TDiagramKeyEvent.new('keydown', LOptions);
  GTarget.Renderer.ElementFor(ID(ndepSelection)).dispatchEvent(LEvent);
  Check(LEvent.defaultPrevented, 'Escape consumes its browser default');
  {$else}
  LKey := $1B;
  LFace := GTarget.Renderer.FocusFor(ID(ndepSelection));
  TKeyAccess(LFace).OnKeyDown(LFace, LKey, []);
  Check(LKey = 0, 'Escape consumes its native default');
  {$endif}
  Pump;
end;

{$ifdef PAS2JS}
{ Yield actual host turns so scheduler previews run normally. No private queue
  drain or fake pointer-capture implementation replaces browser behavior. }
procedure DiagramTurn(AResolve, AReject: TJSPromiseResolver);
begin
  window.setTimeout(
    procedure
    begin
      AResolve(True);
    end, 25);
end;
{$endif}

procedure RunManipulation(ADisconnect: Boolean); {$ifdef PAS2JS}async;{$endif}
var
  LHistory: INyxDiagramHistoryView;
  LSaved: INyxDiagram;
  LBefore: TNyxText;
  LRevision: Integer;
  LSceneRevision: Integer;
  LPresentation: INyxCanvasLayoutView;
  LVertex: TNyxDiagramVertex;
  LPoint: TNyxCanvasPoint;
  LMinimum: TNyxCanvasBounds;
  LExpected: INyxDrawingPlan;
  LIndex: Integer;
  LFailed: Boolean;
  LEdge: TNyxDiagramEdge;
  LEdgeBefore: TNyxText;
  {$ifndef PAS2JS}
  LFace: TNyxLCLDrawing;
  {$endif}
begin
  LSaved := GView.Diagram;
  GView.ShowTools(False);
  GView.SelectVertex(NyxDiagramVertexRef('start'));
  LVertex := GView.Diagram.FindVertex(GView.Selection);
  GView.Canvas.ZoomAt(4, NyxCanvasPoint(0, 0));
  GView.Canvas.SetCamera(GView.Canvas.Camera.AtCenter(LVertex.Bounds.Center));
  Pump;
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@DiagramTurn)));{$endif}
  LBefore := GView.Diagram.ToData.ToJSON;
  Check(Supports(GView, INyxDiagramHistoryView, LHistory), 'manipulation exposes optional local graph history');
  LHistory.ClearHistory;
  LRevision := GView.Revision;
  LPoint := LVertex.Bounds.Center;
  LMinimum := DrawNyxDiagramEditor(GView.Diagram, GView.Selection,
    GView.SelectedEdge, 8 / GView.Canvas.Camera.Zoom).ViewBox;
  DragSample(ddsDown, LPoint);
  Check(GView.Manipulating, 'actual shape pointer-down captures manipulation authority: ' + GView.LastError);
  {$ifndef PAS2JS}
  LFace := TNyxLCLDrawing(GCanvas.RendererFor(NyxCanvasItem('Diagram picture')).ControlFor('workflow-diagram-picture'));
  Check(NyxLCLHasPointerCapture(LFace), 'native pointer capture belongs to this owned Drawing');
  {$endif}
  Check(Supports(GView.Canvas, INyxCanvasLayoutView, LPresentation),
    'the mounted canvas exposes its independent presentation revision');
  LSceneRevision := LPresentation.PresentationRevision;
  for LIndex := 1 to 8 do
  begin
    DragSample(ddsMove, LPoint.Offset(LIndex * 2, -LIndex), False);
  end;
  DragSample(ddsMove, LPoint.Offset(20.5, -8.5), False);
  Check((GView.Revision = LRevision) and (LPresentation.PresentationRevision = LSceneRevision) and
    (GView.Diagram.ToData.ToJSON = LBefore), 'a pointer burst queues presentation without accepting intermediate data');
  Pump;
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@DiagramTurn)));{$endif}
  Check((GView.Revision = LRevision) and (LPresentation.PresentationRevision = LSceneRevision + 1) and
    (GView.DisplayDiagram.FindVertex(LVertex.Reference).Bounds.Left = LVertex.Bounds.Left + 20.5) and
    (GView.Diagram.ToData.ToJSON = LBefore), 'one deferred preview coalesces the burst and leaves accepted model exact');
  Check((LHistory.UndoCount = 0) and (LHistory.RedoCount = 0),
    'coalesced pointer previews do not append local graph history');
  LExpected := DrawNyxDiagramEditor(GView.DisplayDiagram, GView.Selection,
    GView.SelectedEdge, 8 / GView.Canvas.Camera.Zoom, LMinimum);
  {$ifndef PAS2JS}
  LFace := TNyxLCLDrawing(GCanvas.RendererFor(NyxCanvasItem('Diagram picture')).ControlFor('workflow-diagram-picture'));
  Check(LFace.Plan.ToData.ToJSON = LExpected.ToData.ToJSON, 'actual native face displays the independent live graph preview');
  {$endif}
  LFailed := False;
  try
    GView.SetGeometry(LVertex.Reference, LVertex.Bounds, LRevision);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and GView.Manipulating, 'an accepted-model command refuses while its preview is active');
  DragSample(ddsUp, LPoint.Offset(20.5, -8.5));
  Check(not GView.Manipulating and (GView.Revision = LRevision + 1) and
    (GView.Diagram.FindVertex(LVertex.Reference).Bounds.Left = LVertex.Bounds.Left + 20.5) and
    (GView.Diagram.FindVertex(LVertex.Reference).Bounds.Top = LVertex.Bounds.Top - 8.5) and
    (GView.DisplayDiagram.ToData.ToJSON = GView.Diagram.ToData.ToJSON),
    'release publishes the exact final fractional move once: ' + GView.LastError);
  Pump;
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@DiagramTurn)));{$endif}
  Check(GView.Revision = LRevision + 1, 'retired queued samples cannot publish again');
  Check(LHistory.UndoCount = 1, 'one released pointer gesture creates exactly one local command');
  LHistory.Undo(GView.Revision);
  Check(GView.Diagram.ToData.ToJSON = LBefore, 'one local Undo restores the complete pre-drag graph');
  LHistory.Redo(GView.Revision);

  LVertex := GView.Diagram.FindVertex(LVertex.Reference);
  LRevision := GView.Revision;
  GView.Canvas.ZoomAt(2, NyxCanvasPoint(0, 0));
  GView.RevealSelection;
  Pump;
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@DiagramTurn)));{$endif}
  LPoint := NyxDiagramGripPoint(LVertex.Bounds, ndgSouthEast);
  DragSample(ddsDown, LPoint);
  DragSample(ddsMove, LPoint.Offset(16.5, 10));
  Check(GView.Diagram.FindVertex(LVertex.Reference).Bounds.Width = LVertex.Bounds.Width,
    'resize preview does not mutate accepted dimensions');
  DragSample(ddsUp, LPoint.Offset(16.5, 10));
  Check((GView.Revision = LRevision + 1) and
    (GView.Diagram.FindVertex(LVertex.Reference).Bounds.Width = LVertex.Bounds.Width + 16.5) and
    (GView.Diagram.FindVertex(LVertex.Reference).Bounds.Height = LVertex.Bounds.Height + 10) and
    (GView.Diagram.FindVertex(LVertex.Reference).Bounds.Left = LVertex.Bounds.Left),
    'captured corner drag resizes once around its pinned opposite corner');

  LVertex := GView.Diagram.FindVertex(LVertex.Reference);
  LPoint := LVertex.Bounds.Center;
  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  DragSample(ddsDown, LPoint);
  DragSample(ddsMove, LPoint.Offset(8, 6));
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@DiagramTurn)));{$endif}
  DragSample(ddsLostCapture, LPoint.Offset(8, 6));
  Check(not GView.Manipulating and (GView.Revision = LRevision) and
    (GView.DisplayDiagram.ToData.ToJSON = LBefore), 'lost capture restores accepted paint without a model revision');
  DragSample(ddsDown, LPoint);
  DragSample(ddsMove, LPoint.Offset(4, 4));
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@DiagramTurn)));{$endif}
  EscapeManipulation;
  Check(not GView.Manipulating and (GView.Diagram.ToData.ToJSON = LBefore) and
    (GView.DisplayDiagram.ToData.ToJSON = LBefore), 'Escape cancels model and projection together');
  DragSample(ddsDown, LPoint);
  DragSample(ddsMove, LPoint.Offset(4, 2));
  GView.Canvas.PanBy(10, -6);
  Pump;
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@DiagramTurn)));{$endif}
  Check(not GView.Manipulating and (GView.Diagram.ToData.ToJSON = LBefore),
    'a changed camera retires the pinned gesture plane');
  DragSample(ddsUp, LPoint.Offset(4, 2));
  GView.ShowTools(True);
  DragSample(ddsDown, LPoint);
  DragSample(ddsMove, LPoint.Offset(3, 3), False);
  WriteInput(ndepCaption, 'New text during a drag');
  DragSample(ddsUp, LPoint.Offset(3, 3));
  Check(not GView.Manipulating and GView.HasPendingProperties and
    (ReadInput(ndepCaption) = 'New text during a drag') and (GView.Diagram.ToData.ToJSON = LBefore),
    'new property text wins over a stale preview and remains exact');
  Click(ndepRestore);
  GView.ShowTools(False);

  LEdge := GView.Diagram.FindEdge(NyxDiagramEdgeRef('review'));
  LEdgeBefore := LEdge.ToData.ToJSON;
  GView.SelectEdge(LEdge.Reference);
  GView.ShowDrawTools(True);
  Click(ndepReconnectFinish);
  Check(GView.ReconnectingEdge.Name = LEdge.Reference.Name, 'ordinary reconnect command arms the existing edge');
  GView.SelectVertex(NyxDiagramVertexRef('finish'));
  Click(ndepFinishConnection);
  LEdge := GView.Diagram.FindEdge(LEdge.Reference);
  Check((LEdge.Finish.Vertex.Name = 'finish') and (LEdge.Reference.Name = 'review') and
    (LEdge.ToData.Field('start').ToJSON =
      TNyxDataValue.ParseJSON(LEdgeBefore).Field('start').ToJSON),
    'button reconnection retains identity/opposite endpoint while replacing the chosen target');
  GView.ReconnectEdge(LEdge.Reference, ndceStart,
    NyxDiagramFreeEnd(NyxCanvasPoint(-33.5, 12.25)), GView.Revision);
  Check(GView.Diagram.FindEdge(LEdge.Reference).Start.Point.X = -33.5,
    'typed direct reconnection accepts exact free endpoints');
  GView.ShowDrawTools(False);
  GView.Canvas.ZoomAt(1, NyxCanvasPoint(0, 0));
  GView.SelectVertex(LVertex.Reference);
  GView.RevealSelection;
  Pump;
  {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@DiagramTurn)));{$endif}
  PictureMatches;
  Capture('-manipulated');
  GView.ReplaceDiagram(LSaved, GView.Revision);

  if ADisconnect then
  begin
    GView.SelectVertex(NyxDiagramVertexRef('start'));
    LPoint := GView.Diagram.FindVertex(GView.Selection).Bounds.Center;
    GView.RevealSelection;
    Pump;
    {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@DiagramTurn)));{$endif}
    DragSample(ddsDown, LPoint);
    DragSample(ddsMove, LPoint.Offset(12, 8), False);
    GView.Disconnect;
    Pump;
    {$ifdef PAS2JS}await(TJSPromise.resolve(TJSPromise.new(@DiagramTurn)));{$endif}
    Check(not GView.Connected, 'disconnect retires a captured gesture and its queued preview');
    {$ifndef PAS2JS}
    Check((GetCaptureControl = nil) and (GFailure.Message = ''), 'native capture and callbacks retire before target destruction');
    {$endif}
  end;
end;

{$I nyx.test.diagram.history.controls.inc}

procedure RunAuthoring;
var
  LSaved: INyxDiagram;
  LBefore: TNyxText;
  LRevision: Integer;
  LFailed: Boolean;
  LRoute: TNyxDiagramRoute;
  LStart: TNyxDiagramVertexRef;
  LFinish: TNyxDiagramVertexRef;
begin
  LSaved := GView.Diagram;
  GView.ReplaceDiagram(NewNyxDiagram, GView.Revision);
  GView.Canvas.SetCamera(GView.Canvas.Camera.AtCenter(NyxCanvasPoint(0, 0)));
  Click(ndepToggleDraw);
  Check(GView.DrawToolsExpanded and not GView.ToolsExpanded, 'Draw alternates with Properties without shrinking text');
  Click(ndepAddRectangle);
  LStart := GView.Selection;
  Check((LStart.Name = 'Rectangle1') and (GView.Diagram.VertexCount = 1) and
    (GView.Diagram.Vertex(0).Shape = ndsRectangle), 'actual palette creates and selects a typed rectangle');
  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  LFailed := False;
  try
    GView.AddVertex(GView.Diagram.Vertex(0), LRevision);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GView.Revision = LRevision) and (GView.Diagram.ToData.ToJSON = LBefore),
    'duplicate Add refuses replacement and retains exact local intent');
  Click(ndepAddEllipse);
  LFinish := GView.Selection;
  Check((LFinish.Name = 'Ellipse1') and (GView.Diagram.Vertex(1).Shape = ndsEllipse),
    'actual palette creates a specialized ellipse with a readable default name');
  GView.SetGeometry(LStart, NyxCanvasBounds(-180, 0, 160, 100), GView.Revision);
  GView.SetGeometry(LFinish, NyxCanvasBounds(180, 0, 160, 100), GView.Revision);
  GView.SelectVertex(LStart);
  Click(ndepStartOrthogonal);
  LRevision := GView.Revision;
  Check(GView.ConnectionStart.Name = LStart.Name, 'ordinary start button arms an exact source');
  GView.Canvas.PanBy(8, -4);
  GView.Canvas.SetCamera(GView.Canvas.Camera.AtCenter(NyxCanvasPoint(260, 50)));
  Check(GView.ConnectionStart.Name = LStart.Name, 'navigation retains the two-step connection intent');
  SelectPointer(NyxCanvasPoint(260, 50));
  Check((GView.Revision = LRevision + 1) and (GView.Diagram.EdgeCount = 1) and
    (GView.SelectedEdge.Name = 'Connection1') and not GView.Selection.Defined and
    not GView.ConnectionStart.Defined, 'actual pointer completes and selects one new connection atomically');
  PictureMatches;
  Click(ndepStraightEdge);
  Check(GView.Diagram.Edge(0).Routing = ndrStraight, 'ordinary route button edits the selected edge');
  Click(ndepOrthogonalEdge);
  Check(GView.Diagram.Edge(0).Routing = ndrOrthogonal, 'orthogonal route remains typed and reversible');
  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  LRoute := GView.Diagram.Route(GView.SelectedEdge);
  GView.SelectVertex(LStart);
  GView.Canvas.SetCamera(GView.Canvas.Camera.AtCenter(LRoute.Point(1)));
  SelectPointer(LRoute.Point(1));
  Check((GView.SelectedEdge.Name = 'Connection1') and (GView.Revision = LRevision) and
    (GView.Diagram.ToData.ToJSON = LBefore), 'pointer picks a connector without authoring highlighted styles');
  Click(ndepToggleTools);
  Check(GView.DrawToolsExpanded and not GView.ToolsExpanded,
    'selected connection Properties presents its route controls rather than ownerless shape fields');
  PictureMatches;
  Click(ndepNextEdge);
  Check(GView.SelectedEdge.Name = 'Connection1', 'ordered connection navigation works without pointer precision');
  GView.SelectVertex(LStart);
  Click(ndepStartStraight);
  GView.SelectVertex(LFinish);
  Click(ndepFinishConnection);
  Check((GView.Diagram.EdgeCount = 2) and (GView.SelectedEdge.Name = 'Connection2') and
    (GView.Diagram.Edge(1).Routing = ndrStraight), 'selection buttons complete a distinct parallel edge');
  GView.BeginConnection(LStart, ndrOrthogonal);
  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  LFailed := False;
  try
    GView.CompleteConnection(LFinish, LRevision - 1);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GView.Diagram.ToData.ToJSON = LBefore) and
    (GView.ConnectionStart.Name = LStart.Name), 'stale completion preserves the exact graph and pending source');
  Click(ndepCancelConnection);
  Check(not GView.ConnectionStart.Defined and (GView.Revision = LRevision), 'Cancel authors no graph change');
  GView.SelectVertex(LStart);
  Click(ndepStartOrthogonal);
  Click(ndepFinishConnection);
  Check((GView.Diagram.EdgeCount = 3) and
    (GView.Diagram.Route(GView.SelectedEdge).Count >= 4), 'two-step creation supports a routed self-loop');
  PictureMatches;
  Capture('-connections');
  GView.SelectVertex(LStart);
  GView.ShowTools(True);
  WriteInput(ndepCaption, 'Unfinished shape');
  Click(ndepToggleDraw);
  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  Click(ndepAddDiamond);
  Check(GView.HasPendingProperties and (GView.Diagram.ToData.ToJSON = LBefore) and
    (GView.Revision = LRevision), 'hidden unfinished properties refuse creation without being discarded');
  Click(ndepStartStraight);
  Check(not GView.ConnectionStart.Defined and GView.HasPendingProperties,
    'unfinished properties also refuse starting a new connection');
  GView.ShowTools(True);
  Check(ReadInput(ndepCaption) = 'Unfinished shape', 'alternating panels retains the same exact property draft');
  Click(ndepRestore);
  Click(ndepToggleDraw);
  Click(ndepStartOrthogonal);
  GView.SetGeometry(LStart, NyxCanvasBounds(-190, 10, 160, 100), GView.Revision);
  Check(not GView.ConnectionStart.Defined, 'applied graph changes retire prior connection-generation intent');
  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  Click(ndepRemoveVertex);
  Check((GView.Revision = LRevision) and (GView.Diagram.ToData.ToJSON = LBefore),
    'connected removal refuses until incident-line removal is explicit');
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GTarget.Renderer.InputFor(ID(ndepIncidentEdges))).checked := True;
  GTarget.Renderer.InputFor(ID(ndepIncidentEdges)).dispatchEvent(TJSEvent.new('change'));
  {$else}
  TCheckBox(GTarget.Renderer.InputFor(ID(ndepIncidentEdges))).Checked := True;
  {$endif}
  Click(ndepRemoveVertex);
  Check((GView.Revision = LRevision + 1) and (GView.Diagram.VertexCount = 1) and
    (GView.Diagram.EdgeCount = 0) and (GView.Selection.Name = LFinish.Name),
    'explicit removal retires all incident edges in one complete publication');
  Click(ndepAddDiamond);
  Check((GView.Selection.Name = 'Decision1') and (GView.Diagram.Vertex(1).Shape = ndsDiamond),
    'the decision recipe is directly authorable');
  GView.SetGeometry(GView.Selection, NyxCanvasBounds(-140, -10, 160, 100), GView.Revision);
  Click(ndepStartStraight);
  GView.SelectVertex(LFinish);
  Click(ndepFinishConnection);
  Click(ndepRemoveEdge);
  Check((GView.Diagram.EdgeCount = 0) and (GView.Diagram.VertexCount = 2),
    'ordinary Remove line retains both endpoint shapes');
  LBefore := GView.Diagram.ToData.ToJSON;
  RetainNyxControl(GTarget.Renderer.Root).Configure.ReadOnly(True).Done;
  GTarget.Renderer.Sync;
  Click(ndepAddRectangle);
  Check(GView.Diagram.ToData.ToJSON = LBefore, 'read-only input refuses the new authoring actions');
  RetainNyxControl(GTarget.Renderer.Root).Configure.ReadOnly(False).Done;
  GTarget.Renderer.Sync;
  Click(ndepActualSize);
  PictureMatches;
  Capture('-draw');
  Click(ndepToggleDraw);
  Capture('-composed');
  GView.ReplaceDiagram(LSaved, GView.Revision);
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
var
  LHistory: INyxDiagramHistoryView;
  LBefore: TNyxText;
  LRevision: Integer;
  LFailed: Boolean;
  LVertex: TNyxDiagramVertex;
  LRoute: TNyxDiagramRoute;
  LCamera: TNyxCanvasCamera;
  LCase: Integer;
  LCreator: TCreatorDiagram;
  LCreatorLease: INyxDiagram;
  {$ifdef PAS2JS}
  LInput: TJSHTMLElement;
  {$else}
  LInput: TControl;
  LWorker: TWorker;
  LStream: INyxEventStream;
  LExecution: INyxExecution;
  LDeadline: QWord;
  {$endif}
begin
  Check(GView.Connected and GOther.Connected, 'two independent ordinary mounts');
  Check((GView.Revision = 0) and (GView.Selection.Name = 'start'), 'initial exact snapshot and selection');
  Check(not GView.ToolsExpanded and not GView.HasPendingProperties, 'compact defaults preserve canvas space');
  PictureMatches;
  GView.Canvas.SetCamera(GView.Canvas.Camera.AtCenter(NyxCanvasPoint(180, 40)));
  SelectPointer(NyxCanvasPoint(300, 40));
  Check(GView.Selection.Name = 'decision', 'actual pointer selects the mathematical shape');
  Check(GView.Revision = 0, 'selection authors no graph mutation');
  Click(ndepSelection);
  GEditor.WithOptions(NyxDiagramEditorOptions.Nudge(99));
  ArrowRight;
  Check((GView.Revision = 1) and (GView.Diagram.Vertex(1).Bounds.Left = 250),
    'actual keyboard nudge changes only selected geometry');
  Check(GOther.Diagram.ToData.ToJSON = GInitialBytes, 'another mount retains its own exact graph');
  Check(GEditor.InitialDiagram.ToData.ToJSON = GInitialBytes, 'runtime edits preserve the descriptor initial snapshot');
  PictureMatches;
  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  LFailed := False;
  try
    GView.SetGeometry(NyxDiagramVertexRef('decision'), NyxCanvasBounds(0, 0, 120, 100), 0);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GView.Revision = LRevision) and (GView.Diagram.ToData.ToJSON = LBefore),
    'stale geometry preserves the whole accepted graph');
  Click(ndepToggleTools);
  Check(GView.ToolsExpanded, 'ordinary Properties button opens reusable form');
  {$ifdef PAS2JS}
  LInput := GTarget.Renderer.InputFor(ID(ndepX));
  {$else}
  LInput := GTarget.Renderer.InputFor(ID(ndepX));
  {$endif}
  WriteInput(ndepX, '-');
  {$ifdef PAS2JS}
  TJSHTMLInputElement(LInput).selectionStart := 1;
  TJSHTMLInputElement(LInput).selectionEnd := 1;
  {$else}
  TEdit(LInput).SelStart := 1;
  {$endif}
  Check(GView.HasPendingProperties and (ReadInput(ndepX) = '-'), 'unfinished native/browser number remains exact');
  LCamera := GView.Canvas.Camera;
  Click(ndepApply);
  Check((GView.LastError <> '') and GView.HasPendingProperties and
    (GView.Revision = LRevision) and (GView.Diagram.ToData.ToJSON = LBefore),
    'invalid Apply reports failure without publishing graph or discarding input');
  Check((GView.Canvas.Camera.WorldCenter.X = LCamera.WorldCenter.X) and
    (GView.Canvas.Camera.WorldCenter.Y = LCamera.WorldCenter.Y), 'invalid properties retain navigation center');
  ArrowRight;
  Check((GView.Revision = LRevision) and (ReadInput(ndepX) = '-') and
    (GView.Diagram.ToData.ToJSON = LBefore), 'a refused keyboard nudge consumes its default and retains the whole draft');
  for LCase := 0 to 2 do
  begin
    LFailed := False;
    try
      case LCase of
        0: GView.SelectVertex(NyxDiagramVertexRef('finish'));
        1: GView.ReplaceDiagram(GInitial, LRevision);
        2: GView.NudgeSelection(1, 0, LRevision);
      end;
    except
      on E: ENyxDiagram do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (ReadInput(ndepX) = '-') and
      (GView.Diagram.ToData.ToJSON = LBefore) and (GView.Selection.Name = 'decision'),
      'unfinished properties guard selection/replacement/geometry ' + IntToStr(LCase));
  end;
  Click(ndepToggleTools);
  GView.Canvas.PanBy(12, -8);
  Click(ndepZoomIn);
  Click(ndepToggleTools);
  Check((ReadInput(ndepX) = '-') and GView.HasPendingProperties,
    'hide, pan, zoom and show preserve exact unfinished properties');
  Check(GTarget.Renderer.InputFor(ID(ndepX)) = LInput, 'same properties input survives navigation and collapse');
  {$ifdef PAS2JS}
  Check(TJSHTMLInputElement(LInput).selectionStart = 1, 'unfinished property caret survives collapse/navigation');
  {$else}
  Check(TEdit(LInput).SelStart = 1, 'unfinished property caret survives collapse/navigation');
  {$endif}
  Click(ndepRestore);
  Check(not GView.HasPendingProperties and (ReadInput(ndepX) = '250'), 'deliberate Revert restores accepted field values');
  WriteInput(ndepX, '310.125');
  WriteInput(ndepY, '125.75');
  WriteInput(ndepWidth, '180.5');
  WriteInput(ndepHeight, '140.25');
  WriteInput(ndepCaption, 'Review details');
  Click(ndepApply);
  LVertex := GView.Diagram.FindVertex(NyxDiagramVertexRef('decision'));
  Check((GView.Revision = LRevision + 1) and not GView.HasPendingProperties,
    'one valid properties action publishes once');
  Check((LVertex.Bounds.Left = 310.125) and (LVertex.Bounds.Top = 125.75) and
    (LVertex.Bounds.Width = 180.5) and (LVertex.Bounds.Height = 140.25) and
    (LVertex.Caption = 'Review details'), 'complete fractional geometry and caption stay exact');
  LRoute := GView.Diagram.Route(NyxDiagramEdgeRef('review'));
  Check((LRoute.Point(LRoute.Count - 1).X = 310.125) and
    (LRoute.Point(LRoute.Count - 1).Y = 195.875), 'published scene and graph reroute together');
  Check(GView.LastError = '', 'successful callback clears the prior diagnostic');
  Click(ndepActualSize);
  Check(GView.Canvas.Camera.Zoom = 1, '100 percent restores actual-size presentation');
  PictureMatches;
  Capture('-properties');
  Click(ndepToggleTools);
  Click(ndepFit);
  Check(GView.Canvas.Camera.Zoom > 0, 'ordinary Fit navigates complete diagram bounds');
  Click(ndepActualSize);
  Check((GView.Canvas.Camera.Zoom = 1) and not GView.ToolsExpanded,
    'actual-size focus and collapsed properties preserve readable canvas area');
  PictureMatches;
  Capture('-canvas');
  Check(NyxDiagramFromData(GView.Diagram.ToData).ToData.ToJSON = GView.Diagram.ToData.ToJSON,
    'complete edited intent exports independently of its picture');
  Check((GInitial.ToData.ToJSON = GInitialBytes) and (GOther.Revision = 0) and
    (GOther.Diagram.ToData.ToJSON = GInitialBytes), 'retained initial and independent mount stay exact');
  {$ifdef PAS2JS}await(RunHistoryLimits);{$else}RunHistoryLimits;{$endif}
  LBefore := GView.Diagram.ToData.ToJSON;
  RetainNyxControl(GTarget.Renderer.Root).Configure.ReadOnly(True).Done;
  GTarget.Renderer.Sync;
  Click(ndepRight);
  Check(GView.Diagram.ToData.ToJSON = LBefore, 'read-only editor refuses an ordinary geometry action');
  RetainNyxControl(GTarget.Renderer.Root).Configure.ReadOnly(False).Enabled(False).Done;
  GTarget.Renderer.Sync;
  Click(ndepNext);
  Check((GView.Diagram.ToData.ToJSON = LBefore) and (GView.Selection.Name = 'decision'),
    'disabled editor refuses ordinary input');
  RetainNyxControl(GTarget.Renderer.Root).Configure.Enabled(True).Done;
  GTarget.Renderer.Sync;

  {$ifndef PAS2JS}
  LBefore := GView.Diagram.ToData.ToJSON;
  LWorker := TWorker.Create(True);
  LWorker.View := GView;
  LWorker.Start;
  LWorker.WaitFor;
  Check((LWorker.Refusals = 6) and (GView.Diagram.ToData.ToJSON = LBefore),
    'three direct worker UI calls refuse before touching live state');
  LWorker.Free;
  Check(GFailure.Message = '', 'no framework failures escaped event/layout ownership');
  {$endif}
  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  LFailed := False;
  try
    GView.SetGeometry(NyxDiagramVertexRef('decision'),
      NyxCanvasBounds(0, 0, 1000000, 1000000), LRevision);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GView.Revision = LRevision) and
    (GView.Diagram.ToData.ToJSON = LBefore) and not GView.HasPendingProperties,
    'refused native allocation preserves accepted graph and complete properties');
  LCreator := TCreatorDiagram.Create;
  LCreatorLease := LCreator;
  LCreator.Base := GView.Diagram;
  LCreator.Target := GView;
  LCreator.Mode := dcmProperties;
  { Ordinary input correctly refuses writes into hidden controls. Keep the
    properties visible so this callout exercises newly admitted input, rather
    than a synthetic edit that the existing interaction gate must reject. }
  GView.ShowTools(True);
  LFailed := False;
  try
    GView.ReplaceDiagram(LCreatorLease, LRevision, True);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LCreator.Calls = 1) and (ReadInput(ndepX) = '-created') and
    GView.HasPendingProperties and (GView.Revision = LRevision) and
    (GView.Diagram.ToData.ToJSON = LBefore),
    'creator-side fresh properties win over an explicitly discarded older proposal: refused=' +
      BoolToStr(LFailed, True) + ', calls=' + IntToStr(LCreator.Calls) +
      ', field=' + ReadInput(ndepX) + ', pending=' +
      BoolToStr(GView.HasPendingProperties, True) + ', revision=' + IntToStr(GView.Revision));
  Click(ndepRestore);
  LCreator.Mode := dcmOrdinary;
  GView.ReplaceDiagram(LCreatorLease, LRevision);
  Check((LCreator.Calls = 2) and (GView.Revision = LRevision + 1) and
    (GView.Diagram.ToData.ToJSON = LBefore), 'alternate complete implementation captures once per admission');
  LCreator.Base := GOther.Diagram;
  LCreator.Target := GOther;
  LCreator.Mode := dcmDisconnect;
  LFailed := False;
  try
    GOther.ReplaceDiagram(LCreatorLease, GOther.Revision);
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and not GOther.Connected and GView.Connected,
    'creator-side revocation refuses through the typed boundary without touching another mount');
  LCreatorLease := nil;
  {$ifndef PAS2JS}
  GView.ShowTools(True);
  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  LStream := GTarget.Renderer.Events.On(NyxControlEvents(ID(ndepRight)), ntClick);
  LStream.Policy(neThreaded);
  Click(ndepRight);
  LExecution := LStream.Registrations[0].LastExecution;
  Check(LExecution <> nil, 'worker-policy callback owns an execution diagnostic');
  LDeadline := GetTickCount64 + 5000;
  while (LExecution.Status in [nesPending, nesRunning]) and (GetTickCount64 < LDeadline) do
  begin
    Pump;
    Sleep(1);
  end;
  Check((LExecution.Status = nesFailed) and (GView.Revision = LRevision) and
    (GView.Diagram.ToData.ToJSON = LBefore) and (GView.LastError = ''),
    'an unsupported worker UI callback refuses before reading its borrowed owner or changing UI/model');
  LStream.Policy(neSequential);
  {$endif}
  RunAuthoring;
  {$ifdef PAS2JS}await(RunManipulation(False));{$else}RunManipulation(False);{$endif}
  RunHistory;
  GDisconnect := GTarget.Renderer.Events.On(NyxControlEvents(ID(ndepNext)), ntClick)
    .Subscribe(TDisconnect.Create);
  Click(ndepNext);
  Check(not GView.Connected and not GDisconnect.Active, 'callback-triggered disconnect revokes all view authority');
  LFailed := False;
  try
    GView.GetDiagram;
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'retained disconnected editor refuses live access');
  Check(Supports(GView, INyxDiagramHistoryView, LHistory),
    'a retained retired view still exposes its guarded optional history interface');
  LFailed := False;
  try
    LHistory.GetUndoCount;
  except
    on E: ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'retained history authority refuses after editor retirement');
end;

procedure Cleanup;
begin

  if GView <> nil then
  begin
    GView.Disconnect;
  end;

  if GOther <> nil then
  begin
    GOther.Disconnect;
  end;
  GDisconnect := nil;
  GCanvas := nil;
  GTarget := nil;
  GView := nil;
  GOther := nil;
  GEditor := nil;
  GInitial := nil;
  FreeAndNil(GDocument);
  {$ifdef PAS2JS}
  GHost.remove;
  GOtherHost.remove;
  {$else}
  Pump;
  FreeAndNil(GOtherHost);
  FreeAndNil(GHost);
  Pump;
  Application.OnException := nil;
  FreeAndNil(GFailure);
  {$endif}
end;

{$ifdef PAS2JS}
procedure AfterLayout; async;
begin
  try

    if Pos('gestures-only', window.location.search) > 0 then
    begin
      await(RunManipulation(True));
    end
    else
    begin
      await(Run);
    end;
    Cleanup;
    window.setTimeout(procedure
      begin
        document.body.setAttribute('data-diagram-editor', 'passed');
        document.body.setAttribute('data-diagram-editor-checks', IntToStr(GChecks));
        document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' diagram editor checks';
      end, 80);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-diagram-editor', 'failed');
      document.body.setAttribute('data-diagram-editor-error', E.Message);
    end;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}
    Application.Initialize;
    GFailure := TFailure.Create;
    Application.OnException := GFailure.Failed;
    {$endif}
    Setup;
    {$ifdef PAS2JS}
    window.setTimeout(@AfterLayout, 180);
    {$else}

    if ParamStr(3) = '--gestures-only' then
    begin
      RunManipulation(True);
    end
    else
    begin
      Run;
    end;
    Cleanup;
    WriteLn('PASS ', GChecks, ' diagram editor checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-diagram-editor', 'failed');
      document.body.setAttribute('data-diagram-editor-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      Cleanup;
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
