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

program nyx_program_logic_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef NYX_PIPELINE_NODE_FACES}{$define NYX_CANVAS_CONTROL_CONNECTORS}{$endif}
{$ifdef NYX_SAVED_CANVAS_ROUTES}{$define NYX_CONTROL_ROUTES_RUNTIME}{$endif}
{$ifdef NYX_CANVAS_CONTROL_CONNECTORS}{$define NYX_CANVAS_ROUTE_AUTHORING}{$endif}
{$ifdef NYX_CANVAS_ROUTE_AUTHORING}{$define NYX_PROGRAM_LOGIC_CANVAS}{$endif}
{$ifdef NYX_CANVAS_ROUTE_AUTHORING}{$define NYX_INPUT_ROUTE_CALLBACKS}{$endif}
{$ifdef NYX_CONTROL_ROUTES_RUNTIME}
{$define NYX_PROGRAM_LOGIC_CANVAS}
{$define NYX_PROGRAM_LOGIC_COMPANION}
{$define NYX_INPUT_ROUTE_CALLBACKS}
{$endif}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.state, nyx.model, nyx.codec,
  nyx.events, nyx.behavior, nyx.scheduler, nyx.application.context,
  nyx.flow.routing, nyx.flow.routes,
  nyx.flow.route.model, nyx.flow.declarations, nyx.flow.types, nyx.programs.declarations,
  nyx.test.programs.logic
  {$ifdef NYX_CANVAS_ROUTE_AUTHORING}, nyx.controls, nyx.root.types,
    nyx.canvas.routes, nyx.canvas.scene, nyx.canvas.declarations,
    nyx.flow.editing, nyx.flow.controls.editor, nyx.tool.canvas{$endif}
  {$ifdef NYX_CANVAS_CONTROL_CONNECTORS}, nyx.tool.wires, nyx.tool.ports,
    nyx.canvas.observers, nyx.flow, nyx.logic.programs{$endif}
  {$ifdef NYX_PROGRAM_LOGIC_CANVAS}, nyx.canvas.types, nyx.canvas.geometry,
    nyx.canvas.workspace, nyx.view.behavior, nyx.composition
    {$ifndef NYX_CANVAS_ROUTE_AUTHORING}, nyx.canvas.declarations{$endif}{$endif}
  {$ifdef NYX_PROGRAM_LOGIC_COMPANION}, nyx.generated.view{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser, nyx.render.browser
    {$ifdef NYX_PROGRAM_LOGIC_CANVAS}, nyx.canvas.browser{$endif}
    {$ifdef NYX_CANVAS_ROUTE_AUTHORING}, nyx.tool.canvas.browser{$endif}
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, Graphics,
    nyx.widgets.lcl, nyx.application.lcl, nyx.render.lcl
    {$ifdef NYX_PROGRAM_LOGIC_CANVAS}, nyx.canvas.lcl{$endif}
    {$ifdef NYX_CANVAS_ROUTE_AUTHORING}, nyx.tool.canvas.lcl{$endif}
    {$ifdef NYX_CANVAS_CONTROL_CONNECTORS}, Windows, Types{$endif}{$endif};

type
  { Managed ordinary callback counts actual event dispatch. It owns no control,
    application or result receiver, so route and static callbacks remain separate. }
  TOrdinary = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

var
  GDocument: TNyxDocument;
  GOtherDocument: TNyxDocument;
  GBefore: TNyxText;
  GBindings: INyxFlowRouteBinding;
  GContext: INyxApplicationContext;
  GCallbacks: array of INyxEventSubscription;
  GCallbackCount: Integer;
  GChecks: Integer;
  GStage: Integer;
  GRevision: Integer;
  GExpectedFailures: Integer;
  GDone: Boolean;
  GCaptured: Boolean;
  GReturned: Boolean;
  {$ifdef NYX_PROGRAM_LOGIC_CANVAS}
  GCanvasView: {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};
  {$endif}
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GOther: TNyxBrowserApplication;
  GHost: TJSHTMLElement;
  GOtherHost: TJSHTMLElement;
  GStarted: Double;
  {$else}
  GApp: TNyxLCLApplication;
  GOther: TNyxLCLApplication;
  GStarted: QWord;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Actual program route: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TOrdinary.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Inc(GCallbackCount);
end;

{ Borrow the current item renderer only for one input/read. Navigation may retire
  it; the retained canvas face and route reports remain independently revocable. }
function MainView: {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif};
begin
  {$ifdef NYX_PROGRAM_LOGIC_CANVAS}
  Result := GCanvasView.RendererFor(NyxCanvasItem('Greeting'));
  {$else}
  Result := GApp.View;
  {$endif}
end;

{$ifdef NYX_PROGRAM_LOGIC_CANVAS}
function SetupCanvas: Boolean;
var
  LRuntime: INyxCanvasRuntimeView;
begin
  Result := False;

  if (GCanvasView = nil) or not GCanvasView.Connected then
  begin
    GCanvasView := GApp.View.CanvasView(GDocument.Find('program-tools')
      .Part(NyxPart('workspace')).ID) as
      {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};
  end;
  GCanvasView.Refresh;
  GCanvasView.Execute(nccFit);

  if GCanvasView.MountedCount = 0 then
  begin
    Exit;
  end;
  Check(Supports(GCanvasView, INyxCanvasRuntimeView, LRuntime),
    'embedded program uses the public canvas runtime ownership face');
  GBindings := LRuntime.BehaviorFor(NyxCanvasItem('Greeting')).Routes;
  Result := True;
end;
{$endif}

procedure SubscribeCallbacks;
begin
  GCallbacks := nil;
  SetLength(GCallbacks, 2);
  {$ifdef NYX_INPUT_ROUTE_CALLBACKS}
  { These registrations share the exact input event authored through Studio.
    The companion owns graph routing; ordinary callbacks remain independent. }
  GCallbacks[0] := MainView.Events.On(NyxControlEvents('name-input', niRuntime), ntChange)
    .Subscribe(TOrdinary.Create);
  GCallbacks[1] := MainView.Events.On(NyxControlEvents('name-input', niRuntime), ntChange)
    .Subscribe(TOrdinary.Create);
  {$else}
  GCallbacks[0] := MainView.Events.On(NyxControlEvents('greet-button', niRuntime), ntClick)
    .Subscribe(TOrdinary.Create);
  GCallbacks[1] := MainView.Events.On(NyxControlEvents('greet-button', niRuntime), ntClick)
    .Subscribe(TOrdinary.Create);
  {$endif}
end;

procedure EditName(const AValue: TNyxText; AOther: Boolean = False);
begin
  {$ifdef PAS2JS}

  if AOther then
  begin
    TJSHTMLInputElement(GOther.View.InputFor('name-input')).value := AValue;
    GOther.View.InputFor('name-input').dispatchEvent(TJSEvent.new('input'));
  end
  else
  begin
    TJSHTMLInputElement(MainView.InputFor('name-input')).value := AValue;
    MainView.InputFor('name-input').dispatchEvent(TJSEvent.new('input'));
  end;
  {$else}

  if AOther then
  begin
    TEdit(GOther.View.InputFor('name-input')).Text := AValue;
  end
  else
  begin
    TEdit(MainView.InputFor('name-input')).Text := AValue;
  end;
  {$endif}
end;

procedure Click(AOther: Boolean = False);
begin
  {$ifdef NYX_CONTROL_ROUTES_RUNTIME}
  { Editing the first application's input already dispatches its saved OnChange
    route. The independent second application deliberately retains its button. }

  if not AOther then
  begin
    Exit;
  end;
  {$endif}
  {$ifdef PAS2JS}

  if AOther then
  begin
    TJSHTMLButtonElement(GOther.View.ElementFor('greet-button')).click;
  end
  else
  begin
    TJSHTMLButtonElement(MainView.ElementFor('greet-button')).click;
  end;
  {$else}

  if AOther then
  begin
    TNyxLCLButton(GOther.View.ControlFor('greet-button')).Click;
  end
  else
  begin
    TNyxLCLButton(MainView.ControlFor('greet-button')).Click;
  end;
  {$endif}
end;

function Caption(AOther: Boolean = False): TNyxText;
begin
  {$ifdef PAS2JS}

  if AOther then
  begin
    Result := GOther.View.ElementFor('caption-label').textContent;
  end
  else
  begin
    Result := MainView.ElementFor('caption-label').textContent;
  end;
  {$else}

  if AOther then
  begin
    Result := TLabel(GOther.View.ControlFor('caption-label')).Caption;
  end
  else
  begin
    Result := TLabel(MainView.ControlFor('caption-label')).Caption;
  end;
  {$endif}
end;

function Idle(const ABinding: INyxFlowRouteBinding): Boolean;
var
  LReport: TNyxDataValue;
begin
  LReport := ABinding.Report(0);

  if LReport.Field('failed').AsInteger > GExpectedFailures then
  begin
    raise Exception.Create('Program route refused: ' + LReport.Field('failure').AsText);
  end;
  Result := LReport.Field('active').AsInteger = 0;
end;

{$ifdef NYX_CANVAS_ROUTE_AUTHORING}
procedure RetireRouteAuthoring; forward;
{$endif}

procedure Retire;
begin
  {$ifdef NYX_CANVAS_ROUTE_AUTHORING}RetireRouteAuthoring;{$endif}
  FreeAndNil(GApp);
  FreeAndNil(GOther);
  GBindings := nil;
  GContext := nil;
  {$ifdef NYX_PROGRAM_LOGIC_CANVAS}GCanvasView := nil;{$endif}
  GCallbacks := nil;
  FreeAndNil(GDocument);
  FreeAndNil(GOtherDocument);
  {$ifndef PAS2JS}
  { Cancelled UI couriers release their work ports on the next actual queue
    turn. Drain that turn after revoking all borrowed application receivers. }
  CheckSynchronize;
  Application.ProcessMessages;
  {$endif}
end;

{$ifdef NYX_CONTROL_ROUTES_RUNTIME}
{ Compare the entire compiler-produced model with an independent typed expected
  recipe before mounting. This consumer establishes constructor/application
  agreement, including scene/program/default metadata outside the new route;
  ordinary Studio authoring and source admission retain their separate receipts. }
procedure VerifyControlCompanion;
var
  LExpected: TNyxDocument;
  LRoutes: INyxFlowRoutes;
begin
  LExpected := ProgramToolCanvasDocument;
  try
    LRoutes := NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('Live greeting'), NyxFlowRef('Greeting flow'),
        ntChange, nfcmAfterDispatch)
        .InputEventValue(ProgramLogicEndpoint('Name'), nftText)
        .OutputState(ProgramLogicEndpoint('Caption'), NyxTextState('caption'))
        .Using(neUIQueue).Concurrent(nfrLatest).MaximumRuns(8));
    {$ifdef NYX_SAVED_CANVAS_ROUTES}
    LExpected.Canvases.Define(NyxCanvasRef('Greeting canvas'),
      NyxCanvasConnections(LExpected.Canvases.Definition(NyxCanvasRef('Greeting canvas')))
        .ForControl(NyxCanvasItem('Greeting'), NyxControl('name-input'), LRoutes)
        .ForControl(NyxCanvasItem('Greeting'), NyxControl('greet-button'), NewNyxFlowRoutes).Done);
    Check((NyxControlFlowRoutes(GDocument.Find('name-input')).Count = 0) and
      (NyxControlFlowRoutes(GDocument.Find('greet-button')).Count = 1),
      'per-item runtime masks leave both shared authored control route sets unchanged');
    {$else}
    SetNyxFlowRoutes(LExpected.Find('greet-button'), NewNyxFlowRoutes);
    SetNyxFlowRoutes(LExpected.Find('name-input'), LRoutes);
    {$endif}
    Check(TNyxCodec.Encode(GDocument) = TNyxCodec.Encode(LExpected),
      'the complete actual compiled companion equals the independent typed design');
  finally
    LExpected.Free;
  end;
end;
{$endif}

{$ifdef NYX_CANVAS_ROUTE_AUTHORING}
{$ifdef NYX_CANVAS_CONTROL_CONNECTORS}
{$I nyx.test.canvas.control.connectors.inc}
{$else}
{$I nyx.test.canvas.route.authoring.inc}
{$endif}
{$else}
procedure Step;
{$ifndef PAS2JS}
var
  LBitmap: TBitmap;
  LMonitor: INyxSchedulerMonitor;
{$endif}
begin
  case GStage of
    0:
    begin
      {$ifdef NYX_PROGRAM_LOGIC_CANVAS}

      if not SetupCanvas then
      begin
        Exit;
      end;
      {$endif}
      Check(GBindings.Count = 1, 'current owner attaches its one declared program route');
      Check((Caption = 'Ready for a greeting') and
        (GOther.State.GetValue(NyxTextState('caption')) = 'Ready for a greeting'),
        'mounting does not execute saved programs');
      SubscribeCallbacks;
      EditName('Grace');
      GRevision := GApp.State.Revision;
      Click;
      Check(GCallbackCount = 2, 'multiple ordinary callbacks precede the saved after-dispatch route');
    end;
    1:
    begin

      if not Idle(GBindings) then
      begin
        Exit;
      end;
      Check((Caption = 'Hello, Grace!') and (GApp.State.Revision = GRevision + 1),
        'actual control input runs the named program and publishes one visible state revision');
      Check(GOther.State.GetValue(NyxTextState('caption')) = 'Ready for a greeting',
        'independent applications retain separate runtime state');
      EditName('First');
      Click;
      EditName('Latest');
      Click;
    end;
    2:
    begin

      if not Idle(GBindings) then
      begin
        Exit;
      end;
      Check((Caption = 'Hello, Latest!') and (GCallbackCount = 6),
        'latest concurrency keeps callback order and only current program output');
      Check(GBindings.Report(0).Field('cancelled').AsInteger >= 1,
        'a superseded queued program is explicitly cancelled');
      EditName('Stale');
      Click;
      GApp.State.SetValue(NyxTextState('caption'), 'An intervening local edit');
      GExpectedFailures := 1;
    end;
    3:
    begin

      if not Idle(GBindings) then
      begin
        Exit;
      end;
      Check((Caption = 'An intervening local edit') and
        (GBindings.Report(0).Field('failed').AsInteger = 1) and
        (Pos('stale', GBindings.Report(0).Field('failure').AsText) > 0),
        'ordinary stale-revision refusal preserves intervening user state and target text');
      EditName('Retired view');
      Click;
      GApp.ShowPage('other');
    end;
    4:
    begin

      if not Idle(GBindings) then
      begin
        Exit;
      end;

      if not GReturned then
      begin
        Check(GApp.State.GetValue(NyxTextState('caption')) = 'An intervening local edit',
          'navigation revokes queued program publication');
        GApp.ShowPage('home');
        GReturned := True;
      end;
      {$ifdef NYX_PROGRAM_LOGIC_CANVAS}

      if not SetupCanvas then
      begin
        Exit;
      end;
      SubscribeCallbacks;
      GExpectedFailures := 0;
      {$endif}
      EditName('New view');
      Click;
    end;
    5:
    begin

      if not Idle(GBindings) then
      begin
        Exit;
      end;

      if not GCaptured then
      begin
        Check(Caption = 'Hello, New view!', 'saved program route works after a fresh page generation');
        Check(TNyxCodec.Encode(GDocument) = GBefore,
          'runtime execution never changes saved programs, routes or defaults');
        {$ifdef PAS2JS}
        document.body.setAttribute('data-capture-checkpoint', 'program-main');
        {$endif}
        GCaptured := True;
        {$ifdef PAS2JS}
        Exit;
        {$endif}
      end;
      {$ifdef PAS2JS}
      if document.body.getAttribute('data-capture-observed') <> 'program-main' then
      begin
        Exit;
      end;
      {$else}

      if ParamCount > 0 then
      begin
        LBitmap := GApp.Window.GetFormImage;
        try
          LBitmap.SaveToFile(ParamStr(1));
        finally
          LBitmap.Free;
        end;
      end;
      {$endif}
      EditName('Retired application');
      Click;
      GContext := GApp.Context;
      FreeAndNil(GApp);
      Check(not GContext.Active and GBindings.Report(0).Field('closed').AsBoolean,
        'application disposal revokes weak context and retained route inspection');
      EditName('Independent', True);
      Click(True);
    end;
    6:
    begin

      if not Idle(GOther.FlowRoutes) then
      begin
        Exit;
      end;
      Check(Caption(True) = 'Welcome, Independent!',
        'a second runtime resolves its own program definition and visible output');
      {$ifndef PAS2JS}
      Check(Supports(GOther.Context.Scheduler, INyxSchedulerMonitor, LMonitor) and
        (LMonitor.WorkerLoad.ActiveWorkers > 0),
        'native threaded program route completes through an entered worker pool');
      {$endif}
      {$ifdef PAS2JS}
      GOtherHost.style.removeProperty('display');
      {$endif}
      GDone := True;
    end;
  end;
  Inc(GStage);
end;

{$endif}

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if TJSDate.now - GStarted > {$ifdef NYX_CANVAS_ROUTE_AUTHORING}120000{$else}45000{$endif} then
    begin
      raise Exception.Create('Program application deadline at stage ' + IntToStr(GStage));
    end;
    Step;

    if GDone then
    begin
      document.body.setAttribute('data-program-logic-controls', 'passed');
      document.body.setAttribute('data-program-logic-controls-checks', IntToStr(GChecks));
      Exit;
    end;
    window.setTimeout(@Drive, 20);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-program-logic-controls', 'failed');
      document.body.setAttribute('data-program-logic-controls-error', E.Message);
      {$ifndef NYX_CANVAS_CONTROL_CONNECTORS}
      try
        Retire;
      except
        on LCleanup: Exception do
        begin
          document.body.setAttribute('data-program-logic-controls-error',
            E.Message + '; cleanup: ' + LCleanup.Message);
        end;
      end;
      {$endif}
    end;
  end;
end;
{$endif}

begin
  try
    GExpectedFailures := 0;
    {$ifdef NYX_PROGRAM_LOGIC_COMPANION}
    GDocument := BuildNyxDocument;
    {$ifdef NYX_CONTROL_ROUTES_RUNTIME}VerifyControlCompanion;{$endif}
    {$else}
    {$ifdef NYX_PROGRAM_LOGIC_CANVAS}
    GDocument := ProgramToolCanvasDocument;
    {$else}
    GDocument := ProgramLogicDocument;
    {$endif}{$endif}
    {$ifdef NYX_CANVAS_ROUTE_AUTHORING}
    SetNyxFlowRoutes(GDocument.Find('greet-button'), NewNyxFlowRoutes);
    {$ifdef NYX_CANVAS_CONTROL_CONNECTORS}
    GDocument.Canvases.Define(NyxCanvasRef('Greeting canvas'),
      NewNyxCanvasDefinition
        .WithCamera(NyxCanvasCamera(900, 600, NyxCanvasPoint(0, 0), 1))
        .WithLayer(NyxCanvasLayer('Controls'))
        .WithItem(NyxCanvasItem('Greeting'), NyxReusableRoot('greeting-template'),
          NyxCanvasPlacement(NyxCanvasLayer('Controls'), NyxCanvasBounds(20, 20, 300, 260))));
    {$endif}
    GDocument.Canvases.Define(NyxCanvasRef('Greeting canvas'),
      GDocument.Canvases.Definition(NyxCanvasRef('Greeting canvas'))
        .WithItem(NyxCanvasItem('Second greeting'), NyxReusableRoot('greeting-template'),
          NyxCanvasPlacement(NyxCanvasLayer('Controls'), NyxCanvasBounds(540, 20, 480, 300))));
    {$endif}
    GOtherDocument := ProgramLogicDocument;
    {$ifdef NYX_PIPELINE_NODE_FACES}
    GDocument.Find('program-tools').Part(NyxPart('workspace')).Configure.Height(800).Done;
    {$endif}
    GOtherDocument.Programs.Define(NyxProgramRef('Greeting'), NewNyxProgramDefinition(
      'begin Caption := ''Welcome, '' + Name + ''!'' end.', GreetingProgramContract));
    {$ifndef PAS2JS}
    SetNyxFlowRoutes(GOtherDocument.Find('greet-button'), NewNyxFlowRoutes.WithRoute(
      NyxControlFlowRoutes(GOtherDocument.Find('greet-button')).Route(0).Using(neThreaded)));
    {$endif}
    GBefore := TNyxCodec.Encode(GDocument);
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('main'));
    GOtherHost := TJSHTMLElement(document.createElement('main'));
    document.body.appendChild(GHost);
    document.body.appendChild(GOtherHost);
    GOtherHost.style.setProperty('display', 'none');
    GApp := TNyxBrowserApplication.Create;
    GOther := TNyxBrowserApplication.Create;
    GApp.Run(GDocument, GHost);
    GOther.Run(GOtherDocument, GOtherHost);
    {$else}
    Application.Initialize;
    GApp := TNyxLCLApplication.Create;
    GOther := TNyxLCLApplication.Create;
    GApp.Mount(GDocument);
    GOther.Mount(GOtherDocument);
    {$ifdef NYX_PIPELINE_NODE_FACES}
    GApp.Window.SetBounds(20, 20, 1800, 960);
    {$endif}
    GApp.Window.Show;
    {$endif}
    {$ifndef NYX_PROGRAM_LOGIC_CANVAS}GBindings := GApp.FlowRoutes;{$endif}
    {$ifdef PAS2JS}
    GStarted := TJSDate.now;
    Drive;
    {$else}
    GStarted := GetTickCount64;
    while not GDone do
    begin
      CheckSynchronize;
      Application.ProcessMessages;

      if GetTickCount64 - GStarted > {$ifdef NYX_CANVAS_ROUTE_AUTHORING}120000{$else}45000{$endif} then
      begin
        raise Exception.Create('Program application deadline at stage ' + IntToStr(GStage));
      end;
      Step;
      Sleep(5);
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' actual named program application checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-program-logic-controls', 'failed');
      document.body.setAttribute('data-program-logic-controls-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      Retire;
    end;
  end;
end.
