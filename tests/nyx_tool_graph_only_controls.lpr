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

program nyx_tool_graph_only_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec, nyx.codegen,
  nyx.controls, nyx.root.types, nyx.state, nyx.events, nyx.scheduler, nyx.behavior, nyx.source,
  nyx.canvas.types, nyx.canvas.workspace, nyx.canvas.declarations, nyx.canvas.scene,
  nyx.tool.canvas, nyx.tool.wires, nyx.flow, nyx.flow.types, nyx.flow.declarations,
  nyx.test.graph.only
  {$ifdef NYX_GRAPH_ONLY_COMPANION}, nyx.generated.view{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser, nyx.canvas.browser,
    nyx.render.browser, nyx.tool.canvas.browser
  {$else}, Classes, Interfaces, Forms, Controls, StdCtrls, Graphics, Windows,
    nyx.application.lcl, nyx.canvas.lcl, nyx.render.lcl, nyx.tool.canvas.lcl,
    nyx.studio.projects, nyx.studio.session, nyx.studio.sourcejobs,
    nyx.studio.directories, nyx.studio.outputs, nyx.studio.builds,
    nyx.studio.buildexecutor, nyx.studio.sourceprojection,
    nyx.studio.sourcecompilation, nyx.studio.sourcecompilation.native{$endif};

type
  { Fixture-owned callback invokes the explicitly applied pure graph. It retains
    no document/view; its subscription is cancelled before global test owners die. }
  TRunGraph = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  {$ifndef PAS2JS}
  { Native exception observer outlives all test windows and turns asynchronous
    failures into a bounded test failure instead of an unattended modal dialog. }
  TInputFailure = class
  public
    Error: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;
  {$endif}

var
  GDocument: TNyxDocument;
  GBefore: TNyxText;
  GTools: INyxToolCanvasView;
  GLogic: INyxToolCanvasLogicAuthoring;
  GGraph: INyxToolCanvasGraphConnections;
  GMode: INyxToolCanvasControlConnectors;
  GNodes: INyxToolPipelineNodes;
  GNodeMode: INyxToolCanvasPipelineNodes;
  GWires: INyxToolControlWires;
  GEditing: INyxToolPipelineGraphEditing;
  GRuntime: INyxCanvasRuntimeView;
  GFlows: INyxCanvasFlowEditing;
  GOldCard: INyxCard;
  GRun: INyxEventCallback;
  GRunToken: INyxEventSubscription;
  GOutput: TNyxText;
  GChecks: Integer;
  GStage: Integer;
  GTracedStage: Integer;
  GDone: Boolean;
  GStarted: {$ifdef PAS2JS}Double{$else}QWord{$endif};
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GHost: TJSHTMLElement;
  GCanvas: INyxBrowserCanvasWorkspaceView;
  GFace: INyxBrowserToolCanvasView;
  GNodeFace: INyxBrowserToolPipelineNodes;
  GWireFace: INyxBrowserToolCanvasConnectors;
  {$else}
  GApp: TNyxLCLApplication;
  GCanvas: INyxLCLCanvasWorkspaceView;
  GFace: INyxLCLToolCanvasView;
  GNodeFace: INyxLCLToolPipelineNodes;
  GWireFace: INyxLCLToolCanvasConnectors;
  GFailure: TInputFailure;
  {$ifndef NYX_GRAPH_ONLY_COMPANION}
  GSession: TNyxStudioSession;
  GCommands: TNyxSourceCommands;
  GProfile: TNyxOutputConfiguration;
  GExecutor: TNyxBuildExecutor;
  GDirectories: TNyxStudioDirectories;
  GPairBefore: TNyxText;
  GPairAfter: TNyxText;
  GPersistence: INyxToolCanvasRoutePersistence;
  {$endif}
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Graph-only canvas: ' + AReason);
  end;
  Inc(GChecks);
end;

function ID(const ASuffix: TNyxText): TNyxText;
begin
  Result := GTools.Inspector.Root.ID + ASuffix;
end;

function WireID(const ASuffix: TNyxText): TNyxText;
begin
  Result := ID('-canvas-wires' + ASuffix);
end;

procedure TRunGraph.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  GOutput := RunGraphOnly(GFlows.RuntimeFlows);
  GApp.State.SetValue(NyxTextState('result'), GOutput);
end;

{$ifndef PAS2JS}
procedure TInputFailure.Failed(ASender: TObject; AError: Exception);
begin
  Error := AError.Message;
end;

function ReadText(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, LStream.Size);

    if Length(Result) > 0 then
    begin
      LStream.ReadBuffer(Result[1], Length(Result));
    end;
  finally
    LStream.Free;
  end;
end;

procedure WriteText(const APath, AValue: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if Length(AValue) > 0 then
    begin
      LStream.WriteBuffer(AValue[1], Length(AValue));
    end;
  finally
    LStream.Free;
  end;
end;

{$ifndef NYX_GRAPH_ONLY_COMPANION}
procedure PrepareSource;
var
  LTools: TNyxDataValue;
  LBuild: INyxSourceProjectionBuild;
begin
  Check((ParamCount = 3) and not DirectoryExists(ParamStr(3)),
    'native qualification uses an explicit fresh owned source runtime');
  GDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
    .RunningIn(ParamStr(3)).EnrollingProject(ParamStr(3));
  LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
  GProfile := TNyxOutputConfiguration.Create;
  GProfile.SetField('fpc', LTools.Field('FPC').AsText);
  GProfile.SetField('pas2js', LTools.Field('PAS2JS').AsText);
  GProfile.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
  GExecutor := TNyxBuildExecutor.Create(GDirectories, GProfile.Encode);
  LBuild := GExecutor.ProjectSource(TNyxCodegen.Generate(GDocument),
    NyxPascalUnit('nyx.generated.view'), btNativeLCL, spcChecked);
  Check((LBuild.Projection.State = spsExecuted) and
    (LBuild.Projection.Design = TNyxCodec.Encode(GDocument)),
    'actual native constructor admits the complete empty-scene source baseline');
  GSession := TNyxStudioSession.Create;
  GSession.AdoptProjectedProject(NyxProjectPair(LBuild.Projection.Design,
    TNyxCodegen.Generate(GDocument)), LBuild.Projection);
  GSession.Activate('home');
  GPairBefore := EncodeNyxProject(GSession.ProjectSnapshot);
  GCommands := TNyxSourceCommands.Create(GSession, nil);
  GCommands.UseCompiler(NewNyxNativeSourceCompiler(GDirectories, GProfile.Encode,
    TNyxCompilerLimits.Default));
end;
{$endif}
{$endif}

function Idle: Boolean;
begin
  Result := (GTools.RefreshExecution = nil) or
    not (GTools.RefreshExecution.Status in [nesPending, nesRunning]);

  if Result and (GTools.RefreshExecution <> nil) then
  begin
    if GTools.RefreshExecution.Status <> nesSucceeded then
    begin
      raise Exception.Create('Deferred inspector publication failed: ' + GTools.RefreshExecution.Failure);
    end;
  end;

  if GWireFace.ConnectorExecution <> nil then
  begin
    Result := Result and not (GWireFace.ConnectorExecution.Status in [nesPending, nesRunning]);

    if Result then
    begin
      if GWireFace.ConnectorExecution.Status <> nesSucceeded then
      begin
        raise Exception.Create('Deferred graph presentation failed: ' + GWireFace.ConnectorExecution.Failure);
      end;
    end;
  end;
end;

procedure Change(const ASuffix, AValue: TNyxText);
var
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TControl;{$endif}
begin
  LInput := GFace.Renderer.InputFor(ID(ASuffix));
  {$ifdef PAS2JS}

  if LInput is TJSHTMLSelectElement then
  begin
    TJSHTMLSelectElement(LInput).value := AValue;
    LInput.dispatchEvent(TJSEvent.new('change'));
  end
  else
  begin
    TJSHTMLInputElement(LInput).value := AValue;
    LInput.dispatchEvent(TJSEvent.new('input'));
  end;
  {$else}

  if LInput is TComboBox then
  begin
    Check(TComboBox(LInput).Items.IndexOf(AValue) >= 0, 'operation belongs to the actual choice widget');
    TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(AValue);
    TComboBox(LInput).OnChange(LInput);
  end
  else
  begin
    TCustomEdit(LInput).Text := AValue;
  end;
  {$endif}
end;

{ Host button input, not a call to Inspector.Handle. Native messages preserve
  ordinary default button behavior; browser input remains a separate HTTP gate. }
procedure Press(ARenderer: {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif};
  const AID: TNyxText);
{$ifndef PAS2JS}
var
  LButton: TWinControl;
{$endif}
begin
  {$ifdef PAS2JS}
  ARenderer.ElementFor(AID, niRuntime).click;
  {$else}
  LButton := TWinControl(ARenderer.ControlFor(AID, niRuntime));
  Check(LButton.CanFocus, 'native graph action is focusable');
  LButton.SetFocus;
  Windows.SendMessage(LButton.Handle, WM_KEYDOWN, VK_SPACE, 1);
  Windows.SendMessage(LButton.Handle, WM_KEYUP, VK_SPACE, 1);
  {$endif}
end;

procedure CaptureNodes;
var
  LNodes: INyxToolPipelineNodes;
  LEditing: INyxToolPipelineGraphEditing;
begin
  GWires := GMode.ControlWires;
  Check(Supports(GWires, INyxToolPipelineNodes, LNodes) and
    Supports(GWires, INyxToolPipelineGraphEditing, LEditing), 'graph-only controller exposes specialized node interfaces');
  GNodes := LNodes;
  GEditing := LEditing;
end;

function Port(const AStep: TNyxText; ADirection: TNyxFlowDirection;
  AType: TNyxFlowType): Integer;
var
  LIndex: Integer;
  LPort: TNyxToolWirePort;
begin
  for LIndex := 0 to GWires.PortCount - 1 do
  begin
    LPort := GWires.Port(LIndex);

    if (LPort.Endpoint.Step.Name = AStep) and (LPort.Direction = ADirection) and
      (LPort.ValueType = AType) then
    begin
      Exit(LIndex);
    end;
  end;
  raise Exception.Create('Expected typed graph port is missing');
end;

procedure Step;
{$ifndef NYX_GRAPH_ONLY_COMPANION}
var
  LFailed: Boolean;
  LEvent: TNyxEventInfo;
  LExpected: TNyxDocument;
  LIndex: Integer;
  LOldPort: TNyxControlRef;
  {$ifndef PAS2JS}LPair: TNyxProjectPair;{$endif}
{$endif}
begin

  if not Idle then
  begin
    Exit;
  end;
  {$ifndef PAS2JS}

  if GFailure.Error <> '' then
  begin
    raise Exception.Create(GFailure.Error);
  end;

  if GTracedStage <> GStage then
  begin
    GTracedStage := GStage;
    WriteLn('Graph-only stage ', GStage);
    Flush(Output);
  end;
  {$endif}
  {$ifdef NYX_GRAPH_ONLY_COMPANION}

  if GStage = 0 then
  begin
    Check((GRuntime.Scene.Snapshot.Count = 0) and (GCanvas.MountedCount = 0),
      'exact compiled graph application mounts no dummy scene content');
    Press(GApp.View, 'run-graph');
    Inc(GStage);
    Exit;
  end;
  Check(GApp.State.GetValue(NyxTextState('result')) = GOutput,
    'compiled graph produces the actual application state');
  Check((Pos(#10, GOutput) > 0) and
    (TNyxDataValue.ParseJSON(GOutput).ToJSON = TNyxDataValue.ParseJSON(GraphOnlyInput).ToJSON),
    'compiled pipeline preserves the exact JSON meaning');
  {$ifdef PAS2JS}
  Check(GApp.View.ElementFor('result-label').textContent = GOutput,
    'compiled graph updates the actual browser label');
  {$else}
  Check(TLabel(GApp.View.ControlFor('result-label')).Caption = GOutput,
    'compiled graph updates the actual native label');
  {$endif}
  Check(TNyxCodec.Encode(GDocument) = GBefore, 'compiled graph execution preserves every authored byte');
  GDone := True;
  Exit;
  {$else}
  case GStage of
    0:
      begin
        Check((GRuntime.Scene.Snapshot.Count = 0) and (GCanvas.MountedCount = 0) and
          (GFlows.RuntimeFlows.Count = 0), 'starting canvas has no items, mounted controls or graphs');
        GLogic.ShowLogic;
        GTools.Refresh;
      end;
    1:
      begin
        Change('-logic-graph-name', GraphOnlyName);
        Press(GFace.Renderer, ID('-logic-create'));
      end;
    2:
      begin
        Check(GLogic.HasPendingLogic and not GFlows.RuntimeFlows.Contains(NyxFlowRef(GraphOnlyName)),
          'new graph is staged without first publishing it');
        Press(GFace.Renderer, ID('-logic-show-graph'));
      end;
    3:
      begin
        CaptureNodes;
        Check(not GWires.Item.Defined and (GWires.ControlCount = 0) and
          GNodeMode.PipelineNodesVisible and (GNodes.NodeCount = 1),
          'staged graph appears as a node before first Apply without an item owner');
        Check(GRuntime.Scene.Snapshot.Count = 0, 'showing nodes creates no scene placeholder');
        GOldCard := GNodes.BuildNode(0);
        Press(GWireFace.PipelineConnectorRenderer, GNodes.ModeControl.ID);
      end;
    4:
      begin
        Check(not GNodeMode.PipelineNodesVisible, 'graph-only nodes can be hidden independently');
        Press(GWireFace.PipelineConnectorRenderer, GNodes.ModeControl.ID);
      end;
    5:
      begin
        Check(GNodeMode.PipelineNodesVisible, 'same proposal restores graph-only node cards');
        LOldPort := GWires.PortControl(0);
        GGraph.ShowGraphConnections(NyxFlowRef(GraphOnlyName));
        { The old faces remain physically mounted until publication. Such input
          must be ignored before consulting their now-retired controller. }
        Press(GNodeFace.PipelineNodeRenderer(NyxFlowStep('parse')), LOldPort.ID);
        GTools.Refresh;
        CaptureNodes;
        LEvent := Default(TNyxEventInfo);
        LEvent.Trigger := ntClick;
        Check(not GWires.Handle(GOldCard.Node.Find(GWires.PortControl(0).ID), LEvent),
          'old controller card is rejected even when local epochs and control IDs match');
        GLogic.ShowLogic;
        GTools.Refresh;
      end;
    6:
      begin
        Change('-logic-operation', 'json-format');
        Change('-logic-step-name', 'format');
        Press(GFace.Renderer, ID('-logic-add'));
      end;
    7: Press(GFace.Renderer, ID('-logic-show-graph'));
    8:
      begin
        CaptureNodes;
        Check((GNodes.NodeCount = 2) and (GNodes.NodeWireCount = 0) and
          (GFlows.RuntimeFlows.Count = 0), 'both unpublished operation cards share the same proposal');
        Press(GNodeFace.PipelineNodeRenderer(NyxFlowStep('parse')),
          GWires.PortControl(Port('parse', nfdOutput, nftJSON)).ID);
      end;
    9: Press(GNodeFace.PipelineNodeRenderer(NyxFlowStep('format')),
      GWires.PortControl(Port('format', nfdInput, nftJSON)).ID);
    10: Press(GWireFace.PipelineConnectorRenderer, WireID('-graph-connect'));
    11:
      begin
        Check((GNodes.NodeWireCount = 1) and GLogic.HasPendingLogic and
          (GFlows.RuntimeFlows.Count = 0) and (GRuntime.Scene.Snapshot.Count = 0),
          'actual port actions wire a still-unpublished graph on an empty canvas');
        Change('-logic-step-name', 'unfinished');
        Press(GFace.Renderer, ID('-logic-show-graph'));
      end;
    12:
      begin
        CaptureNodes;
        LFailed := False;
        try
          GEditing.ApplyGraph;
        except
          on E: ENyxFlow do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and (GFlows.RuntimeFlows.Count = 0) and
          (GFace.Renderer.Root.Find(ID('-logic-step-name')).Prop('value') = 'unfinished'),
          'graph-only Apply preserves unfinished fields and refuses all publication');
        Change('-logic-step-name', 'step');
        Press(GWireFace.PipelineConnectorRenderer, WireID('-graph-apply'));
      end;
    13:
      begin
        Check(not GLogic.HasPendingLogic and GFlows.RuntimeFlows.Contains(NyxFlowRef(GraphOnlyName)) and
          (GRuntime.Scene.Snapshot.Count = 0) and (GCanvas.MountedCount = 0),
          'prepared first Apply publishes graph definitions without constructing any item');
        Press(GApp.View, 'run-graph');
      end;
    14:
      begin
        Check((Pos(#10, GOutput) > 0) and
          (TNyxDataValue.ParseJSON(GOutput).ToJSON = TNyxDataValue.ParseJSON(GraphOnlyInput).ToJSON),
          'actual run action executes JSON parse and format operations');
        {$ifdef PAS2JS}
        Check(GApp.View.ElementFor('result-label').textContent = GOutput,
          'graph-only result reaches the actual browser label');
        {$else}
        Check(TLabel(GApp.View.ControlFor('result-label')).Caption = GOutput,
          'graph-only result reaches the actual native label');
        {$endif}
        Check(TNyxCodec.Encode(GDocument) = GBefore, 'runtime operations preserve complete authored defaults/content');
        {$ifndef PAS2JS}
        Check(Supports(GTools.Inspector, INyxToolCanvasRoutePersistence, GPersistence),
          'graph-only inspector supports ordinary paired project adoption');
        GPersistence.ConfigureAdoption(NyxControl(GDocument.Find('data-tools').Part(NyxPart('workspace')).ID),
          GCommands.CanvasAdoptionHost(GSession.CommandContext, NyxControl('home')), nil);
        GTools.Refresh;
        Press(GFace.Renderer, ID('-save-connections'));
        {$endif}
      end;
    15:
      begin
        {$ifndef PAS2JS}

        if GCommands.Busy then
        begin
          Exit;
        end;
        Check((GCommands.State = nssApplied) and not GPersistence.AdoptionPending,
          'ordinary source queue adopts the graph-only snapshot: ' + GCommands.Message);
        LExpected := GDocument.Clone;
        try
          for LIndex := 0 to GFlows.RuntimeFlows.Count - 1 do
          begin
            LExpected.Flows.Define(GFlows.RuntimeFlows.Reference(LIndex),
              GFlows.RuntimeFlows.Definition(GFlows.RuntimeFlows.Reference(LIndex)));
          end;
          Check(GSession.Save = TNyxCodec.Encode(LExpected),
            'complete saved model retains empty scene, unrelated content and exact graph');
        finally
          LExpected.Free;
        end;
        GPairAfter := EncodeNyxProject(GSession.ProjectSnapshot);
        GSession.Undo;
        Check(EncodeNyxProject(GSession.ProjectSnapshot) = GPairBefore, 'one Undo restores the exact pre-graph design/Pascal pair');
        GSession.Redo;
        Check(EncodeNyxProject(GSession.ProjectSnapshot) = GPairAfter, 'one Redo restores the exact graph-only pair');
        LPair := GSession.ProjectSnapshot;
        WriteText(GDirectories.RuntimeRoot + 'nyx.generated.view.pas', LPair.Source);
        WriteText(GDirectories.RuntimeRoot + 'accepted.nyx', LPair.Design);
        {$endif}
        Change('-logic-graph-name', 'Another tool');
        Press(GFace.Renderer, ID('-logic-create'));
      end;
    16:
      begin
        GGraph.ShowGraphConnections(NyxFlowRef('Another tool'));
        GTools.Refresh;
        CaptureNodes;
        LFailed := False;
        try
          GGraph.ShowGraphConnections(NyxFlowRef(GraphOnlyName));
        except
          on E: ENyxCanvasScene do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and (GMode.ControlWires = GWires) and (GNodes.NodeCount = 1) and GLogic.HasPendingLogic,
          'switching away refuses losing an unpublished second graph and retains its controller');
        GLogic.ReloadLogic(True);
        GGraph.ShowGraphConnections(NyxFlowRef(GraphOnlyName));
        GTools.Refresh;
        CaptureNodes;
        Press(GWireFace.PipelineConnectorRenderer, GNodes.ModeControl.ID);
      end;
    17:
      begin
        Check(not GNodeMode.PipelineNodesVisible, 'explicit node hiding survives current graph selection');
        GMode.ShowControlConnectors(False);
        GTools.Refresh;
        GMode.ShowControlConnectors(True);
        GTools.Refresh;
      end;
    18:
      begin
        Check(not GNodeMode.PipelineNodesVisible and (GRuntime.Scene.Snapshot.Count = 0),
          'whole presentation hide/show retains node visibility without fabricating an item');
        Press(GWireFace.PipelineConnectorRenderer, GNodes.ModeControl.ID);
      end;
    19:
      begin
        Check(GNodeMode.PipelineNodesVisible and (GNodes.NodeCount = 2), 'graph cards reopen after scoped hiding');
        GDone := True;
      end;
  end;
  Inc(GStage);
  {$endif}
end;

procedure Retire;
begin

  if GRunToken <> nil then
  begin
    GRunToken.Cancel;
  end;
  GRunToken := nil;
  GRun := nil;
  GOldCard := nil;
  GEditing := nil;
  GNodes := nil;
  GWires := nil;
  GNodeMode := nil;
  GNodeFace := nil;
  GWireFace := nil;
  GFace := nil;
  GMode := nil;
  GGraph := nil;
  GLogic := nil;
  GTools := nil;
  GFlows := nil;
  GRuntime := nil;
  GCanvas := nil;
  FreeAndNil(GApp);
  FreeAndNil(GDocument);
  {$ifndef PAS2JS}
  {$ifndef NYX_GRAPH_ONLY_COMPANION}
  GPersistence := nil;
  FreeAndNil(GCommands);
  FreeAndNil(GSession);
  FreeAndNil(GExecutor);
  FreeAndNil(GProfile);
  {$endif}
  Application.OnException := nil;
  FreeAndNil(GFailure);
  {$endif}
end;

procedure Start;
var
  LRuntime: INyxCanvasRuntimeView;
  LFlows: INyxCanvasFlowEditing;
  LLogic: INyxToolCanvasLogicAuthoring;
  LGraph: INyxToolCanvasGraphConnections;
  LMode: INyxToolCanvasControlConnectors;
  LNodeMode: INyxToolCanvasPipelineNodes;
  {$ifdef PAS2JS}
  LFace: INyxBrowserToolCanvasView;
  LNodes: INyxBrowserToolPipelineNodes;
  LWires: INyxBrowserToolCanvasConnectors;
  {$else}
  LFace: INyxLCLToolCanvasView;
  LNodes: INyxLCLToolPipelineNodes;
  LWires: INyxLCLToolCanvasConnectors;
  {$endif}
begin
  GTools := GApp.View.ToolCanvasView('data-tools');
  GCanvas := GApp.View.CanvasView(GDocument.Find('data-tools').Part(NyxPart('workspace')).ID) as
    {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};
  Check(Supports(GCanvas, INyxCanvasRuntimeView, LRuntime) and
    Supports(GCanvas, INyxCanvasFlowEditing, LFlows), 'empty actual canvas exposes the runtime graph owner');
  GRuntime := LRuntime;
  GFlows := LFlows;
  Check(Supports(GTools.Inspector, INyxToolCanvasLogicAuthoring, LLogic) and
    Supports(GTools.Inspector, INyxToolCanvasGraphConnections, LGraph) and
    Supports(GTools.Inspector, INyxToolCanvasControlConnectors, LMode), 'public graph-only Inspector interfaces exist');
  GLogic := LLogic;
  GGraph := LGraph;
  GMode := LMode;
  Check(Supports(GTools, INyxToolCanvasPipelineNodes, LNodeMode) and
    Supports(GTools, {$ifdef PAS2JS}INyxBrowserToolCanvasView{$else}INyxLCLToolCanvasView{$endif}, LFace) and
    Supports(GTools, {$ifdef PAS2JS}INyxBrowserToolPipelineNodes{$else}INyxLCLToolPipelineNodes{$endif}, LNodes) and
    Supports(GTools, {$ifdef PAS2JS}INyxBrowserToolCanvasConnectors{$else}INyxLCLToolCanvasConnectors{$endif}, LWires),
    'target supplies the existing public node presentation');
  GNodeMode := LNodeMode;
  GFace := LFace;
  GNodeFace := LNodes;
  GWireFace := LWires;
  GRun := TRunGraph.Create;
  GRunToken := GApp.View.Events.On(NyxControlEvents('run-graph', niRuntime), ntClick).Subscribe(GRun);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if TJSDate.now - GStarted > 300000 then
    begin
      raise Exception.Create('Graph-only browser input deadline');
    end;
    Step;

    if GDone then
    begin
      document.body.setAttribute('data-graph-only', 'passed');
      document.body.setAttribute('data-graph-only-checks', IntToStr(GChecks));
      Retire;
      Exit;
    end;
    window.setTimeout(@Drive, 25);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-graph-only', 'failed');
      document.body.setAttribute('data-graph-only-error', E.Message);
      Retire;
    end;
  end;
end;
{$endif}

{$ifndef PAS2JS}
var
  LTurn: Integer;
  {$ifndef NYX_GRAPH_ONLY_COMPANION}
  LBitmap: Graphics.TBitmap;
  LPNG: TPortableNetworkGraphic;
  {$endif}
{$endif}
begin
  try
    {$ifdef NYX_GRAPH_ONLY_COMPANION}
    GTracedStage := -1;
    GDocument := BuildNyxDocument;
    {$ifndef PAS2JS}
    Check(TNyxCodec.Encode(GDocument) = ReadText(ParamStr(1)), 'exact compiled constructor retains the whole accepted model');
    {$endif}
    {$else}
    GDocument := GraphOnlyDocument;
    GTracedStage := -1;
    {$ifndef PAS2JS}
    PrepareSource;
    {$endif}
    {$endif}
    GBefore := TNyxCodec.Encode(GDocument);
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GHost);
    GApp := TNyxBrowserApplication.Create;
    GApp.Run(GDocument, GHost);
    GStarted := TJSDate.now;
    Start;
    Drive;
    {$else}
    Application.Initialize;
    GFailure := TInputFailure.Create;
    Application.OnException := GFailure.Failed;
    GApp := TNyxLCLApplication.Create;
    GApp.Mount(GDocument);
    GApp.Window.SetBounds(0, 0, 1400, 1000);
    GApp.Window.Show;
    GStarted := GetTickCount64;
    Start;
    while not GDone do
    begin
      CheckSynchronize;
      Application.ProcessMessages;

      if GetTickCount64 - GStarted > 300000 then
      begin
        raise Exception.Create('Graph-only native deadline at stage ' + IntToStr(GStage));
      end;
      Step;
      Sleep(10);
    end;
    {$ifndef NYX_GRAPH_ONLY_COMPANION}
    LBitmap := GApp.Window.GetFormImage;
    LPNG := TPortableNetworkGraphic.Create;
    try
      LPNG.Assign(LBitmap);
      LPNG.SaveToFile(GDirectories.RuntimeRoot + 'canvas.png');
    finally
      LPNG.Free;
      LBitmap.Free;
    end;
    {$endif}
    Retire;
    for LTurn := 1 to 5 do
    begin
      CheckSynchronize;
      Application.ProcessMessages;
    end;
    WriteLn('PASS ', GChecks, ' actual graph-only canvas checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-graph-only', 'failed');
      document.body.setAttribute('data-graph-only-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      Retire;
    end;
  end;
end.
