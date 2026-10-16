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

type
  { Independent input witness retains only copied trigger text. Its subscriptions
    belong to this fixture and are cancelled before borrowed faces retire. }
  TConnectorTrace = class(TNyxEventCallback)
  public
    Trail: TNyxText;
    SawUp: Boolean;
    LossAfterUp: Boolean;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

var
  GTools: INyxToolCanvasView;
  GWireMode: INyxToolCanvasControlConnectors;
  GWires: INyxToolControlWires;
  GWireFace: {$ifdef PAS2JS}INyxBrowserToolCanvasConnectors{$else}INyxLCLToolCanvasConnectors{$endif};
  GToolFace: {$ifdef PAS2JS}INyxBrowserToolCanvasView{$else}INyxLCLToolCanvasView{$endif};
  GPanel: INyxToolCanvasConnectorPresentation;
  GGeometry: INyxCanvasControlGeometry;
  GValuePort: Integer;
  GCaptionPort: Integer;
  GNamePort: Integer;
  GOutputPort: Integer;
  GInputRequest: Integer;
  GPanelDragRequested: Boolean;
  GPanelDragTop: Double;
  GControlBefore: TNyxCanvasBounds;
  GCanvasInput: {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif};
  GTrace: TConnectorTrace;
  GTraceLease: INyxEventCallback;
  GTraceSubscriptions: array of INyxEventSubscription;

{$ifdef NYX_PIPELINE_NODE_FACES}
procedure RetirePipelineNodes; forward;
{$endif}

procedure TConnectorTrace.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Trail := Trail + ' ' + TNyxText(IntToStr(Ord(AEvent.Trigger)));

  if AEvent.Trigger = ntPointerUp then
  begin
    SawUp := True;
  end;

  if AEvent.Trigger = ntPointerCaptureLost then
  begin
    LossAfterUp := SawUp;
  end;

  if AEvent.HasPointer then
  begin
    Trail := Trail + '(' + TNyxText(IntToStr(Round(AEvent.Pointer.X))) + ',' +
      TNyxText(IntToStr(Round(AEvent.Pointer.Y))) + ')';
  end;
end;

procedure RetireRouteAuthoring;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(GTraceSubscriptions) do
  begin
    GTraceSubscriptions[LIndex].Cancel;
  end;
  GTraceSubscriptions := nil;
  GTrace := nil;
  GTraceLease := nil;
  GWires := nil;
  GWireMode := nil;
  GWireFace := nil;
  GToolFace := nil;
  GPanel := nil;
  GGeometry := nil;
  GTools := nil;
  {$ifdef NYX_PIPELINE_NODE_FACES}
  RetirePipelineNodes;
  {$endif}
end;

function WireID(const ASuffix: TNyxText): TNyxText;
begin
  Result := GTools.Inspector.Root.ID + '-canvas-wires' + ASuffix;
end;

function WireRenderer(AIndex: Integer): {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif};
var
  LPort: TNyxToolWirePort;
begin
  LPort := GWires.Port(AIndex);

  if LPort.Kind = ntwpkPipeline then
  begin
    Exit(GWireFace.PipelineConnectorRenderer);
  end;
  Result := GWireFace.ControlConnectorRenderer(LPort.ControlPort.Control);
end;

function ConnectorIdle: Boolean;
begin
  Result := True;

  if GTools = nil then
  begin
    Exit;
  end;

  if GTools.RefreshExecution <> nil then
  begin

    if GTools.RefreshExecution.Status = nesFailed then
    begin
      raise Exception.Create('Tool Canvas refresh: ' + GTools.RefreshExecution.Failure);
    end;
    Result := Result and not (GTools.RefreshExecution.Status in [nesPending, nesRunning]);
  end;

  if GWireFace.ConnectorExecution <> nil then
  begin

    if GWireFace.ConnectorExecution.Status = nesFailed then
    begin
      raise Exception.Create('Connector refresh: ' + GWireFace.ConnectorExecution.Failure);
    end;
    Result := Result and not (GWireFace.ConnectorExecution.Status in [nesPending, nesRunning]);
  end;
end;

{ This fixture requests host input only. Browser acknowledgements come from an
  independent Pascal Chromium-pipe driver; native controls receive ordinary Win32
  key/mouse messages. Neither path dispatches a portable Nyx event directly. }
procedure KeyButton(ARenderer: {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif};
  const AID: TNyxText; const AKey: TNyxText = 'space');
var
  {$ifdef PAS2JS}
  LButton: TJSHTMLElement;
  {$else}
  LButton: TWinControl;
  LKey: Integer;
  LIndex: Integer;
  LCount: Integer;
  {$endif}
begin
  {$ifdef PAS2JS}
  LButton := ARenderer.ElementFor(AID, niRuntime);
  LButton.scrollIntoView;
  LButton.focus;
  Inc(GInputRequest);
  document.body.setAttribute('data-wire-kind', AKey);
  document.body.setAttribute('data-wire-request', IntToStr(GInputRequest));
  {$else}
  LButton := TWinControl(ARenderer.ControlFor(AID, niRuntime));
  Check(LButton.CanFocus, 'native connector is a focusable ordinary Nyx button');
  LButton.SetFocus;
  LKey := VK_SPACE;

  if (AKey = 'right') or (AKey = 'right-twice') then
  begin
    LKey := VK_RIGHT;
  end
  else if AKey = 'home' then
  begin
    LKey := VK_HOME;
  end;
  LCount := 1;

  if AKey = 'right-twice' then
  begin
    LCount := 2;
  end;
  for LIndex := 1 to LCount do
  begin
    Windows.SendMessage(LButton.Handle, WM_KEYDOWN, LKey, 1);
    Windows.SendMessage(LButton.Handle, WM_KEYUP, LKey, 1);
  end;
  {$endif}
end;

function InputFinished: Boolean;
begin
  {$ifdef PAS2JS}
  Result := (GInputRequest = 0) or
    (document.body.getAttribute('data-wire-observed') = IntToStr(GInputRequest));
  {$else}
  Result := True;
  {$endif}
end;

procedure ConfirmRemoval;
var
  {$ifdef PAS2JS}
  LInput: TJSHTMLElement;
  {$else}
  LInput: TWinControl;
  {$endif}
begin
  {$ifdef PAS2JS}
  LInput := GWireFace.PipelineConnectorRenderer.InputFor(WireID('-confirm'), niRuntime);
  LInput.scrollIntoView;
  LInput.focus;
  Inc(GInputRequest);
  document.body.setAttribute('data-wire-kind', 'space');
  document.body.setAttribute('data-wire-request', IntToStr(GInputRequest));
  {$else}
  LInput := TWinControl(GWireFace.PipelineConnectorRenderer.InputFor(WireID('-confirm'), niRuntime));
  Check(LInput.CanFocus, 'removal acknowledgement is an ordinary focusable native input');
  LInput.SetFocus;
  Windows.SendMessage(LInput.Handle, WM_KEYDOWN, VK_SPACE, 1);
  Windows.SendMessage(LInput.Handle, WM_KEYUP, VK_SPACE, 1);
  {$endif}
end;

procedure DragPanel;
var
  {$ifdef PAS2JS}
  LHeader: TJSHTMLElement;
  {$else}
  LHeader: TWinControl;
  LX: Integer;
  LY: Integer;
  {$endif}
begin
  GPanelDragTop := GPanel.PipelineBounds.Top;
  {$ifdef PAS2JS}
  LHeader := GWireFace.PipelineConnectorRenderer.ElementFor(WireID('-title'), niRuntime);
  LHeader.setAttribute('data-wire-panel', 'true');
  Inc(GInputRequest);
  document.body.setAttribute('data-wire-kind', 'touch-panel');
  document.body.setAttribute('data-wire-request', IntToStr(GInputRequest));
  {$else}
  LHeader := TWinControl(GWireFace.PipelineConnectorRenderer.ControlFor(WireID('-title'), niRuntime));
  LX := LHeader.Width div 2;
  LY := LHeader.Height div 2;
  Windows.SendMessage(LHeader.Handle, WM_LBUTTONDOWN, MK_LBUTTON, LPARAM(LY shl 16 or LX));
  Windows.SendMessage(LHeader.Handle, WM_MOUSEMOVE, MK_LBUTTON, LPARAM((LY + 32) shl 16 or LX));
  Windows.SendMessage(LHeader.Handle, WM_LBUTTONUP, 0, LPARAM((LY + 32) shl 16 or LX));
  {$endif}
end;

procedure DragPorts(ASource, ATarget: Integer);
const
  CTriggers: array[0..4] of TNyxTrigger =
    (ntPointerDown, ntPointerMove, ntPointerUp, ntPointerCancel, ntPointerCaptureLost);
var
  LIndex: Integer;
  {$ifdef PAS2JS}
  LSource: TJSHTMLElement;
  LTarget: TJSHTMLElement;
  {$else}
  LSource: TWinControl;
  LTarget: TWinControl;
  LPoint: TPoint;
  LStart: TPoint;
  LPosition: LPARAM;
  {$endif}
begin
  GTrace := TConnectorTrace.Create;
  GTraceLease := GTrace;
  SetLength(GTraceSubscriptions, Length(CTriggers));
  for LIndex := 0 to High(CTriggers) do
  begin
    GTraceSubscriptions[LIndex] := WireRenderer(ASource).Events.On(
      NyxControlEvents(GWires.PortControl(ASource).ID, niRuntime),
      CTriggers[LIndex]).Subscribe(GTraceLease);
  end;
  {$ifdef PAS2JS}
  LSource := WireRenderer(ASource).ElementFor(GWires.PortControl(ASource).ID, niRuntime);
  LTarget := WireRenderer(ATarget).ElementFor(GWires.PortControl(ATarget).ID, niRuntime);
  LSource.setAttribute('data-wire-source', 'true');
  LTarget.setAttribute('data-wire-target', 'true');
  Inc(GInputRequest);
  document.body.setAttribute('data-wire-kind', 'touch-wire');
  document.body.setAttribute('data-wire-request', IntToStr(GInputRequest));
  {$else}
  LSource := TWinControl(WireRenderer(ASource).ControlFor(GWires.PortControl(ASource).ID, niRuntime));
  LTarget := TWinControl(WireRenderer(ATarget).ControlFor(GWires.PortControl(ATarget).ID, niRuntime));
  LStart := Types.Point(LSource.Width div 2, LSource.Height div 2);
  LPoint := LSource.ScreenToClient(LTarget.ClientToScreen(Types.Point(LTarget.Width div 2, LTarget.Height div 2)));
  Windows.SendMessage(LSource.Handle, WM_LBUTTONDOWN, MK_LBUTTON,
    LPARAM((LStart.Y and $FFFF) shl 16 or (LStart.X and $FFFF)));
  LPosition := LPARAM((LPoint.Y and $FFFF) shl 16 or (LPoint.X and $FFFF));
  Windows.SendMessage(LSource.Handle, WM_MOUSEMOVE, MK_LBUTTON, LPosition);
  Windows.SendMessage(LSource.Handle, WM_LBUTTONUP, 0, LPosition);
  {$endif}
end;

procedure DiscoverPorts;
var
  LIndex: Integer;
  LPort: TNyxToolWirePort;
begin
  GValuePort := -1;
  GNamePort := -1;
  GCaptionPort := -1;
  GOutputPort := -1;
  for LIndex := 0 to GWires.PortCount - 1 do
  begin
    LPort := GWires.Port(LIndex);

    if LPort.Kind = ntwpkControl then
    begin

      if (LPort.ControlPort.Control.ID = 'name-input') and
        (LPort.ControlPort.Role = ntprEventValue) and
        (LPort.ControlPort.Trigger = ntChange) then
      begin
        GValuePort := LIndex;
      end;

      if (LPort.ControlPort.Control.ID = 'caption-label') and
        (LPort.ControlPort.Role = ntprBoundState) then
      begin
        GCaptionPort := LIndex;
      end;
    end
    else if LPort.Endpoint.Same(ProgramLogicEndpoint('Name')) then
    begin
      GNamePort := LIndex;
    end
    else if LPort.Endpoint.Same(ProgramLogicEndpoint('Caption')) then
    begin
      GOutputPort := LIndex;
    end;
  end;
  Check((GValuePort >= 0) and (GNamePort >= 0) and
    (GCaptionPort >= 0) and (GOutputPort >= 0),
    'actual mounted faces expose exact control/event and Pascal script endpoints');
end;

{$ifdef NYX_PIPELINE_NODE_FACES}
{$I nyx.test.canvas.pipeline.nodes.inc}
{$endif}

procedure Step;
var
  LMode: INyxToolCanvasControlConnectors;
  LFace: {$ifdef PAS2JS}INyxBrowserToolCanvasConnectors{$else}INyxLCLToolCanvasConnectors{$endif};
  LTools: {$ifdef PAS2JS}INyxBrowserToolCanvasView{$else}INyxLCLToolCanvasView{$endif};
  LPanel: INyxToolCanvasConnectorPresentation;
  LGeometry: INyxCanvasControlGeometry;
  LRuntime: INyxCanvasRuntimeView;
  LFlows: INyxCanvasFlowEditing;
  LDefinitions: INyxFlowDeclarations;
  LDisclosure: INyxDisclosure;
  LFailed: Boolean;
  LRestricted: INyxToolControlWires;
  LCount: Integer;
  LPort: TNyxToolWirePort;
  LPortIndex: Integer;
  LInternalInput: Boolean;
  LWidth: Integer;
  LHeight: Integer;
  LBounds: TNyxCanvasBounds;
  {$ifndef PAS2JS}
  LInput: TEdit;
  LCapture: TWinControl;
  LIndex: Integer;
  {$endif}
begin

  if not InputFinished or not ConnectorIdle then
  begin
    Exit;
  end;
  {$ifdef NYX_PIPELINE_NODE_FACES}

  if GStage >= 21 then
  begin
    StepPipelineNodes;
    Exit;
  end;
  {$endif}
  case GStage of
    0:
      begin

        if not SetupCanvas then
        begin
          Exit;
        end;
        LWidth := GCanvasView.Surface.{$ifdef PAS2JS}clientWidth{$else}ClientWidth{$endif};
        LHeight := GCanvasView.Surface.{$ifdef PAS2JS}clientHeight{$else}ClientHeight{$endif};
        GCanvasView.SetCamera(NyxCanvasCamera(LWidth, LHeight,
          NyxCanvasPoint(8 + LWidth / 2, 8 + LHeight / 2), 1));
        GTools := GApp.View.ToolCanvasView('program-tools');
        GTools.Inspector.SelectControl(NyxCanvasItem('Greeting'), NyxControl('name-input'));
        GTools.Refresh;
        Check(Supports(GTools.Inspector, INyxToolCanvasControlConnectors, LMode),
          'ordinary Tool Canvas exposes optional control connector enablement');
        GWireMode := LMode;
        Check(Supports(GTools, {$ifdef PAS2JS}INyxBrowserToolCanvasConnectors{$else}INyxLCLToolCanvasConnectors{$endif}, LFace),
          'actual adapter exposes borrowed mounted connector renderers');
        GWireFace := LFace;
        Check(Supports(GTools, {$ifdef PAS2JS}INyxBrowserToolCanvasView{$else}INyxLCLToolCanvasView{$endif}, LTools),
          'ordinary inspector remains a public Nyx renderer');
        GToolFace := LTools;
        Check(Supports(GTools, INyxToolCanvasConnectorPresentation, LPanel),
          'pipeline placement uses a portable viewport contract');
        GPanel := LPanel;
        Check(Supports(GCanvasView, INyxCanvasControlGeometry, LGeometry),
          'actual canvas supplies borrowed-control geometry');
        GGeometry := LGeometry;
      end;
    1:
      begin
        KeyButton(GToolFace.Renderer, GTools.Inspector.Root.ID + '-canvas-connectors');
      end;
    2:
      begin
        Check(GWireMode.ControlConnectorsVisible, 'actual keyboard enables canvas connector chrome / ' +
          GToolFace.Renderer.LastBindingError + ' / ' + GTools.Inspector.Root.ID);
        GWires := GWireMode.ControlWires;
        DiscoverPorts;
        Check((GWires.WireCount = 0) and (GBindings.Count = 0),
          'showing ports grants no graph execution');
        GCanvasInput := MainView.InputFor('name-input');
        SubscribeCallbacks;
        KeyButton(WireRenderer(GValuePort), GWires.PortControl(GValuePort).ID);
      end;
    3:
      begin
        KeyButton(WireRenderer(GNamePort), GWires.PortControl(GNamePort).ID);
      end;
    4:
      begin
        KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-connect'));
      end;
    5:
      begin
        Check((GWires.WireCount = 1) and GWires.Dirty and (GBindings.Count = 0),
          'keyboard staging retains an unfinished inert control-to-script wire');
        DragPorts(GOutputPort, GCaptionPort);
      end;
    6:
      begin
        {$ifndef PAS2JS}
        Check(GTrace.LossAfterUp,
          'ordinary Win32 button-up reaches the port before its automatic capture-loss notification');
        {$endif}
        Check((GWires.WireCount = 2) and (GBindings.Count = 0),
          'real pointer capture stages script-to-bound-label without executing / count ' +
          TNyxText(IntToStr(GWires.WireCount)) + ' / triggers' + GTrace.Trail);
        KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-apply'));
      end;
    7:
      begin
        Supports(GCanvasView, INyxCanvasRuntimeView, LRuntime);
        GBindings := LRuntime.BehaviorFor(NyxCanvasItem('Greeting')).Routes;
        Check((GBindings.Count = 1) and not GWires.Dirty,
          'actual Apply publishes the complete event/script/control route');
        Check(MainView.InputFor('name-input') = GCanvasInput,
          'route publication retains the original embedded input');
        {$ifdef PAS2JS}
        GCanvasInput.setAttribute('data-wire-editor', 'true');
        Inc(GInputRequest);
        document.body.setAttribute('data-wire-kind', 'edit-name');
        document.body.setAttribute('data-wire-request', IntToStr(GInputRequest));
        {$else}
        LInput := TEdit(GCanvasInput);
        LInput.SetFocus;
        LInput.SelectAll;
        for LIndex := 1 to Length('Grace') do
        begin
          Windows.SendMessage(LInput.Handle, WM_CHAR, Ord('Grace'[LIndex]), 1);
        end;
        {$endif}
      end;
    8:
      begin

        if not Idle(GBindings) then
        begin
          Exit;
        end;
        Check(Caption = 'Hello, Grace!', 'normal editor input executes the actual Pascal greeting and bound output');
        Check((GCallbackCount >= 2) and (GCallbackCount mod 2 = 0),
          'both ordinary callbacks remain registered beside graph execution');
        Check(MainView.InputFor('name-input') = GCanvasInput,
          'typing and graph completion retain the actual embedded editor');
        Check(GGeometry.TryControlBounds(NyxCanvasItem('Greeting'), NyxControl('name-input'), GControlBefore),
          'input geometry resolves from the mounted adapter');
        GCanvasView.PanBy(12, 8);
      end;
    9:
      begin
        Check(GGeometry.TryControlBounds(NyxCanvasItem('Greeting'), NyxControl('name-input'), LBounds) and
          (Abs(Abs(LBounds.Left - GControlBefore.Left) - 12) < 2) and
          (Abs(Abs(LBounds.Top - GControlBefore.Top) - 8) < 2),
          'actual borrowed control geometry follows the camera');
        Check(MainView.InputFor('name-input') = GCanvasInput,
          'camera geometry does not remount the edited control');
        GPanel.MovePipeline(NyxCanvasPoint(0, 20));
        KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-title'), 'right-twice');
      end;
    10:
      begin

        if not GPanelDragRequested then
        begin
          Check(GPanel.PipelineBounds.Defined and (Abs(GPanel.PipelineBounds.Left - 16) < 1),
            'successive physical arrow keys retain both pending panel movements');
          GPanelDragRequested := True;
          DragPanel;
          Exit;
        end;
        Check(Abs(GPanel.PipelineBounds.Top - GPanelDragTop - 32) < 1,
          'actual captured header dragging moves the pipeline within the canvas');
        KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-title'), 'home');
      end;
    11:
      begin
        Check(TNyxCodec.Encode(GDocument) = GBefore,
          'runtime wiring, script execution and canvas movement preserve every authored byte');
        {$ifdef PAS2JS}
        document.body.setAttribute('data-capture-checkpoint', 'control-connectors');
        {$endif}
      end;
    12:
      begin
        {$ifdef PAS2JS}

        if document.body.getAttribute('data-capture-observed') <> 'control-connectors' then
        begin
          Exit;
        end;
        {$endif}
        LDisclosure := RetainNyxControl(GWireFace.PipelineConnectorRenderer.Root.Find(
          WireID('-options'))) as INyxDisclosure;
        KeyButton(GWireFace.PipelineConnectorRenderer, LDisclosure.HeaderButton.ID);
      end;
    13:
      begin
        KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-remove-applied'));
      end;
    14:
      begin
        Supports(GCanvasView, INyxCanvasRuntimeView, LRuntime);
        Check(LRuntime.BehaviorFor(NyxCanvasItem('Greeting')).Routes.Count = 1,
          'unacknowledged removal preserves the live route');
        Check(GWireFace.PipelineConnectorRenderer.Root.Find(WireID('-options')).Prop('value') = 'true',
          'a refused action retains the open options disclosure');
        ConfirmRemoval;
      end;
    15:
      begin
        KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-remove-applied'));
      end;
    16:
      begin
        Supports(GCanvasView, INyxCanvasRuntimeView, LRuntime);
        GBindings := LRuntime.BehaviorFor(NyxCanvasItem('Greeting')).Routes;
        Check(GBindings.Count = 0, 'acknowledged keyboard removal retires the applied route');
        EditName('Unwired');
        Check(Caption = 'Hello, Grace!', 'removing wires preserves output and ordinary control input');
        LFailed := False;
        try
          GWires.Remove(0);
        except
          on E: ENyxFlow do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and (GWires.WireCount = 2), 'staged-wire removal also requires acknowledgement');
        GWires.Remove(0, True);
        Check(GWires.Dirty and (GWires.WireCount = 1), 'confirmed staged removal retains unfinished intent');
        KeyButton(GToolFace.Renderer, GTools.Inspector.Root.ID + '-canvas-connectors');
      end;
    17:
      begin
        Check(not GWireMode.ControlConnectorsVisible and (GWires.WireCount = 1) and GWires.Dirty,
          'hiding connector chrome retains its unfinished proposal');
        Check(MainView.InputFor('name-input') = GCanvasInput,
          'hiding connectors retains the ordinary embedded editor');
        KeyButton(GToolFace.Renderer, GTools.Inspector.Root.ID + '-canvas-connectors');
      end;
    18:
      begin
        Check(GWireMode.ControlConnectorsVisible and (GWireMode.ControlWires = GWires) and
          (GWires.WireCount = 1), 'reopening restores the same controller and unfinished wire');
        Supports(GCanvasView, INyxCanvasFlowEditing, LFlows);
        Supports(GCanvasView, INyxCanvasRuntimeView, LRuntime);
        LDefinitions := NewNyxFlowDeclarations.Define(NyxFlowRef('Extra pipeline'),
          NewNyxFlowGraph
            .WithStep(NyxFlowStep('first'), NewNyxProgramLogic(NyxProgramRef('Greeting'), GreetingProgramContract))
            .WithStep(NyxFlowStep('second'), NewNyxProgramLogic(NyxProgramRef('Greeting'), GreetingProgramContract))
            .Connect(NyxFlowEndpoint(NyxFlowStep('first'), NyxFlowPort('Caption')),
              NyxFlowEndpoint(NyxFlowStep('second'), NyxFlowPort('Name'))).Build);
        LFlows.ApplyFlowChanges([NyxDefineFlow(NyxFlowRef('Extra pipeline'),
          LDefinitions.Definition(NyxFlowRef('Extra pipeline')))],
          LRuntime.Scene.Revision, LFlows.FlowRevision);
        Check(GWires.Stale, 'independent graph publication fences the old connector proposal');
        LFailed := False;
        try
          GWires.Apply(NyxFlowRouteID('Stale proposal'));
        except
          on E: ENyxFlow do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and GWires.Dirty and (GWires.WireCount = 1),
          'stale Apply preserves the complete unfinished proposal');
        LFailed := False;
        try
          GWires.Reload;
        except
          on E: ENyxFlow do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and GWires.Dirty, 'Reload refuses silent draft discard');
        GWires.Reload(True);
        Check(not GWires.Stale and not GWires.Dirty and (GWires.WireCount = 0),
          'acknowledged Reload captures current routes without restoring old removal authority');
        GTools.Refresh;
      end;
    19:
      begin
        LCount := GWires.PortCount;
        LRestricted := NewNyxToolControlWires('limited-wires', GCanvasView,
          NyxCanvasItem('Greeting'), TNyxToolWireOptions.Defaults.Ports(LCount));
        try
          LFailed := False;
          try
            LRestricted.SelectFlow(NyxFlowRef('Extra pipeline'));
          except
            on E: ENyxFlow do
            begin
              LFailed := True;
            end;
          end;
          Check(LFailed and (LRestricted.PortCount = LCount) and
            (LRestricted.BuildPipeline.Node.Find('limited-wires-flow').Prop('value') = 'Greeting flow'),
            'a later discovery-budget refusal preserves the exact previous pipeline selection and ports');
        finally
          LRestricted.Disconnect;
          LRestricted := nil;
        end;
        GWires.SelectFlow(NyxFlowRef('Extra pipeline'));
        LInternalInput := False;
        for LPortIndex := 0 to GWires.PortCount - 1 do
        begin
          LPort := GWires.Port(LPortIndex);
          LInternalInput := LInternalInput or ((LPort.Kind = ntwpkPipeline) and
            LPort.Endpoint.Same(NyxFlowEndpoint(NyxFlowStep('second'), NyxFlowPort('Name'))));
        end;
        Check(not LInternalInput and (GWires.PortCount = LCount + 1),
          'an input already supplied by a graph edge is never advertised as another writable input');
        GTools.Refresh;
        Check(TNyxCodec.Encode(GDocument) = GBefore,
          'warned removal, retained drafts and runtime graph edits preserve authored bytes');
        EditName('Independent', True);
        Click(True);
      end;
    20:
      begin

        if not Idle(GOther.FlowRoutes) then
        begin
          Exit;
        end;
        Check((Caption(True) = 'Welcome, Independent!') and (Caption = 'Hello, Grace!'),
          'another application retains its independent program, store and native-worker/browser-queue execution');
        {$ifdef NYX_PIPELINE_NODE_FACES}
        StartPipelineNodes;
        {$else}
        {$ifndef PAS2JS}
        LCapture := TWinControl(GWireFace.PipelineConnectorRenderer.ControlFor(WireID('-title')));
        Windows.SendMessage(LCapture.Handle, WM_LBUTTONDOWN, MK_LBUTTON, LPARAM(8 shl 16 or 8));
        GWires.Disconnect;
        GTools.Disconnect;
        Check(GCanvasView.Connected and (MainView.InputFor('name-input') = GCanvasInput),
          'retiring a captured face after its controller disconnects releases capture without querying that retired owner');
        {$endif}
        GDone := True;
        {$endif}
      end;
  end;
  Inc(GStage);
end;
