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
{ Actual operation-port input extends the maintained wide node journey. Fixture
  preparation adds one typed fan-out step through the ordinary runtime owner;
  removal, replacement and publication below use the real Nyx cards. Browser
  requests require the existing independent Pascal input driver over HTTP. }

procedure StartPipelineGraph;
var
  LGraph: INyxToolPipelineGraphEditing;
  LLogic: INyxToolCanvasLogicAuthoring;
  LFlows: INyxCanvasFlowEditing;
  LRuntime: INyxCanvasRuntimeView;
begin
  Check(Supports(GWires, INyxToolPipelineGraphEditing, LGraph), 'node graph editing is optional typed capability');
  Check(Supports(GTools.Inspector, INyxToolCanvasLogicAuthoring, LLogic), 'cards share the ordinary Logic owner');
  Check(Supports(GCanvasView, INyxCanvasFlowEditing, LFlows), 'prepared runtime graph owner exists');
  Supports(GCanvasView, INyxCanvasRuntimeView, LRuntime);
  GGraphEditing := LGraph;
  GGraphLogic := LLogic;
  GGraphFlows := LFlows;
  LFlows.ApplyFlowChanges([
    NyxAddFlowStep(NyxFlowRef('Extra pipeline'), NyxFlowStep('third'),
      NewNyxProgramLogic(NyxProgramRef('Greeting'), GreetingProgramContract)),
    NyxConnectFlow(NyxFlowRef('Extra pipeline'),
      NyxFlowEndpoint(NyxFlowStep('first'), NyxFlowPort('Caption')),
      NyxFlowEndpoint(NyxFlowStep('third'), NyxFlowPort('Name')))],
    LRuntime.Scene.Revision, LFlows.FlowRevision);
  GWires.Reload(True);
  GTools.Refresh;
  GNodeMode.MovePipelineNode(NyxFlowStep('third'),
    GCanvasView.Camera.ViewportToWorld(NyxCanvasPoint(1040, 360)));
  GGraphBefore := LFlows.RuntimeFlows.ToData.ToJSON;
  KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-graph-begin'));
end;

procedure GraphConfirm;
var
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TWinControl;{$endif}
begin
  {$ifdef PAS2JS}
  LInput := GWireFace.PipelineConnectorRenderer.InputFor(WireID('-graph-confirm'), niRuntime);
  LInput.scrollIntoView;
  LInput.focus;
  Inc(GInputRequest);
  document.body.setAttribute('data-wire-kind', 'space');
  document.body.setAttribute('data-wire-request', IntToStr(GInputRequest));
  {$else}
  LInput := TWinControl(GWireFace.PipelineConnectorRenderer.InputFor(WireID('-graph-confirm'), niRuntime));
  Check(LInput.CanFocus, 'internal connection acknowledgement is actual native input');
  LInput.SetFocus;
  Windows.SendMessage(LInput.Handle, WM_KEYDOWN, VK_SPACE, 1);
  Windows.SendMessage(LInput.Handle, WM_KEYUP, VK_SPACE, 1);
  {$endif}
end;

procedure GraphName(const AValue: TNyxText);
{$ifndef PAS2JS}
var
  LInput: TCustomEdit;
  LIndex: Integer;
{$endif}
begin
  {$ifdef PAS2JS}
  GCanvasInput.setAttribute('data-wire-editor', 'true');
  document.body.setAttribute('data-graph-name', AValue);
  Inc(GInputRequest);
  document.body.setAttribute('data-wire-kind', 'edit-graph-name');
  document.body.setAttribute('data-wire-request', IntToStr(GInputRequest));
  {$else}
  LInput := TCustomEdit(GCanvasInput);
  LInput.SetFocus;
  LInput.SelectAll;
  for LIndex := 1 to Length(AValue) do
  begin
    Windows.SendMessage(LInput.Handle, WM_CHAR, Ord(AValue[LIndex]), 1);
  end;
  {$endif}
end;

procedure DragInternal;
var
  LSourceIndex: Integer;
  LTargetIndex: Integer;
  {$ifdef PAS2JS}
  LSource: TJSHTMLElement;
  LTarget: TJSHTMLElement;
  {$else}
  LSource: TWinControl;
  LTarget: TWinControl;
  LStart: TPoint;
  LPoint: TPoint;
  LPosition: LPARAM;
  {$endif}
begin
  LSourceIndex := NodeRoutePort('first', 'Caption');
  LTargetIndex := NodeRoutePort('second', 'Name');
  {$ifdef PAS2JS}
  LSource := GNodeFaces.PipelineNodeRenderer(NyxFlowStep('first'))
    .ElementFor(GWires.PortControl(LSourceIndex).ID, niRuntime);
  LTarget := GNodeFaces.PipelineNodeRenderer(NyxFlowStep('second'))
    .ElementFor(GWires.PortControl(LTargetIndex).ID, niRuntime);
  LSource.setAttribute('data-wire-source', 'true');
  LTarget.setAttribute('data-wire-target', 'true');
  Inc(GInputRequest);
  document.body.setAttribute('data-wire-kind', 'touch-wire');
  document.body.setAttribute('data-wire-request', IntToStr(GInputRequest));
  {$else}
  LSource := TWinControl(GNodeFaces.PipelineNodeRenderer(NyxFlowStep('first'))
    .ControlFor(GWires.PortControl(LSourceIndex).ID, niRuntime));
  LTarget := TWinControl(GNodeFaces.PipelineNodeRenderer(NyxFlowStep('second'))
    .ControlFor(GWires.PortControl(LTargetIndex).ID, niRuntime));
  LStart := Types.Point(LSource.Width div 2, LSource.Height div 2);
  LPoint := LSource.ScreenToClient(LTarget.ClientToScreen(Types.Point(LTarget.Width div 2, LTarget.Height div 2)));
  Windows.SendMessage(LSource.Handle, WM_LBUTTONDOWN, MK_LBUTTON,
    LPARAM((LStart.Y and $FFFF) shl 16 or (LStart.X and $FFFF)));
  LPosition := LPARAM((LPoint.Y and $FFFF) shl 16 or (LPoint.X and $FFFF));
  Windows.SendMessage(LSource.Handle, WM_MOUSEMOVE, MK_LBUTTON, LPosition);
  Windows.SendMessage(LSource.Handle, WM_LBUTTONUP, 0, LPosition);
  {$endif}
end;

procedure StepPipelineGraph;
var
  LFailed: Boolean;
  LPort: TNyxToolPipelinePort;
  LIndex: Integer;
  LRoot: TNyxNode;
  LEvent: TNyxEventInfo;
  LRuntime: INyxCanvasRuntimeView;
  {$ifndef PAS2JS}
  LInput: TCustomEdit;
  LBitmap: Graphics.TBitmap;
  LPNG: TPortableNetworkGraphic;
  LHeader: TWinControl;
  {$endif}
begin
  {$ifndef PAS2JS}

  if GNodeTracedStage <> GStage then
  begin
    GNodeTracedStage := GStage;
    WriteLn('Internal stage ', GStage, ' / elapsed ms ', GetTickCount64 - GStarted);
    Flush(Output);
  end;
  {$endif}
  case GStage of
    39:
      begin
        Check(GGraphEditing.GraphEditing and (GNodeMetadata.NodeCount = 3) and
          (GWires.ControlCount = 0) and (GWires.PortCount = 6),
          'explicit internal mode exposes all six graph ports and preserves embedded controls');
        LFailed := False;
        try
          GWires.BuildControl(NyxControl('name-input'));
        except
          on E: ENyxFlow do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed, 'internal mode refuses inactive control-strip composition');
        LPort := GNodeMetadata.NodePort(1, 0);
        Check((LPort.RoutePort >= 0) and
          (GNodeFaces.PipelineNodeRenderer(NyxFlowStep('second')).Root
            .Find(LPort.DisplayControl.ID).Prop('enabled') = 'true'),
          'occupied internal input is visible and selectable for acknowledged replacement');
        KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-graph-wire-0'));
      end;
    40: KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-graph-remove'));
    41:
      begin
        Check((GNodeMetadata.NodeWireCount = 2) and not GGraphLogic.HasPendingLogic and
          (GWireFace.PipelineConnectorRenderer.Root.Find(WireID('-message')).Prop('text') <> ''),
          'unconfirmed actual removal retains the exact shared proposal');
        GraphConfirm;
      end;
    42: KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-graph-remove'));
    43:
      begin
        Check((GNodeMetadata.NodeWireCount = 1) and GGraphLogic.HasPendingLogic and
          (GGraphFlows.RuntimeFlows.ToData.ToJSON = GGraphBefore),
          'actual removal stages only in the same Logic proposal, leaving runtime graph exact');
        KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-graph-apply'));
      end;
    44:
      begin
        Check(GGraphLogic.HasPendingLogic and (GNodeMetadata.NodeWireCount = 1) and
          (GGraphFlows.RuntimeFlows.ToData.ToJSON = GGraphBefore),
          'incomplete route admission preserves the pending graph and complete live registry');
        GraphName('Kept live');
      end;
    45:
      begin

        if not Idle(GBindings) then
        begin
          Exit;
        end;
        Check(Caption = 'Hello, Hello, Kept live!!', 'rejected graph leaves the working tool executable');
        DragInternal;
      end;
    46:
      begin
        Check((GNodeMetadata.NodeWireCount = 2) and GGraphLogic.HasPendingLogic,
          'captured pointer input restores connectivity while retaining chronological wire order');
        KeyButton(GNodeFaces.PipelineNodeRenderer(NyxFlowStep('third')),
          GWires.PortControl(NodeRoutePort('third', 'Caption')).ID);
      end;
    47: KeyButton(GNodeFaces.PipelineNodeRenderer(NyxFlowStep('second')),
      GWires.PortControl(NodeRoutePort('second', 'Name')).ID);
    48: KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-graph-connect'));
    49:
      begin
        Check(GGraphLogic.HasPendingLogic and (GNodeMetadata.NodeWireCount = 2) and
          (GGraphFlows.RuntimeFlows.ToData.ToJSON = GGraphBefore),
          'keyboard connection refuses silent replacement of an occupied input');
        GraphConfirm;
      end;
    50: KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-graph-connect'));
    51:
      begin
        Check(GGraphLogic.HasPendingLogic and (GNodeMetadata.NodeWireCount = 2) and
          (GGraphFlows.RuntimeFlows.ToData.ToJSON = GGraphBefore),
          'acknowledged replacement stages disconnect/connect as one shared typed group');
        KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-graph-apply'));
      end;
    52:
      begin
        Check(not GGraphLogic.HasPendingLogic and
          (GGraphFlows.RuntimeFlows.ToData.ToJSON <> GGraphBefore),
          'ordinary node Apply publishes the complete prepared graph');
        Supports(GCanvasView, INyxCanvasRuntimeView, LRuntime);
        GBindings := LRuntime.BehaviorFor(NyxCanvasItem('Greeting')).Routes;
        GraphName('Connected');
      end;
    53:
      begin

        if not Idle(GBindings) then
        begin
          Exit;
        end;
        Check(Caption = 'Hello, Hello, Hello, Connected!!!',
          'actual input executes the new three-step wiring into the actual bound control');
        Check((Caption(True) = 'Welcome, Independent!') and
          (MainView.InputFor('name-input') = GCanvasInput),
          'graph publication preserves independent application and embedded editor identity');
        GGraphBefore := GGraphFlows.RuntimeFlows.ToData.ToJSON;
        GGraphLogic.ShowLogic;
        GTools.Refresh;
      end;
    54:
      begin
        {$ifdef PAS2JS}
        GGraphDraftFace := GToolFace.Renderer.InputFor(GTools.Inspector.Root.ID + '-logic-step-name');
        GGraphDraftFace.setAttribute('data-graph-draft', 'true');
        Inc(GInputRequest);
        document.body.setAttribute('data-wire-kind', 'graph-draft');
        document.body.setAttribute('data-wire-request', IntToStr(GInputRequest));
        {$else}
        LInput := TCustomEdit(GToolFace.Renderer.InputFor(GTools.Inspector.Root.ID + '-logic-step-name'));
        GGraphDraftFace := LInput;
        LInput.SetFocus;
        LInput.SelectAll;
        for LIndex := 1 to Length('unfinished node') do
        begin
          Windows.SendMessage(LInput.Handle, WM_CHAR, Ord('unfinished node'[LIndex]), 1);
        end;
        {$endif}
      end;
    55:
      begin
        Check((GToolFace.Renderer.InputFor(GTools.Inspector.Root.ID + '-logic-step-name') = GGraphDraftFace) and
          {$ifdef PAS2JS}(TJSHTMLInputElement(GGraphDraftFace).selectionStart = Length('unfinished node')){$else}
          (TCustomEdit(GGraphDraftFace).SelStart = Length('unfinished node')){$endif},
          'ordinary Logic typing preserves the actual editor and caret');
        LFailed := False;
        try
          GWires.Connect(NodeRoutePort('first', 'Caption'), NodeRoutePort('second', 'Name'), True);
        except
          on E: ENyxFlow do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and (GGraphFlows.RuntimeFlows.ToData.ToJSON = GGraphBefore),
          'unfinished ordinary Logic fields refuse canvas edits before any mutation');
        LFailed := False;
        try
          GWires.Reload;
        except
          on E: ENyxCanvasScene do
          begin
            LFailed := True;
          end;
        end;
        LRoot := GToolFace.Renderer.Root;
        Check(LFailed and (LRoot.Find(GTools.Inspector.Root.ID + '-logic-step-name').Prop('value') = 'unfinished node'),
          'unacknowledged Reload retains exact ordinary field text');
        GGraphDraftFace := nil;
        GWires.Reload(True);
        GGraphLogic.ShowLogic(False);
        GTools.Refresh;
      end;
    56:
      begin
        GGraphOldFace := GNodeMetadata.BuildNode(0);
        GGraphLogic.SelectLogicFlow(NyxFlowRef('Extra pipeline'), True);
        GTools.Refresh;
      end;
    57:
      begin
        LFailed := False;
        try
          GWires.Connect(NodeRoutePort('first', 'Caption'), NodeRoutePort('second', 'Name'), True);
        except
          on E: ENyxFlow do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and (GGraphFlows.RuntimeFlows.ToData.ToJSON = GGraphBefore),
          'same-flow owner reset invalidates the old proposal version');
        GGraphEditing.BeginGraphEditing;
        GGraphLogic.ShowLogic(False);
        GTools.Refresh;
      end;
    58:
      begin
        LFailed := False;
        try
          GWires.Connect(NodeRoutePort('second', 'Caption'), NodeRoutePort('first', 'Name'), True);
        except
          on E: ENyxFlow do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and not GGraphLogic.HasPendingLogic and
          (GNodeMetadata.NodeWireCount = 2), 'cycle admission refuses the whole candidate without changing shared topology');
        LFailed := False;
        try
          GWires.Connect(NodeRoutePort('first', 'Name'), NodeRoutePort('second', 'Name'), True);
        except
          on E: ENyxFlow do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and not GGraphLogic.HasPendingLogic, 'input/input wiring refuses typed direction mismatch');
        LEvent := Default(TNyxEventInfo);
        LEvent.Trigger := ntClick;
        Check(not GWires.Handle(GGraphOldFace.Node.Find(WireID('-graph-port-0')), LEvent),
          'retained old face cannot select a port after proposal epoch retirement');
        Check((GGraphFlows.RuntimeFlows.ToData.ToJSON = GGraphBefore) and
          (TNyxCodec.Encode(GDocument) = GBefore), 'all refusals preserve runtime and authored document bytes');
        GGraphEditing.RemoveGraphWire(0, True);
        GWires.Connect(NodeRoutePort('first', 'Caption'), NodeRoutePort('third', 'Name'));
        GGraphLogic.ApplyLogic;
        Check(GWires.Stale, 'ordinary Logic Apply retires the separately retained canvas proposal frame');
        GGraphEditing.BeginGraphEditing;
        GTools.Refresh;
      end;
    59:
      begin
        {$ifndef PAS2JS}

        if ParamStr(1) <> '' then
        begin
          LBitmap := GApp.Window.GetFormImage;
          LPNG := TPortableNetworkGraphic.Create;
          try
            LPNG.Assign(LBitmap);
            LPNG.SaveToFile(ParamStr(1));
          finally
            LPNG.Free;
            LBitmap.Free;
          end;
        end;
        LHeader := TWinControl(GNodeFaces.PipelineNodeRenderer(NyxFlowStep('first')).ControlFor(
          GNodeFaces.PipelineNodeRenderer(NyxFlowStep('first')).Root.Part(NyxPart('move')).ID, niRuntime));
        Windows.SendMessage(LHeader.Handle, WM_LBUTTONDOWN, MK_LBUTTON, LPARAM(12 shl 16 or 12));
        {$endif}
        GWires.Disconnect;
        GTools.Disconnect;
        LFailed := False;
        try
          GGraphEditing.BeginGraphEditing;
        except
          on E: ENyxFlow do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and GCanvasView.Connected and (MainView.InputFor('name-input') = GCanvasInput),
          'captured node retirement revokes graph access without retiring embedded application');
        GDone := True;
      end;
  end;
  Inc(GStage);
end;
