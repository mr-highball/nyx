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
{ Actual target input extends the existing connector journey. The earlier
  route/draft/control cases still run; these cases qualify operation expansion
  without synthesizing portable Nyx events or claiming browser execution. }

{$ifndef PAS2JS}
type
  { Real native-worker probe retains only the optional view capability while
    the fixture keeps its UI owner alive. Every cross-thread call must refuse
    before reading presentation data or changing visibility/positions. Cleanup
    joins this short probe before releasing its final UI capability lease. }
  TNodeWorkerProbe = class(TInterfacedObject, INyxWork)
  public
    Mode: INyxToolCanvasPipelineNodes;
    Refused: Integer;
    ReadValue: TNyxDataValue;
    procedure Execute(const AExecution: INyxExecution);
  end;
{$endif}

var
  GNodeMode: INyxToolCanvasPipelineNodes;
  GNodeFaces: {$ifdef PAS2JS}INyxBrowserToolPipelineNodes{$else}INyxLCLToolPipelineNodes{$endif};
  GNodeMetadata: INyxToolPipelineNodes;
  GNodeBefore: TNyxCanvasBounds;
  GNodeExport: TNyxText;
  GNodeHeader: TObject;
  GNodeTracedStage: Integer;
  GNodeCameraRestored: Boolean;
  {$ifndef PAS2JS}
  GNodeWorker: TNodeWorkerProbe;
  GNodeWorkerLease: INyxWork;
  GNodeWorkerExecution: INyxExecution;
  {$endif}

{$ifndef PAS2JS}
procedure TNodeWorkerProbe.Execute(const AExecution: INyxExecution);
begin
  try
    ReadValue := Mode.ExportPipelineNodePositions;
  except
    on E: ENyxSchedule do
    begin
      Inc(Refused);
    end;
  end;
  try
    Mode.ShowPipelineNodes(False);
  except
    on E: ENyxSchedule do
    begin
      Inc(Refused);
    end;
  end;
  try
    Mode.MovePipelineNode(NyxFlowStep('first'), NyxCanvasPoint(0, 0));
  except
    on E: ENyxSchedule do
    begin
      Inc(Refused);
    end;
  end;
end;
{$endif}

procedure RetirePipelineNodes;
{$ifndef PAS2JS}
var
  LStarted: QWord;
{$endif}
begin
  {$ifndef PAS2JS}

  if GNodeWorkerExecution <> nil then
  begin
    LStarted := GetTickCount64;
    while GNodeWorkerExecution.Status in [nesPending, nesRunning] do
    begin
      CheckSynchronize;
      Application.ProcessMessages;

      if GetTickCount64 - LStarted > 5000 then
      begin
        raise Exception.Create('Native node worker probe did not retire');
      end;
      Sleep(1);
    end;
  end;

  if GNodeWorker <> nil then
  begin
    GNodeWorker.Mode := nil;
  end;
  GNodeWorker := nil;
  GNodeWorkerLease := nil;
  GNodeWorkerExecution := nil;
  {$endif}
  GNodeMode := nil;
  GNodeFaces := nil;
  GNodeMetadata := nil;
end;

procedure StartPipelineNodes;
var
  LMode: INyxToolCanvasPipelineNodes;
  LFaces: {$ifdef PAS2JS}INyxBrowserToolPipelineNodes{$else}INyxLCLToolPipelineNodes{$endif};
  LMetadata: INyxToolPipelineNodes;
begin
  Check(Supports(GTools, INyxToolCanvasPipelineNodes, LMode), 'public operation-node presentation exists');
  Check(Supports(GTools, {$ifdef PAS2JS}INyxBrowserToolPipelineNodes{$else}INyxLCLToolPipelineNodes{$endif},
    LFaces), 'adapter exposes borrowed operation renderers');
  Check(Supports(GWires, INyxToolPipelineNodes, LMetadata), 'the route controller exposes inert full graph metadata');
  GNodeMode := LMode;
  GNodeFaces := LFaces;
  GNodeMetadata := LMetadata;
  GNodeTracedStage := -1;
  GNodeCameraRestored := False;
  KeyButton(GWireFace.PipelineConnectorRenderer, GNodeMetadata.ModeControl.ID);
end;

function NodeRoutePort(const AStep, APort: TNyxText): Integer;
var
  LIndex: Integer;
  LPort: TNyxToolWirePort;
  LEndpoint: TNyxFlowEndpoint;
begin
  LEndpoint := NyxFlowEndpoint(NyxFlowStep(AStep), NyxFlowPort(APort));
  for LIndex := 0 to GWires.PortCount - 1 do
  begin
    LPort := GWires.Port(LIndex);

    if (LPort.Kind = ntwpkPipeline) and LPort.Endpoint.Same(LEndpoint) then
    begin
      Exit(LIndex);
    end;
  end;
  raise Exception.Create('Expected a writable exposed node endpoint');
end;

procedure DragOperation;
var
  {$ifdef PAS2JS}LHeader: TJSHTMLElement;{$else}LHeader: TWinControl;{$endif}
begin
  {$ifdef PAS2JS}
  LHeader := GNodeFaces.PipelineNodeRenderer(NyxFlowStep('first')).ElementFor(
    GNodeFaces.PipelineNodeRenderer(NyxFlowStep('first')).Root.Part(NyxPart('move')).ID, niRuntime);
  LHeader.setAttribute('data-wire-panel', 'true');
  Inc(GInputRequest);
  document.body.setAttribute('data-wire-kind', 'touch-panel');
  document.body.setAttribute('data-wire-request', IntToStr(GInputRequest));
  {$else}
  LHeader := TWinControl(GNodeFaces.PipelineNodeRenderer(NyxFlowStep('first')).ControlFor(
    GNodeFaces.PipelineNodeRenderer(NyxFlowStep('first')).Root.Part(NyxPart('move')).ID, niRuntime));
  Windows.SendMessage(LHeader.Handle, WM_LBUTTONDOWN, MK_LBUTTON, LPARAM(12 shl 16 or 12));
  Windows.SendMessage(LHeader.Handle, WM_MOUSEMOVE, MK_LBUTTON, LPARAM(44 shl 16 or 12));
  Windows.SendMessage(LHeader.Handle, WM_LBUTTONUP, 0, LPARAM(44 shl 16 or 12));
  {$endif}
end;

procedure CheckVisibleNodePort(AStep, APort: Integer);
var
  LPort: TNyxToolPipelinePort;
  LBounds: TNyxCanvasBounds;
  LActual: TNyxCanvasBounds;
  {$ifdef PAS2JS}
  LElement: TJSHTMLElement;
  LRect: TJSDOMRect;
  LSurface: TJSDOMRect;
  {$else}
  LControl: TControl;
  LPosition: TPoint;
  {$endif}
begin
  LPort := GNodeMetadata.NodePort(AStep, APort);
  LBounds := GNodeMode.PipelineNodeBounds(GNodeMetadata.NodeStep(AStep));
  {$ifdef PAS2JS}
  LElement := GNodeFaces.PipelineNodeRenderer(GNodeMetadata.NodeStep(AStep))
    .ElementFor(LPort.DisplayControl.ID, niRuntime);
  LRect := LElement.getBoundingClientRect;
  LSurface := GCanvasView.Surface.getBoundingClientRect;
  LActual := NyxCanvasBounds((LRect.left - LSurface.left) * GCanvasView.Surface.clientWidth / LSurface.width,
    (LRect.top - LSurface.top) * GCanvasView.Surface.clientHeight / LSurface.height,
    LRect.width * GCanvasView.Surface.clientWidth / LSurface.width,
    LRect.height * GCanvasView.Surface.clientHeight / LSurface.height);
  {$else}
  LControl := GNodeFaces.PipelineNodeRenderer(GNodeMetadata.NodeStep(AStep))
    .ControlFor(LPort.DisplayControl.ID, niRuntime);
  LPosition := GCanvasView.Surface.ScreenToClient(LControl.ClientToScreen(Point(0, 0)));
  LActual := NyxCanvasBounds(LPosition.X, LPosition.Y, LControl.Width, LControl.Height);
  {$endif}
  Check(LBounds.Defined and (LActual.Width > 0) and (LActual.Height > 0) and
    (LActual.Left >= LBounds.Left) and (LActual.Top >= LBounds.Top) and
    (LActual.Left + LActual.Width <= LBounds.Left + LBounds.Width + 1) and
    (LActual.Top + LActual.Height <= LBounds.Top + LBounds.Height + 1),
    'typed operation port is actually allocated inside its visible card');
end;

procedure StepPipelineNodes;
var
  LPort: TNyxToolPipelinePort;
  LCard: INyxCard;
  LDescriptor: TNyxLogicDescriptor;
  LFailed: Boolean;
  LEndpoint: TNyxToolPipelineWire;
  LRuntime: INyxCanvasRuntimeView;
  LBounds: TNyxCanvasBounds;
  LIndex: Integer;
  {$ifndef PAS2JS}
  LInput: TEdit;
  LHeader: TWinControl;
  LBitmap: Graphics.TBitmap;
  LPNG: TPortableNetworkGraphic;
  {$endif}
begin
  {$ifndef PAS2JS}

  if GNodeTracedStage <> GStage then
  begin
    GNodeTracedStage := GStage;
    WriteLn('Node stage ', GStage);
    Flush(Output);
  end;
  {$endif}
  case GStage of
    21:
      begin
        Check(GNodeMode.PipelineNodesVisible and (GNodeMetadata.NodeCount = 2) and
          (GNodeMetadata.NodeWireCount = 1), 'ordinary button expands every operation and existing wire');
        LEndpoint := GNodeMetadata.NodeWire(0);
        Check(LEndpoint.Source.Same(NyxFlowEndpoint(NyxFlowStep('first'), NyxFlowPort('Caption'))) and
          LEndpoint.Target.Same(NyxFlowEndpoint(NyxFlowStep('second'), NyxFlowPort('Name'))),
          'internal wire retains exact typed endpoints');
        LPort := GNodeMetadata.NodePort(1, 0);
        Check((LPort.RoutePort = -1) and
          (GNodeFaces.PipelineNodeRenderer(NyxFlowStep('second')).Root.Find(LPort.DisplayControl.ID).Prop('enabled') = 'false'),
          'already-connected input remains visible without accepting a second route source');
        LDescriptor := GNodeMetadata.NodeDescriptor(0).Labelled('Independent caller label');
        Check(LDescriptor.LabelCount = GNodeMetadata.NodeDescriptor(0).LabelCount + 1,
          'derived descriptor vectors do not modify the captured graph');
        LCard := GNodeMetadata.BuildNode(0);
        LCard.Configure.Text('Independent face customization').Done;
        Check(GNodeFaces.PipelineNodeRenderer(NyxFlowStep('first')).Root.Prop('text') <> 'Independent face customization',
          'returned Nyx cards have independent mutable ownership');
        LFailed := False;
        try
          GNodeMode.ShowPipelineNodes(True, 1, 4096);
        except
          on E: ENyxFlow do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and GNodeMode.PipelineNodesVisible, 'face budget refusal retains the current presentation');
        LFailed := False;
        try
          GNodeMetadata.ValidateFaces(2, 3);
        except
          on E: ENyxFlow do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed, 'display budget includes connected inputs, not only writable route ports');
        GNodeMode.MovePipelineNode(NyxFlowStep('first'),
          GCanvasView.Camera.ViewportToWorld(NyxCanvasPoint(1040, 20)));
        GNodeMode.MovePipelineNode(NyxFlowStep('second'),
          GCanvasView.Camera.ViewportToWorld(NyxCanvasPoint(1360, 360)));
        GPanel.MovePipeline(NyxCanvasPoint(650, 400));
      end;
    22:
      begin
        CheckVisibleNodePort(0, 0);
        CheckVisibleNodePort(0, 1);
        CheckVisibleNodePort(1, 0);
        CheckVisibleNodePort(1, 1);
        GNodeBefore := GNodeMode.PipelineNodeBounds(NyxFlowStep('first'));
        GNodeHeader := TObject(GNodeFaces.PipelineNodeRenderer(NyxFlowStep('first')));
        Check(GNodeBefore.Defined, 'allocated operation card has viewport bounds');
        {$ifndef PAS2JS}
        GNodeWorker := TNodeWorkerProbe.Create;
        GNodeWorkerLease := GNodeWorker;
        GNodeWorker.Mode := GNodeMode;
        GNodeWorkerExecution := GWireFace.PipelineConnectorRenderer.Events.Scheduler.Submit(
          GNodeWorkerLease, neThreaded);
        {$endif}
        KeyButton(GNodeFaces.PipelineNodeRenderer(NyxFlowStep('first')),
          GNodeFaces.PipelineNodeRenderer(NyxFlowStep('first')).Root.Part(NyxPart('move')).ID, 'right-twice');
      end;
    23:
      begin
        {$ifndef PAS2JS}

        if GNodeWorkerExecution.Status in [nesPending, nesRunning] then
        begin
          Exit;
        end;
        Check((GNodeWorkerExecution.Status = nesSucceeded) and (GNodeWorker.Refused = 3),
          'entered native worker rejects export, hide and placement before UI access');
        Check(GNodeMode.PipelineNodesVisible, 'rejected worker hide preserves current node visibility');
        {$endif}
        LBounds := GNodeMode.PipelineNodeBounds(NyxFlowStep('first'));
        Check(Abs(LBounds.Left - GNodeBefore.Left - 16 * GCanvasView.Camera.Zoom) < 2,
          'successive native/host arrow keys accumulate world movement');
        Check(TObject(GNodeFaces.PipelineNodeRenderer(NyxFlowStep('first'))) = GNodeHeader,
          'placement retains the actual operation renderer');
        GNodeExport := GNodeMode.ExportPipelineNodePositions.ToJSON;
        GNodeBefore := LBounds;
        GCanvasView.PanBy(14, 9);
      end;
    24:
      begin
        LBounds := GNodeMode.PipelineNodeBounds(NyxFlowStep('first'));
        Check((Abs(LBounds.Left - GNodeBefore.Left - 14) < 2) and
          (Abs(LBounds.Top - GNodeBefore.Top - 9) < 2), 'operation faces follow real camera panning');
        Check(GNodeMode.ExportPipelineNodePositions.ToJSON = GNodeExport,
          'camera panning does not rewrite exported world positions');
        GCanvasView.ZoomAt(2, NyxCanvasPoint(LBounds.Left, LBounds.Top));
      end;
    25:
      begin
        Check(GNodeMode.ExportPipelineNodePositions.ToJSON = GNodeExport,
          'camera zoom retains exact operation world positions');
        GNodeBefore := GNodeMode.PipelineNodeBounds(NyxFlowStep('first'));
        Check(GNodeBefore.Defined, 'zoom anchor retains a visible operation');
        DragOperation;
      end;
    26:
      begin
        if not GNodeCameraRestored then
        begin
          LBounds := GNodeMode.PipelineNodeBounds(NyxFlowStep('first'));
          Check(Abs(LBounds.Top - GNodeBefore.Top - 32) < 2,
            'captured header drag converts screen motion through non-unit camera zoom');
          { Zooming around a distant operation legitimately removes source
            controls from view. Restore the normal camera and wait for actual
            allocation before asking those controls to accept keyboard focus. }
          GCanvasView.SetCamera(NyxCanvasCamera(
            GCanvasView.Surface.{$ifdef PAS2JS}clientWidth{$else}ClientWidth{$endif},
            GCanvasView.Surface.{$ifdef PAS2JS}clientHeight{$else}ClientHeight{$endif},
            NyxCanvasPoint(8 + GCanvasView.Surface.{$ifdef PAS2JS}clientWidth{$else}ClientWidth{$endif} / 2,
              8 + GCanvasView.Surface.{$ifdef PAS2JS}clientHeight{$else}ClientHeight{$endif} / 2), 1));
          GNodeCameraRestored := True;
          Exit;
        end;
        KeyButton(GWireFace.ControlConnectorRenderer(GWires.Port(GValuePort).ControlPort.Control),
          GWires.PortControl(GValuePort).ID);
      end;
    27:
      begin
        LIndex := NodeRoutePort('first', 'Name');
        KeyButton(GNodeFaces.PipelineNodeRenderer(NyxFlowStep('first')), GWires.PortControl(LIndex).ID);
      end;
    28: KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-connect'));
    29:
      begin
        Check(GWires.WireCount = 1, 'actual control and operation port buttons stage their typed input');
        LIndex := NodeRoutePort('second', 'Caption');
        KeyButton(GNodeFaces.PipelineNodeRenderer(NyxFlowStep('second')), GWires.PortControl(LIndex).ID);
      end;
    30:
      begin
        KeyButton(GWireFace.ControlConnectorRenderer(GWires.Port(GCaptionPort).ControlPort.Control),
          GWires.PortControl(GCaptionPort).ID);
      end;
    31: KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-connect'));
    32:
      begin
        Check(GWires.WireCount = 2, 'operation output and real bound control stage the complete route');
        KeyButton(GWireFace.PipelineConnectorRenderer, WireID('-apply'));
      end;
    33:
      begin
        Supports(GCanvasView, INyxCanvasRuntimeView, LRuntime);
        GBindings := LRuntime.BehaviorFor(NyxCanvasItem('Greeting')).Routes;
        Check((GBindings.Count = 1) and not GWires.Dirty, 'node-face route uses the existing prepared publisher');
        {$ifdef PAS2JS}
        GCanvasInput.setAttribute('data-wire-editor', 'true');
        Inc(GInputRequest);
        document.body.setAttribute('data-wire-kind', 'edit-node-name');
        document.body.setAttribute('data-wire-request', IntToStr(GInputRequest));
        {$else}
        LInput := TEdit(GCanvasInput);
        LInput.SetFocus;
        LInput.SelectAll;
        for LIndex := 1 to Length('Node writer') do
        begin
          Windows.SendMessage(LInput.Handle, WM_CHAR, Ord('Node writer'[LIndex]), 1);
        end;
        {$endif}
      end;
    34:
      begin

        if not Idle(GBindings) then
        begin
          Exit;
        end;
        Check(Caption = 'Hello, Hello, Node writer!!',
          'actual input executes both connected Pascal nodes into the actual bound control');
        Check((Caption(True) = 'Welcome, Independent!') and
          (MainView.InputFor('name-input') = GCanvasInput),
          'independent application and original embedded editor retain their owners');
        GNodeExport := GNodeMode.ExportPipelineNodePositions.ToJSON;
        KeyButton(GWireFace.PipelineConnectorRenderer, GNodeMetadata.ModeControl.ID);
      end;
    35:
      begin
        Check(not GNodeMode.PipelineNodesVisible, 'ordinary mode button restores the combined pipeline');
        KeyButton(GWireFace.PipelineConnectorRenderer, GNodeMetadata.ModeControl.ID);
      end;
    36:
      begin
        Check(GNodeMode.PipelineNodesVisible and
          (GNodeMode.ExportPipelineNodePositions.ToJSON = GNodeExport),
          'hide/reopen retains every world placement without touching route history');
        GNodeMode.MovePipelineNode(NyxFlowStep('first'), NyxCanvasPoint(100000000, 100000000));
      end;
    37:
      begin
        Check(not GNodeMode.PipelineNodeBounds(NyxFlowStep('first')).Defined,
          'off-screen world positions do not become huge native allocations');
        GCanvasView.SetCamera(NyxCanvasCamera(
          GCanvasView.Surface.{$ifdef PAS2JS}clientWidth{$else}ClientWidth{$endif},
          GCanvasView.Surface.{$ifdef PAS2JS}clientHeight{$else}ClientHeight{$endif},
          NyxCanvasPoint(8 + GCanvasView.Surface.{$ifdef PAS2JS}clientWidth{$else}ClientWidth{$endif} / 2,
            8 + GCanvasView.Surface.{$ifdef PAS2JS}clientHeight{$else}ClientHeight{$endif} / 2), 1));
        GNodeMode.ResetPipelineNode(NyxFlowStep('first'));
        GNodeMode.MovePipelineNode(NyxFlowStep('first'),
          GCanvasView.Camera.ViewportToWorld(NyxCanvasPoint(1040, 20)));
        GNodeMode.MovePipelineNode(NyxFlowStep('second'),
          GCanvasView.Camera.ViewportToWorld(NyxCanvasPoint(1360, 360)));
        GPanel.MovePipeline(NyxCanvasPoint(650, 400));
      end;
    38:
      begin
        Check(TNyxCodec.Encode(GDocument) = GBefore, 'all node presentation/routing preserves complete authored bytes');
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
        GWires.Disconnect;
        GTools.Disconnect;
        Check(GCanvasView.Connected and (MainView.InputFor('name-input') = GCanvasInput),
          'retiring a captured operation never queries its retired controller or remounts embedded input');
        {$endif}
        GDone := True;
      end;
  end;
  Inc(GStage);
end;
