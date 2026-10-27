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

program nyx_tool_canvas_navigation_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Math, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec,
  nyx.controls, nyx.root.types, nyx.state, nyx.events, nyx.scheduler,
  nyx.canvas.types, nyx.canvas.geometry, nyx.canvas.declarations, nyx.canvas.workspace,
  nyx.canvas.navigation, nyx.canvas.navigation.sources, nyx.canvas.observers, nyx.tool.canvas,
  nyx.flow, nyx.flow.types, nyx.flow.declarations, nyx.logic
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser, nyx.canvas.browser,
    nyx.render.browser, nyx.tool.canvas.browser
  {$else}, Classes, Types, Interfaces, Forms, Controls, StdCtrls, Windows, Graphics,
    nyx.application.lcl, nyx.canvas.lcl, nyx.render.lcl, nyx.tool.canvas.lcl{$endif};

type
  { Creator callback borrows the fixture's global view only for Capture. It
    deliberately reenters/pans to verify that Fit never overwrites a newer
    published camera, and never retains a canvas through its subscription. }
  TGuardSource = class(TInterfacedObject, INyxCanvasNavigationSource)
  public
    Recursive: Boolean;
    function Capture(const ACamera: TNyxCanvasCamera): TNyxCanvasNavigationFrame;
  end;

  {$ifndef PAS2JS}
  { Exposes inherited font observation only; no native control is replaced. }
  TControlAccess = class(TControl);

  { Test-owned exception observer remains alive through all queued UI retirement
    turns, replacing unattended framework dialogs with a retained test failure. }
  TFailure = class
  public
    Message: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;

  { Joined worker borrows live navigation authority from the UI-owned fixture.
    Each entry must refuse before reading controls or mutating camera/chrome;
    the retained interface is released on the UI after the join. }
  TNavigationWorker = class(TThread)
  public
    Navigation: INyxToolCanvasNavigation;
    Rejected: Integer;
    procedure Execute; override;
  end;
  {$endif}

var
  GDocument: TNyxDocument;
  GBefore: TNyxText;
  GPositions: TNyxText;
  GTools: INyxToolCanvasView;
  GMode: INyxToolCanvasControlConnectors;
  GGraph: INyxToolCanvasGraphConnections;
  GNodes: INyxToolCanvasPipelineNodes;
  GNav: INyxToolCanvasNavigation;
  GFlows: INyxCanvasFlowEditing;
  GFlowRevision: Integer;
  GFinalFit: TNyxCanvasNavigationResult;
  GPendingCamera: TNyxCanvasCamera;
  GStage: Integer;
  GChecks: Integer;
  GDone: Boolean;
  GCompact: Boolean;
  GMixed: Boolean;
  GUniform: Boolean;
  GUniformWidth: Double;
  GUniformHeight: Double;
  GUniformHeaderWidth: Double;
  GUniformFace: TObject;
  GUniformHeader: TNyxControlRef;
  {$ifndef PAS2JS}
  GUniformFont: Integer;
  {$endif}
  GStarted: {$ifdef PAS2JS}Double{$else}QWord{$endif};
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GHost: TJSHTMLElement;
  GCanvas: INyxBrowserCanvasWorkspaceView;
  GWires: INyxBrowserToolCanvasConnectors;
  GNodeFaces: INyxBrowserToolPipelineNodes;
  GInput: TJSHTMLElement;
  {$else}
  GApp: TNyxLCLApplication;
  GCanvas: INyxLCLCanvasWorkspaceView;
  GWires: INyxLCLToolCanvasConnectors;
  GNodeFaces: INyxLCLToolPipelineNodes;
  GInput: TControl;
  GFailure: TFailure;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Tool navigation: ' + AReason);
  end;
  Inc(GChecks);
end;

function TGuardSource.Capture(const ACamera: TNyxCanvasCamera): TNyxCanvasNavigationFrame;
begin
  Result := Default(TNyxCanvasNavigationFrame);

  if Recursive then
  begin
    GNav.FitContent;
  end
  else
  begin
    GCanvas.PanBy(11, 7);
  end;
end;

procedure CheckCaptureGuard;
var
  LHost: INyxCanvasNavigationContributions;
  LSource: TGuardSource;
  LLease: INyxCanvasNavigationSource;
  LScope: INyxCancellationScope;
  LBefore: TNyxCanvasCamera;
  LFailed: Boolean;
begin
  Check(Supports(GCanvas, INyxCanvasNavigationContributions, LHost),
    'ordinary mounted canvas exposes optional contribution registration');
  LSource := TGuardSource.Create;
  LLease := LSource;
  LScope := LHost.ObserveNavigation(LLease);
  LBefore := GCanvas.Camera;
  try
    LSource.Recursive := True;
    LFailed := False;
    try
      GNav.FitContent;
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (GCanvas.Camera.WorldCenter.X = LBefore.WorldCenter.X),
      'recursive creator navigation refuses before publishing a camera');
    LSource.Recursive := False;
    LFailed := False;
    try
      GNav.FitContent;
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and
      (Abs(GCanvas.Camera.WorldCenter.X - LBefore.PanBy(11, 7).WorldCenter.X) < 0.000001),
      'a creator-published camera invalidates Fit instead of being overwritten');
  finally
    LScope.Cancel;
    LLease := nil;
    GCanvas.SetCamera(LBefore);
  end;
end;

{$ifndef PAS2JS}
procedure TFailure.Failed(ASender: TObject; AError: Exception);
begin
  Message := AError.Message;
end;

procedure TNavigationWorker.Execute;
var
  LExpanded: Boolean;
begin
  try
    Navigation.ConfigureNavigation(NyxCanvasNavigationOptions, ntctCompact);
  except
    on E: ENyxSchedule do
    begin
      Inc(Rejected);
    end;
  end;
  try
    Navigation.FitContent;
  except
    on E: ENyxSchedule do
    begin
      Inc(Rejected);
    end;
  end;
  try
    Navigation.RevealOperation(NyxFlowStep('read'));
  except
    on E: ENyxSchedule do
    begin
      Inc(Rejected);
    end;
  end;
  try
    LExpanded := Navigation.ToolsExpanded;

    if LExpanded then
    begin
      Rejected := -100;
    end;
  except
    on E: ENyxSchedule do
    begin
      Inc(Rejected);
    end;
  end;
end;

procedure CheckWorker;
var
  LWorker: TNavigationWorker;
begin
  LWorker := TNavigationWorker.Create(True);
  try
    LWorker.Navigation := GNav;
    LWorker.Start;
    LWorker.WaitFor;
    Check((LWorker.FatalException = nil) and (LWorker.Rejected = 4),
      'all public navigation reads/writes refuse an actual native worker');
    LWorker.Navigation := nil;
  finally
    LWorker.Free;
  end;
end;
{$endif}

function NavID(const AName: TNyxText): TNyxText;
begin
  Result := GTools.Inspector.Root.ID + '-navigation-' + AName;
end;

function DocumentForNavigation: TNyxDocument;
var
  LPage: INyxPage;
  LContent: INyxColumn;
  LTools: INyxToolCanvas;
  LScene: INyxCanvasDefinition;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'A place for every operation';
    Result.Flows.Define(NyxFlowRef('Data workshop'), NewNyxFlowGraph
      .WithStep(NyxFlowStep('read'), NewNyxJSONParse)
      .WithStep(NyxFlowStep('pretty'), NewNyxJSONFormat)
      .Connect(NyxFlowEndpoint(NyxFlowStep('read'), NyxLogicPort(nlpJSON)),
        NyxFlowEndpoint(NyxFlowStep('pretty'), NyxLogicPort(nlpJSON))).Build);
    LContent := NewNyxColumn('working-form');
    LContent.Configure.Padding(8).Gap(8).Done;
    LContent.Add(NewNyxLabel('form-title').WithText('Working notes'));
    LContent.Add(NewNyxInput('working-input').Configure.Value('Keep this note').Done);
    Result.AddComponent(LContent);
    LScene := NewNyxCanvasDefinition.WithLayer(NyxCanvasLayer('Workspace'))
      .WithCamera(NyxCanvasCamera(1000, 640, NyxCanvasPoint(0, 0), 1,
        NyxCanvasZoomLimits(0.001, 16))).WithScaling(nccsLogicalSize);

    if GUniform then
    begin
      LScene := LScene.WithScaling(nccsUniform);
    end;

    if GMixed then
    begin
      LScene := LScene.WithItem(NyxCanvasItem('Notes'), NyxReusableRoot('working-form'),
        NyxCanvasPlacement(NyxCanvasLayer('Workspace'), NyxCanvasBounds(-5000, 0, 220, 140)));
    end;
    Result.Canvases.Define(NyxCanvasRef('Workspace'), LScene);
    LPage := NewNyxPage('home');
    LPage.Configure.Padding(0).Gap(0).Done;
    LPage.Add(NewNyxHeading('title').WithText('Find your way around'));
    LTools := NewNyxToolCanvas('nav-tools').WithScene(NyxCanvasRef('Workspace'));
    LTools.Configure.Padding(0).Gap(0).Done;
    LTools.WorkspaceCanvas.Configure.Height(640).MinimumHeight(240).Done;

    if GCompact then
    begin
      LTools.WorkspaceCanvas.Configure.Height(360).Done;
    end;
    LPage.Add(LTools);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function Idle: Boolean;
begin
  Result := (GTools.RefreshExecution = nil) or
    not (GTools.RefreshExecution.Status in [nesPending, nesRunning]);

  if (GTools.RefreshExecution <> nil) and
    (GTools.RefreshExecution.Status in [nesFailed, nesCancelled]) then
  begin
    raise Exception.Create('Inspector refresh failed: ' + GTools.RefreshExecution.Failure);
  end;

  if GWires.ConnectorExecution <> nil then
  begin
    Result := Result and not (GWires.ConnectorExecution.Status in [nesPending, nesRunning]);

    if GWires.ConnectorExecution.Status in [nesFailed, nesCancelled] then
    begin
      raise Exception.Create('Connector refresh failed: ' + GWires.ConnectorExecution.Failure);
    end;
  end;
end;

procedure Press(const AID: TNyxText);
var
  LPresentation: INyxToolCanvasConnectorPresentation;
  LPanel: TNyxCanvasBounds;
  {$ifndef PAS2JS}
  LButton: TWinControl;
  LOrigin: TPoint;
  {$else}
  LButton: TJSHTMLElement;
  LRect: TJSDOMRect;
  LSurface: TJSDOMRect;
  {$endif}
begin
  Check(Supports(GTools, INyxToolCanvasConnectorPresentation, LPresentation),
    'button input retains public pipeline placement');
  LPanel := LPresentation.PipelineBounds;
  {$ifdef PAS2JS}
  LButton := GWires.PipelineConnectorRenderer.ElementFor(AID, niRuntime);
  LRect := LButton.getBoundingClientRect;
  LSurface := GCanvas.Surface.getBoundingClientRect;
  Check((LRect.top - LSurface.top >= LPanel.Top - 1) and
    (LRect.bottom - LSurface.top <= LPanel.Top + LPanel.Height + 1),
    'navigation action is allocated inside the visible toolbar before focus');
  LButton.click;
  {$else}
  LButton := TWinControl(GWires.PipelineConnectorRenderer.ControlFor(AID, niRuntime));
  LOrigin := GCanvas.Surface.ScreenToClient(LButton.ClientToScreen(Types.Point(0, 0)));
  Check((LOrigin.Y >= LPanel.Top - 1) and
    (LOrigin.Y + LButton.Height <= LPanel.Top + LPanel.Height + 1),
    'navigation action is allocated inside the visible toolbar before focus');
  Check(LButton.CanFocus, 'navigation button is an actual focusable native control');
  LButton.SetFocus;
  Windows.SendMessage(LButton.Handle, WM_KEYDOWN, VK_SPACE, 1);
  Windows.SendMessage(LButton.Handle, WM_KEYUP, VK_SPACE, 1);
  {$endif}
end;

procedure ChooseOperation(const AName: TNyxText);
var
  {$ifdef PAS2JS}
  LSelect: TJSHTMLSelectElement;
  {$else}
  LSelect: TComboBox;
  {$endif}
begin
  {$ifdef PAS2JS}
  LSelect := TJSHTMLSelectElement(GWires.PipelineConnectorRenderer.InputFor(NavID('step'), niRuntime));
  LSelect.value := AName;
  LSelect.dispatchEvent(TJSEvent.new('change'));
  {$else}
  Check(GWires.PipelineConnectorRenderer.InputFor(NavID('step'), niRuntime) is TComboBox,
    'operation chooser exposes a native combo input rather than its labeled host');
  LSelect := TComboBox(GWires.PipelineConnectorRenderer.InputFor(NavID('step'), niRuntime));
  Check(LSelect.CanFocus, 'operation chooser is an actual focusable native control');
  LSelect.SetFocus;
  LSelect.ItemIndex := LSelect.Items.IndexOf(AName);
  Check((LSelect.ItemIndex >= 0) and Assigned(LSelect.OnChange),
    'named operation is available through the renderer-owned select event');
  { Exercise the native widget's change adapter after exact selection. The
    separate button path uses real window key messages; this is not a claim
    about physical keyboard, mobile picker or assistive-technology input. }
  LSelect.OnChange(LSelect);
  {$endif}
end;

procedure CheckVisible(const AStep: TNyxFlowStepRef);
var
  LBounds: TNyxCanvasBounds;
  LPanel: TNyxCanvasBounds;
  LPanelMode: INyxToolCanvasConnectorPresentation;
begin
  LBounds := GNodes.PipelineNodeBounds(AStep);
  Check(LBounds.Defined and (LBounds.Left >= -1) and (LBounds.Top >= -1) and
    (LBounds.Left + LBounds.Width <= GCanvas.Camera.ViewportWidth + 1) and
    (LBounds.Top + LBounds.Height <= GCanvas.Camera.ViewportHeight + 1),
    'revealed operation has complete allocated viewport bounds');
  Check(Supports(GTools, INyxToolCanvasConnectorPresentation, LPanelMode), 'navigation retains the ordinary tools placement contract');
  LPanel := LPanelMode.PipelineBounds;
  Check(not ((LBounds.Left < LPanel.Left + LPanel.Width) and
    (LBounds.Left + LBounds.Width > LPanel.Left) and
    (LBounds.Top < LPanel.Top + LPanel.Height) and
    (LBounds.Top + LBounds.Height > LPanel.Top)), 'revealed operation avoids floating tools');
end;

procedure CheckAllocation;
{$ifndef PAS2JS}
var
  LSurface: TControl;
  LWorkspace: TWinControl;
  LOrigin: TPoint;
{$else}
var
  LSurface: TJSDOMRect;
  LWorkspace: TJSDOMRect;
{$endif}
begin
  {$ifdef PAS2JS}
  LSurface := GCanvas.Surface.getBoundingClientRect;
  LWorkspace := TJSHTMLElement(GCanvas.Surface.parentElement.parentElement).getBoundingClientRect;
  Check((LSurface.top >= LWorkspace.top - 1) and
    (LSurface.bottom <= LWorkspace.bottom + 1),
    'actual browser surface stays inside its allocated workspace');
  {$else}
  LSurface := GCanvas.Surface;
  LWorkspace := LSurface.Parent.Parent;
  LOrigin := LWorkspace.ScreenToClient(LSurface.ClientToScreen(Types.Point(0, 0)));
  Check((LOrigin.Y >= 0) and (LOrigin.Y + LSurface.Height <= LWorkspace.ClientHeight),
    'actual native surface stays inside its allocated workspace');
  {$endif}
end;

{ Exercise the same actual operation faces under an opted-in uniform scene.
  Old navigation cases below keep their fixed-size semantics and assertions. }
procedure UniformStep;
const
  CZooms: array[0..4] of Double = (1, 0.5, 1.5, 0.5, 1);
var
  LBounds: TNyxCanvasBounds;
  LOther: TNyxCanvasBounds;
  LZoom: Double;
  LWidth: Double;
  LFace: {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif};
  LScaling: INyxCanvasScalingView;
  {$ifndef PAS2JS}
  LHeader: TWinControl;
  LPositions: TNyxDataValue;
  LPosition: TNyxDataValue;
  LIndex: Integer;
  LFound: Boolean;
  {$endif}
begin

  if not Idle then
  begin
    Exit;
  end;
  case GStage of
    0:
      begin
        GGraph.ShowGraphConnections(NyxFlowRef('Data workshop'));
        GTools.Refresh;
      end;
    1:
      begin
        Check(Supports(GCanvas, INyxCanvasScalingView, LScaling) and
          (LScaling.ControlScaling = nccsUniform), 'mounted view exposes the selected typed scaling policy');
        GNav.ConfigureNavigation(NyxCanvasNavigationOptions, ntctCompact);
        GNodes.MovePipelineNode(NyxFlowStep('read'), NyxCanvasPoint(0, 0));
        GNodes.MovePipelineNode(NyxFlowStep('pretty'), NyxCanvasPoint(300, 0));
        GPositions := GNodes.ExportPipelineNodePositions.ToJSON;
        GFlowRevision := GFlows.FlowRevision;
        GCanvas.SetCamera(GCanvas.Camera.AtCenter(NyxCanvasPoint(290, 160), 1));
      end;
    2..6:
      begin
        LZoom := CZooms[GStage - 2];
        LBounds := GNodes.PipelineNodeBounds(NyxFlowStep('read'));
        LOther := GNodes.PipelineNodeBounds(NyxFlowStep('pretty'));
        LFace := GNodeFaces.PipelineNodeRenderer(NyxFlowStep('read'));
        Check((LFace <> nil) and LBounds.Defined and LOther.Defined,
          'uniform operation faces are actually mounted');

        if GStage = 2 then
        begin
          GUniformFace := LFace;
          GUniformWidth := LBounds.Width;
          GUniformHeight := LBounds.Height;
          GUniformHeader := NyxControl(LFace.Root.Part(NyxPart('move')).ID);
          {$ifdef PAS2JS}
          GUniformHeaderWidth := LFace.ElementFor(GUniformHeader.ID).getBoundingClientRect.width;
          {$else}
          GUniformHeaderWidth := LFace.ControlFor(GUniformHeader.ID).Width;
          GUniformFont := TControlAccess(LFace.ControlFor(GUniformHeader.ID)).Font.Height;
          {$endif}
        end;
        Check(LFace = GUniformFace, 'camera zoom retains the same operation renderer');
        Check((Abs(LBounds.Width - GUniformWidth * LZoom) < 2) and
          (Abs(LBounds.Height - GUniformHeight * LZoom) < 2),
          'operation face extents scale with the same camera as their world positions');
        Check(LBounds.Left + LBounds.Width <= LOther.Left + 1,
          'separated world-space operation cards remain separated after zoom');
        {$ifdef PAS2JS}
        LWidth := LFace.ElementFor(GUniformHeader.ID).getBoundingClientRect.width;
        {$else}
        LWidth := LFace.ControlFor(GUniformHeader.ID).Width;
        Check(Abs(TControlAccess(LFace.ControlFor(GUniformHeader.ID)).Font.Height -
          Round(GUniformFont * LZoom)) <= 1, 'native operation typography follows uniform zoom');
        {$endif}
        Check(Abs(LWidth - GUniformHeaderWidth * LZoom) < 2,
          'the actual operation move header scales with its face');

        if GStage < 6 then
        begin
          GCanvas.SetCamera(GCanvas.Camera.AtCenter(NyxCanvasPoint(290, 160), CZooms[GStage - 1]));
        end
        else
        begin
          GFinalFit := GNav.FitContent;
        end;
      end;
    7:
      begin
        Check(GFinalFit.Status = ncnsComplete, 'uniform operation extents admit complete Fit');
        CheckVisible(NyxFlowStep('read'));
        CheckVisible(NyxFlowStep('pretty'));
        Check((GPositions = GNodes.ExportPipelineNodePositions.ToJSON) and
          (GFlows.FlowRevision = GFlowRevision) and (TNyxCodec.Encode(GDocument) = GBefore),
          'uniform projection/navigation retains exact graph, world positions and authored bytes');
        { Mixed scenes also contain a distant embedded control. Fit legitimately
          centers that larger union; return to the operation pair before testing
          header input so the selected HWND/DOM element is actually visible. }
        GCanvas.SetCamera(GCanvas.Camera.AtCenter(NyxCanvasPoint(290, 160), 0.5));
      end;
    8:
      begin
        {$ifndef PAS2JS}
        LFace := GNodeFaces.PipelineNodeRenderer(NyxFlowStep('read'));
        LHeader := TWinControl(LFace.ControlFor(GUniformHeader.ID));
        LHeader.SetFocus;
        { Ordinary native input moves 8x6 viewport pixels at half zoom. The
          public world position must advance exactly16x12, not twice that. }
        Windows.SendMessage(LHeader.Handle, WM_LBUTTONDOWN, MK_LBUTTON, LPARAM(4 shl 16 or 4));
        Windows.SendMessage(LHeader.Handle, WM_MOUSEMOVE, MK_LBUTTON, LPARAM(10 shl 16 or 12));
        Windows.SendMessage(LHeader.Handle, WM_LBUTTONUP, 0, LPARAM(10 shl 16 or 12));
        LPositions := GNodes.ExportPipelineNodePositions.Field('nodes');
        LFound := False;
        for LIndex := 0 to LPositions.Count - 1 do
        begin
          LPosition := LPositions.Item(LIndex);

          if LPosition.Field('step').AsText = 'read' then
          begin
            Check((Abs(LPosition.Field('x').AsNumber - 16) < 0.01) and
              (Abs(LPosition.Field('y').AsNumber - 12) < 0.01),
              'actual scaled-header pointer input moves the intended world distance');
            LFound := True;
          end;
        end;
        Check(LFound, 'the dragged operation retains its public typed identity');
        GNodes.MovePipelineNode(NyxFlowStep('read'), NyxCanvasPoint(0, 0));
        {$endif}
      end;
    9:
      begin
        Check((GPositions = GNodes.ExportPipelineNodePositions.ToJSON) and
          (GFlows.FlowRevision = GFlowRevision) and (TNyxCodec.Encode(GDocument) = GBefore),
          'input recovery preserves authored source and restored world placement');
        GUniformFace := nil;
        GDone := True;
      end;
  end;
  Inc(GStage);
end;

procedure Step;
var
  LResult: TNyxCanvasNavigationResult;
  LCamera: TNyxCanvasCamera;
  LInput: {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif};
  LFailed: Boolean;
begin

  if GUniform then
  begin
    UniformStep;
    Exit;
  end;

  if not Idle then
  begin
    Exit;
  end;
  {$ifndef PAS2JS}

  if GFailure.Message <> '' then
  begin
    raise Exception.Create(GFailure.Message);
  end;
  WriteLn('Navigation stage ', GStage);
  Flush(Output);
  {$endif}
  case GStage of
    0:
      begin
        GGraph.ShowGraphConnections(NyxFlowRef('Data workshop'));
        GTools.Refresh;
      end;
    1:
      begin
        Check(GNodes.PipelineNodesVisible, 'named graph supplies live operation cards');
        GNav.ConfigureNavigation(NyxCanvasNavigationOptions.Inset(16), ntctAutomatic);
        GNodes.MovePipelineNode(NyxFlowStep('read'), NyxCanvasPoint(0, 0));
        GNodes.MovePipelineNode(NyxFlowStep('pretty'), NyxCanvasPoint(8000, 0));
        GPositions := GNodes.ExportPipelineNodePositions.ToJSON;
        GFlowRevision := GFlows.FlowRevision;
        GCanvas.Execute(nccFit);
      end;
    2:
      begin
        Check(GNav.ToolsExpanded <> GCompact, 'automatic tools adapt to the actual allocation');
        CheckAllocation;
        {$ifndef PAS2JS}CheckWorker;{$endif}
        CheckCaptureGuard;
        Check(GNodes.PipelineNodeBounds(NyxFlowStep('read')).Defined and
          GNodes.PipelineNodeBounds(NyxFlowStep('pretty')).Defined,
          'ordinary canvas Fit includes far operation overlays on empty or mixed scenes');

        if GMixed then
        begin
          GInput := GCanvas.InputFor(NyxCanvasItem('Notes'), NyxControl('working-input'));
          {$ifdef PAS2JS}
          TJSHTMLInputElement(GInput).value := 'Still editing';
          GInput.dispatchEvent(TJSEvent.new('input'));
          {$else}
          TCustomEdit(GInput).Text := 'Still editing';
          {$endif}
          GNav.FitContent;
          LInput := GCanvas.InputFor(NyxCanvasItem('Notes'), NyxControl('working-input'));
          Check((LInput = GInput) and
            ({$ifdef PAS2JS}TJSHTMLInputElement(LInput).value{$else}TCustomEdit(LInput).Text{$endif} = 'Still editing'),
            'Fit retains the same allocated editor and its unfinished text');
        end;
        LResult := GNav.RevealOperation(NyxFlowStep('pretty'));
        Check(LResult.Status = ncnsComplete, 'typed named reveal produces a complete usable camera proposal');
      end;
    3:
      begin
        CheckVisible(NyxFlowStep('pretty'));
        LResult := GNav.RevealOperation(NyxFlowStep('read'));
        Check(LResult.Status = ncnsComplete, 'the other operation can be revealed independently');
      end;
    4:
      begin
        CheckVisible(NyxFlowStep('read'));
        Check((GPositions = GNodes.ExportPipelineNodePositions.ToJSON) and
          (GFlows.FlowRevision = GFlowRevision) and (TNyxCodec.Encode(GDocument) = GBefore),
          'navigation preserves every world position, graph generation and authored document byte');
        LCamera := GCanvas.Camera;
        LFailed := False;
        try
          GNav.RevealOperation(NyxFlowStep('missing'));
        except
          on E: Exception do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and (GCanvas.Camera.WorldCenter.X = LCamera.WorldCenter.X) and
          (GCanvas.Camera.WorldCenter.Y = LCamera.WorldCenter.Y), 'missing operation refuses before moving the camera');
        Press(NavID('tools'));
      end;
    5:
      begin
        Check(GNav.ToolsExpanded = GCompact, 'the tools button permits an explicit expansion or collapse');
        Press(NavID('tools'));
      end;
    6:
      begin
        Check(GNav.ToolsExpanded <> GCompact, 'tools can return to their previous presentation');

        if GCompact then
        begin
          Press(NavID('tools'));
        end;
      end;
    7:
      begin
        Press(NavID('fit'));
      end;
    8:
      begin
        Check((GPositions = GNodes.ExportPipelineNodePositions.ToJSON) and
          (GFlows.FlowRevision = GFlowRevision), 'deferred actual button navigation changes no graph/layout intent');
        GNav.ConfigureNavigation(NyxCanvasNavigationOptions, ntctAutomatic);
      end;
    9:
      begin

        if GCompact then
        begin
          Press(NavID('tools'));
        end;
      end;
    10:
      begin
        ChooseOperation('pretty');
        Press(NavID('reveal'));
      end;
    11:
      begin
        CheckVisible(NyxFlowStep('pretty'));
        GNav.ConfigureNavigation(NyxCanvasNavigationOptions, ntctCompact);
      end;
    12:
      begin
        Check(not GNav.ToolsExpanded, 'explicit compact policy remains available at every viewport');
        GNav.ConfigureNavigation(NyxCanvasNavigationOptions, ntctExpanded);
      end;
    13:
      begin
        Check(GNav.ToolsExpanded, 'caller can explicitly keep tools expanded');
        GNav.ConfigureNavigation(NyxCanvasNavigationOptions.Chrome(ncncIgnore), ntctCompact);
      end;
    14:
      begin
        LResult := GNav.RevealOperation(NyxFlowStep('pretty'));
        Check(LResult.Status = ncnsComplete, 'explicit chrome override remains caller-controlled');
      end;
    15:
      begin
        GNav.ConfigureNavigation(NyxCanvasNavigationOptions, ntctAutomatic);
        GFinalFit := GNav.FitContent;
      end;
    16:
      begin

        if GCompact then
        begin
          Check(GFinalFit.Status = ncnsLimited,
            'crowded readable cards report limited Fit instead of claiming complete visibility');
        end
        else
        begin
          Check(GFinalFit.Status = ncnsComplete,
            'immediate Configure then Fit uses the planned expanded toolbar');
          CheckVisible(NyxFlowStep('read'));
          CheckVisible(NyxFlowStep('pretty'));
        end;
        Check((TNyxCodec.Encode(GDocument) = GBefore) and
          (GPositions = GNodes.ExportPipelineNodePositions.ToJSON), 'complete navigation journey keeps exact source/default/layout intent');
        if GCompact then
        begin
          Press(NavID('tools'));
        end;
      end;
    17:
      begin
        ChooseOperation('read');
        GCanvas.PanBy(500, 500);
        GPendingCamera := GCanvas.Camera;
        Press(NavID('reveal'));
        GGraph.ShowGraphConnections(NyxFlowRef('Data workshop'));
        GTools.Refresh;
      end;
    18:
      begin
        Check((GCanvas.Camera.WorldCenter.X = GPendingCamera.WorldCenter.X) and
          (GCanvas.Camera.WorldCenter.Y = GPendingCamera.WorldCenter.Y),
          'replacing a same-name graph controller revokes its queued navigation');
        Check((TNyxCodec.Encode(GDocument) = GBefore) and
          (GPositions = GNodes.ExportPipelineNodePositions.ToJSON),
          'retired button work changes no authored or staged world intent');
        GNav.ConfigureNavigation(NyxCanvasNavigationOptions, ntctAutomatic);
        GCanvas.SetCamera(GFinalFit.Camera);
      end;
    19:
      begin
        GDone := True;
      end;
  end;
  Inc(GStage);
end;

procedure Retire;
begin
  GInput := nil;
  GNav := nil;
  GFlows := nil;
  GNodes := nil;
  GMode := nil;
  GGraph := nil;
  GWires := nil;
  GNodeFaces := nil;
  GTools := nil;
  GCanvas := nil;
  FreeAndNil(GApp);
  FreeAndNil(GDocument);
  {$ifndef PAS2JS}
  Application.OnException := nil;
  FreeAndNil(GFailure);
  {$endif}
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if TJSDate.now - GStarted > 180000 then
    begin
      raise Exception.Create('Browser navigation input deadline');
    end;
    Step;

    if GDone then
    begin
      document.body.setAttribute('data-tool-navigation', 'passed');
      document.body.setAttribute('data-tool-navigation-checks', IntToStr(GChecks));

      if Pos('review', window.location.search) = 0 then
      begin
        Retire;
      end;
    end
    else
    begin
      window.setTimeout(@Drive, 25);
    end;
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-tool-navigation', 'failed');
      document.body.setAttribute('data-tool-navigation-error', E.Message);
      Retire;
    end;
  end;
end;
{$endif}

var
  LMode: INyxToolCanvasControlConnectors;
  LGraph: INyxToolCanvasGraphConnections;
  LNodes: INyxToolCanvasPipelineNodes;
  LNav: INyxToolCanvasNavigation;
  LFlows: INyxCanvasFlowEditing;
  {$ifdef PAS2JS}
  LWires: INyxBrowserToolCanvasConnectors;
  LNodeFaces: INyxBrowserToolPipelineNodes;
  {$else}
  LWires: INyxLCLToolCanvasConnectors;
  LNodeFaces: INyxLCLToolPipelineNodes;
  LBitmap: Graphics.TBitmap;
  LPNG: TPortableNetworkGraphic;
  LTurn: Integer;
  {$endif}
begin
  try
    {$ifdef PAS2JS}
    GCompact := window.innerWidth <= 500;
    GMixed := Pos('empty', window.location.search) = 0;
    GUniform := Pos('uniform', window.location.search) > 0;
    {$else}
    Check((ParamCount = 3) or ((ParamCount = 4) and (ParamStr(4) = 'uniform')),
      'supply capture path, viewport choice, mixed/empty choice and optional uniform policy');
    GCompact := ParamStr(2) = 'compact';
    GMixed := ParamStr(3) = 'mixed';
    GUniform := ParamStr(4) = 'uniform';
    Application.Initialize;
    GFailure := TFailure.Create;
    Application.OnException := GFailure.Failed;
    {$endif}
    GDocument := DocumentForNavigation;
    GBefore := TNyxCodec.Encode(GDocument);
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GHost);
    GApp := TNyxBrowserApplication.Create;
    GApp.Run(GDocument, GHost);
    {$else}
    GApp := TNyxLCLApplication.Create;
    GApp.Mount(GDocument);
    GApp.Window.SetBounds(0, 0, 1300, 1000);

    if GCompact then
    begin
      GApp.Window.ClientWidth := 430;
      GApp.Window.ClientHeight := 760;
    end;
    GApp.Window.Show;
    {$endif}
    GTools := GApp.View.ToolCanvasView('nav-tools');
    GCanvas := GApp.View.CanvasView(GDocument.Find('nav-tools').Part(NyxPart('workspace')).ID) as
      {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};
    Check(Supports(GTools, INyxToolCanvasNavigation, LNav) and
      Supports(GTools, INyxToolCanvasPipelineNodes, LNodes) and
      Supports(GTools.Inspector, INyxToolCanvasControlConnectors, LMode) and
      Supports(GTools.Inspector, INyxToolCanvasGraphConnections, LGraph) and
      Supports(GCanvas, INyxCanvasFlowEditing, LFlows), 'actual target supplies the public navigation/node owners');
    GNav := LNav;
    GNodes := LNodes;
    GMode := LMode;
    GGraph := LGraph;
    GFlows := LFlows;
    Check(Supports(GTools, {$ifdef PAS2JS}INyxBrowserToolCanvasConnectors{$else}INyxLCLToolCanvasConnectors{$endif}, LWires) and
      Supports(GTools, {$ifdef PAS2JS}INyxBrowserToolPipelineNodes{$else}INyxLCLToolPipelineNodes{$endif}, LNodeFaces),
      'target supplies actual connector input faces');
    GWires := LWires;
    GNodeFaces := LNodeFaces;
    {$ifdef PAS2JS}
    GStarted := TJSDate.now;
    Drive;
    {$else}
    GStarted := GetTickCount64;
    while not GDone do
    begin
      CheckSynchronize;
      Application.ProcessMessages;

      if GetTickCount64 - GStarted > 180000 then
      begin
        raise Exception.Create('Native navigation deadline at stage ' + IntToStr(GStage));
      end;
      Step;
      Sleep(10);
    end;
    LBitmap := GApp.Window.GetFormImage;
    LPNG := TPortableNetworkGraphic.Create;
    try
      LPNG.Assign(LBitmap);
      LPNG.SaveToFile(ParamStr(1));
    finally
      LPNG.Free;
      LBitmap.Free;
    end;
    Retire;
    for LTurn := 1 to 5 do
    begin
      CheckSynchronize;
      Application.ProcessMessages;
    end;
    WriteLn('PASS ', GChecks, ' actual Tool Canvas navigation checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-tool-navigation', 'failed');
      document.body.setAttribute('data-tool-navigation-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      Retire;
      {$ifndef PAS2JS}
      for LTurn := 1 to 5 do
      begin
        CheckSynchronize;
        Application.ProcessMessages;
      end;
      {$endif}
    end;
  end;
end.
