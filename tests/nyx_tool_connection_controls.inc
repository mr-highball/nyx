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

{$ifndef PAS2JS}
type
  { Test-only access to real mounted LCL gesture slots. Controls and callback
    receivers remain owned by the renderer; this class never frees them. }
  TToolControlAccess = class(TControl);
{$endif}

var
  GConnectionEditor: INyxToolConnectionEditor;
  GFlowEditing: INyxCanvasFlowEditing;
  GFormerRoutes: INyxFlowRouteBinding;
  GConnectionChecks: Integer;
  GWireBefore: Integer;
  GWireSource: {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif};
  GWireStaticStart: Integer;
  {$ifdef PAS2JS}
  GKeyboardRequest: Integer;
  GDragRequest: Integer;
  {$endif}
  GUnrelated: INyxFlowRouteBinding;

function WireEndpoint(const AStep, APort: TNyxText): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep(AStep), NyxFlowPort(APort));
end;

procedure SeedConnections;
var
  LContract: TNyxProgramContract;
  LFlows: INyxFlowDeclarations;
begin
  LContract := NyxProgramContract.Input(NyxProgramSymbol('Input'), NyxProgramType(npkText))
    .Output(NyxProgramSymbol('Caption'), NyxProgramType(npkText));
  GDocument.Programs.Define(NyxProgramRef('Connection caption'),
    NewNyxProgramDefinition('begin Caption := ''Connected: '' + Input end.', LContract));
  LFlows := EditNyxFlows(GDocument.Flows, [
    NyxAddFlowStep(NyxFlowRef('Profile'), NyxFlowStep('script'),
      NewNyxProgramLogic(NyxProgramRef('Connection caption'), LContract)),
    NyxConnectFlow(NyxFlowRef('Profile'), WireEndpoint('pretty', 'text'),
      WireEndpoint('script', 'Input')),
    NyxAddFlowStep(NyxFlowRef('Profile'), NyxFlowStep('choose'),
      NewNyxFlowJoin(NyxFlowJoinOptions(nftText).Input(NyxFlowPort('primary'))
        .Input(NyxFlowPort('fallback')).Select(nfjFirstAvailable)))]);
  GDocument.Flows.Define(NyxFlowRef('Profile'), LFlows.Definition(NyxFlowRef('Profile')));
  GDocument.State.SetValue(NyxTextState('unrelated'), 'Not started');
  GDocument.Flows.Define(NyxFlowRef('Other pipeline'), NewNyxFlowGraph
    .WithStep(NyxFlowStep('join'), NewNyxTextJoin(' ')).Build);
  GDocument.Find('output-template').Add(NewNyxButton('unrelated-run').WithText('Keep working'));
  SetNyxFlowRoutes(GDocument.Find('unrelated-run'), NewNyxFlowRoutes.WithRoute(
    NyxFlowRoute(NyxFlowRouteID('Other work'), NyxFlowRef('Other pipeline'), ntClick, nfcmExclusive)
      .InputConstant(WireEndpoint('join', 'left'), NyxFlowText('Unaffected'))
      .InputConstant(WireEndpoint('join', 'right'), NyxFlowText('work'))
      .OutputState(WireEndpoint('join', 'text'), NyxTextState('unrelated'))));
  GDocument.Validate;
end;

function ConnectionUIIdle: Boolean;
begin
  Result := (GTools.RefreshExecution = nil) or
    not (GTools.RefreshExecution.Status in [nesPending, nesRunning]);

  if Result and (GTools.RefreshExecution <> nil) then
  begin

    if GTools.RefreshExecution.Status <> nesSucceeded then
    begin
      raise Exception.Create('Connection form refresh failed: ' + GTools.RefreshExecution.Failure);
    end;
  end;
  {$ifdef PAS2JS}
  Result := Result and ((GKeyboardRequest = 0) or
    (document.body.getAttribute('data-tool-key-ack') = IntToStr(GKeyboardRequest))) and
    ((GDragRequest = 0) or
    (document.body.getAttribute('data-tool-drag-ack') = IntToStr(GDragRequest)));
  {$endif}
end;

procedure WireActivate(const ASuffix: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}
  LTarget: INyxBrowserToolCanvasView;
  LButton: TJSHTMLElement;
  {$else}
  LTarget: INyxLCLToolCanvasView;
  LButton: TWinControl;
  {$endif}
begin
  LID := GTools.Inspector.Root.ID + '-connections-' + ASuffix;
  {$ifdef PAS2JS}
  Supports(GTools, INyxBrowserToolCanvasView, LTarget);
  LButton := LTarget.Renderer.ElementFor(LID);
  Check(LButton <> nil, 'Actual browser connection button exists / ' + ASuffix);
  LButton.scrollIntoView;
  LButton.focus;
  Inc(GKeyboardRequest);
  document.body.setAttribute('data-tool-key', IntToStr(GKeyboardRequest));
  {$else}
  Supports(GTools, INyxLCLToolCanvasView, LTarget);
  LButton := TWinControl(LTarget.Renderer.ControlFor(LID));
  Check((LButton <> nil) and LButton.CanFocus,
    'Actual native connection button accepts host keys / ' + ASuffix);
  LButton.SetFocus;
  Windows.SendMessage(LButton.Handle, WM_KEYDOWN, VK_SPACE, 1);
  Windows.SendMessage(LButton.Handle, WM_KEYUP, VK_SPACE, 1);
  {$endif}
end;

procedure WireConfirm;
var
  LID: TNyxText;
  {$ifdef PAS2JS}
  LTarget: INyxBrowserToolCanvasView;
  {$else}
  LTarget: INyxLCLToolCanvasView;
  {$endif}
begin
  LID := GTools.Inspector.Root.ID + '-connections-confirm';
  {$ifdef PAS2JS}
  Supports(GTools, INyxBrowserToolCanvasView, LTarget);
  TJSHTMLInputElement(LTarget.Renderer.InputFor(LID)).click;
  {$else}
  Supports(GTools, INyxLCLToolCanvasView, LTarget);
  TCheckBox(LTarget.Renderer.InputFor(LID)).Checked := True;
  {$endif}
end;

procedure ConnectionGuards;
var
  LAccess: INyxToolCanvasConnections;
  LBefore: TNyxText;
  LStateRevision: Integer;
  LSceneRevision: Integer;
  LLayout: INyxCanvasLayoutView;
  LPresentation: Integer;
  LEditing: INyxCanvasFlowEditing;
  LFlowRevision: Integer;
  LFailed: Boolean;
  LCase: Integer;
begin
  { The baseline navigates twice. Obtain this generation's static context from
    an actual current button, rather than reusing its deliberately retired one. }
  ClickRun;
  GWireStaticStart := GStaticCalls;
  Check(GContext.Active, 'Current static callback context is active before rewiring');
  Check(Supports(GTools.Inspector, INyxToolCanvasConnections, LAccess),
    'Public Tool Canvas exposes optional typed connection editing');
  GConnectionEditor := LAccess.ConnectionEditor;
  Check(Supports(GView, INyxCanvasFlowEditing, LEditing),
    'Mounted workspace exposes revision-aware graph admission');
  GFlowEditing := LEditing;
  GConnectionEditor.SelectFlow(NyxFlowRef('Profile'));
  Supports(GView, INyxCanvasLayoutView, LLayout);
  LBefore := GFlowEditing.RuntimeFlows.ToData.ToJSON;
  LStateRevision := GApp.State.Revision;
  LSceneRevision := GScene.Revision;
  LPresentation := LLayout.PresentationRevision;
  GFormerRoutes := GBehavior.Routes;
  GWireSource := GView.InputFor(NyxCanvasItem('Editor'), NyxControl('source-memo'));
  for LCase := 0 to 3 do
  begin
    LFailed := False;
    try
      case LCase of
        0: GConnectionEditor.Apply(WireEndpoint('pretty', 'text'),
          WireEndpoint('summary', 'right'), ntcaConnect);
        1: GConnectionEditor.Apply(WireEndpoint('parse', 'json'),
          WireEndpoint('choose', 'primary'), ntcaConnect);
        2: GConnectionEditor.Apply(WireEndpoint('pretty', 'text'),
          WireEndpoint('summary', 'right'), ntcaReplace, False);
        3: GConnectionEditor.Apply(WireEndpoint('script', 'Caption'),
          WireEndpoint('script', 'Input'), ntcaReplace, True);
      end;
    except
      on E: ENyxFlow do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (GFlowEditing.RuntimeFlows.ToData.ToJSON = LBefore) and
      (GApp.State.Revision = LStateRevision) and (GScene.Revision = LSceneRevision) and
      (LLayout.PresentationRevision = LPresentation) and (GBehavior.Routes = GFormerRoutes),
      'Refused type/occupied/unconfirmed/cyclic connection retains all owners / ' +
      TNyxText(IntToStr(LCase)));
  end;
  LFlowRevision := GFlowEditing.FlowRevision;
  GBehavior.EnableGraphs(False);
  GFlowEditing.ApplyFlowChanges([NyxDefineFlow(NyxFlowRef('Profile'),
    GFlowEditing.RuntimeFlows.Definition(NyxFlowRef('Profile')))],
    GScene.Revision, LFlowRevision);
  Check(not GBehavior.GraphsEnabled and GContext.Active,
    'Prepared connection replacement retains paused graph policy and independent static callbacks');
  GBehavior.EnableGraphs(True);
  LFailed := False;
  try
    GConnectionEditor.Apply(WireEndpoint('pretty', 'text'),
      WireEndpoint('choose', 'fallback'), ntcaConnect);
  except
    on E: ENyxCanvasScene do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GFlowEditing.RuntimeFlows.ToData.ToJSON = LBefore) and
    (GFlowEditing.FlowRevision = LFlowRevision + 1),
    'Same bytes at a newer graph revision still refuse an old wire selection');
  GConnectionEditor.SelectFlow(NyxFlowRef('Profile'));
  GFormerRoutes := GBehavior.Routes;
  GUnrelated := GRuntime.BehaviorFor(NyxCanvasItem('Output')).Routes;
  {$ifdef PAS2JS}
  TJSHTMLButtonElement(GView.RendererFor(NyxCanvasItem('Output')).ElementFor('unrelated-run')).click;
  {$else}
  TNyxLCLButton(GView.RendererFor(NyxCanvasItem('Output')).ControlFor('unrelated-run')).Click;
  {$endif}
  GView.PanBy(2, 0);
  Check(GFlowEditing.FlowRevision = LFlowRevision + 1,
    'Ordinary panning changes no graph meaning or connection revision');
  GConnectionEditor.Apply(WireEndpoint('pretty', 'text'),
    WireEndpoint('choose', 'fallback'), ntcaConnect);
  GConnectionEditor.Apply(WireEndpoint('choose', 'value'),
    WireEndpoint('summary', 'right'), ntcaReplace, True);
  Check(GRuntime.BehaviorFor(NyxCanvasItem('Output')).Routes = GUnrelated,
    'Changing one graph preserves the unrelated fragment route owner and its pending work');
  Check((GBehavior.Routes <> GFormerRoutes) and
    GFormerRoutes.Report(0).Field('closed').AsBoolean,
    'Accepted connections retire old routes without replacing static behavior');
  Check(GView.InputFor(NyxCanvasItem('Editor'), NyxControl('source-memo')) = GWireSource,
    'Connection changes retain the exact editing face');
  Check(GRuntime.BehaviorFor(NyxCanvasItem('Editor')) = GBehavior,
    'Connection changes retain the exact item attachment');
  Check(GContext.Active, 'Connection changes retain the current static callback context');
  Check((GScene.Revision = LSceneRevision) and (GApp.State.Revision = LStateRevision) and
    (TNyxCodec.Encode(GDocument) = GBefore),
    'Runtime wiring changes no scene placement, application state or authored project');
  GWireBefore := GFlowEditing.RuntimeFlows.Definition(NyxFlowRef('Profile')).WireCount;
  GTools.Inspector.SelectControl(NyxCanvasItem('Editor'), NyxControl('source-memo'));
  LAccess.ShowConnections;
  GTools.Refresh;
end;

procedure WireDrag(ACancel: Boolean);
var
  LID: TNyxText;
  {$ifndef PAS2JS}
  LTargetView: INyxLCLToolCanvasView;
  LSource: TControl;
  LTarget: TControl;
  LDrag: TDragObject;
  LAccept: Boolean;
  {$endif}
begin
  LID := GTools.Inspector.Root.ID + '-connections-';
  {$ifdef PAS2JS}

  if not ACancel then
  begin
    Inc(GDragRequest);
    document.body.setAttribute('data-tool-drag-source', LID + 'port-12');
    document.body.setAttribute('data-tool-drag-target', LID + 'port-13');
    document.body.setAttribute('data-tool-drag', IntToStr(GDragRequest));
  end;
  {$else}
  Supports(GTools, INyxLCLToolCanvasView, LTargetView);
  LSource := LTargetView.Renderer.ControlFor(LID + 'port-12');
  LTarget := LTargetView.Renderer.ControlFor(LID + 'port-13');

  if ACancel then
  begin
    LSource.BeginDrag(True);
    try
      Check(LSource.Dragging, 'Actual native drag manager constructs the connection source');
    finally
      LSource.EndDrag(False);
    end;
    Exit;
  end;
  LDrag := nil;
  TToolControlAccess(LSource).OnStartDrag(LSource, LDrag);
  try
    Check(LDrag <> nil, 'Actual mounted native port supplies its typed drag offer');
    LAccept := False;
    TToolControlAccess(LTarget).OnDragOver(LTarget, LDrag, 8, 8, dsDragEnter, LAccept);
    Check(LAccept, 'Native compatible input accepts the offered Link operation');
    TToolControlAccess(LTarget).OnDragDrop(LTarget, LDrag, 8, 8);
    TToolControlAccess(LSource).OnEndDrag(LSource, LTarget, 8, 8);
  finally
    LDrag.Free;
  end;
  {$endif}
end;

{$ifndef PAS2JS}
{ Capture only the owned foreground form's client pixels after actual input.
  CopyRect invalidates the bitmap cache through LCL; the temporary canvas borrows
  the screen DC and never releases it outside its enclosing finally scope. }
procedure CaptureConnections;
var
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
  LCanvas: TCanvas;
  LDC: HDC;
  LOrigin: Types.TPoint;
begin

  if ParamStr(2) = '' then
  begin
    Exit;
  end;
  Windows.SetForegroundWindow(GApp.Window.Handle);
  GApp.Window.BringToFront;
  Application.ProcessMessages;
  GApp.Window.Update;

  if Windows.GetForegroundWindow <> GApp.Window.Handle then
  begin
    raise Exception.Create('Owned Tool Canvas is not foreground for capture');
  end;
  LBitmap := TBitmap.Create;
  LPNG := TPortableNetworkGraphic.Create;
  LCanvas := TCanvas.Create;
  try
    LBitmap.SetSize(GApp.Window.ClientWidth, GApp.Window.ClientHeight);
    LOrigin := GApp.Window.ClientToScreen(Types.Point(0, 0));
    LDC := Windows.GetDC(0);

    if LDC = 0 then
    begin
      raise Exception.Create('Owned Tool Canvas screen context is unavailable');
    end;
    try
      LCanvas.Handle := LDC;
      LBitmap.Canvas.CopyRect(Types.Rect(0, 0, LBitmap.Width, LBitmap.Height),
        LCanvas, Types.Rect(LOrigin.X, LOrigin.Y,
          LOrigin.X + LBitmap.Width, LOrigin.Y + LBitmap.Height));
    finally
      LCanvas.Handle := 0;
      Windows.ReleaseDC(0, LDC);
    end;
    LPNG.Assign(LBitmap);
    LPNG.SaveToFile(ParamStr(2));
  finally
    LCanvas.Free;
    LPNG.Free;
    LBitmap.Free;
  end;
end;
{$endif}

function ConnectionStep: Boolean;
var
  LFailed: Boolean;
  LFlows: INyxFlowDeclarations;
  LEditing: INyxCanvasFlowEditing;
begin
  Result := False;

  if not ConnectionUIIdle then
  begin
    Exit;
  end;
  case GStage of
    7:
      begin
        GConnectionChecks := GChecks;
        ConnectionGuards;
      end;
    8:
      begin
        Check(GApp.State.GetValue(NyxTextState('unrelated')) = 'Unaffected work',
          'Unrelated in-flight work still publishes after another graph is rewired');
        WireActivate('next');
      end;
    9: WireActivate('port-12');
    10: WireActivate('port-13');
    11: WireActivate('connect');
    12:
      begin
        Check(GFlowEditing.RuntimeFlows.Definition(NyxFlowRef('Profile')).WireCount = GWireBefore + 1,
          'Actual host keyboard port choices create the typed script-to-branch wire / ' +
          GTools.Inspector.Root.Node.Find('tool-port-inspector-connections-selected').Prop(NyxAttributeName(atText)) +
          ' / ' + GTools.Inspector.Root.Node.Find('tool-port-inspector-connections-message').Prop(NyxAttributeName(atText)));
        Edit('{"profile":{"name":"A connected script"}}');
        ClickRun;
      end;
    13:
      begin

        if not Idle then
        begin
          Exit;
        end;
        Check(Pos('Connected:', GApp.State.GetValue(NyxTextState('summary'))) > 0,
          'Interpreted script output reaches an ordinary control through the new wire');
        Check((GView.InputFor(NyxCanvasItem('Editor'), NyxControl('source-memo')) = GWireSource) and
          (TNyxCodec.Encode(GDocument) = GBefore),
          'Useful live rewiring retains the exact editing face and complete authored defaults');
        WireActivate('next');
      end;
    14: WireActivate('port-12');
    15: WireActivate('port-13');
    16: WireActivate('remove');
    17:
      begin
        Check(GFlowEditing.RuntimeFlows.Definition(NyxFlowRef('Profile')).WireCount = GWireBefore + 1,
          'Actual removal refuses without warning acknowledgement');
        WireConfirm;
      end;
    18: WireActivate('remove');
    19:
      begin
        Check(GFlowEditing.RuntimeFlows.Definition(NyxFlowRef('Profile')).WireCount = GWireBefore,
          'Warned keyboard removal retires the exact selected wire');
        Edit('{"profile":{"name":"After connection removal"}}');
        ClickRun;
      end;
    20:
      begin

        if not Idle then
        begin
          Exit;
        end;
        Check((Pos('After connection removal', GApp.State.GetValue(NyxTextState('summary'))) > 0) and
          (Pos('Connected:', GApp.State.GetValue(NyxTextState('summary'))) = 0),
          'Removal changes real execution to the explicit fallback branch');
        Check((GStaticCalls = GWireStaticStart + 2) and (TNyxCodec.Encode(GDocument) = GBefore),
          'Ordinary static registrations and authored bytes survive every connection edit');
        WireActivate('next');
      end;
    21: WireDrag(True);
    22:
      begin
        Check(GFlowEditing.RuntimeFlows.Definition(NyxFlowRef('Profile')).WireCount = GWireBefore,
          'Cancelled native manager drag creates no connection');
        WireDrag(False);
      end;
    23:
      begin
        Check(GFlowEditing.RuntimeFlows.Definition(NyxFlowRef('Profile')).WireCount = GWireBefore + 1,
          'Real browser host/native gesture slots admit the exact typed drag connection');
        Edit('{"profile":{"name":"A dragged connection"}}');
        ClickRun;
      end;
    24:
      begin

        if not Idle then
        begin
          Exit;
        end;
        {$ifdef PAS2JS}

        if document.body.getAttribute('data-tool-capture-ack') <> 'connected' then
        begin
          document.body.setAttribute('data-tool-capture', 'connected');
          Exit;
        end;
        {$else}
        CaptureConnections;
        {$endif}
        Check((Pos('Connected:', GApp.State.GetValue(NyxTextState('summary'))) > 0) and
          (Pos('A dragged connection', GApp.State.GetValue(NyxTextState('summary'))) > 0),
          'Dragged script wiring produces useful actual control output');
        GApp.ShowPage('other');
        Check(not GContext.Active, 'Navigating retires the rewired static application context');
      end;
    25:
      begin
        { Keep the old editor interface deliberately: it must refuse before any
          borrowed store access, even after a different view becomes active. }
        GApp.ShowPage('home');
        ConnectView;
        LFailed := False;
        LFlows := nil;
        try
          LFlows := GFlowEditing.RuntimeFlows;
        except
          on E: ENyxCanvasScene do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and (LFlows = nil), 'Retained old flow access cannot address the new view');
        LFailed := False;
        try
          GConnectionEditor.Apply(WireEndpoint('script', 'Caption'),
            WireEndpoint('choose', 'primary'), ntcaConnect);
        except
          on E: ENyxCanvasScene do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed, 'Retired connection editor refuses before borrowed application access');
        Supports(GView, INyxCanvasFlowEditing, LEditing);
        Check((LEditing.RuntimeFlows.ToData.ToJSON = GDocument.Flows.ToData.ToJSON) and
          (LEditing.FlowRevision = 0), 'Independent fresh view reconstructs exact authored graph defaults');
        Edit('{"profile":{"name":"Fresh original wiring"}}');
        ClickRun;
      end;
    26:
      begin

        if not Idle then
        begin
          Exit;
        end;
        Check((Pos('Fresh original wiring', GApp.State.GetValue(NyxTextState('summary'))) > 0) and
          (Pos('Connected:', GApp.State.GetValue(NyxTextState('summary'))) = 0) and GContext.Active,
          'Fresh ordinary routes and callbacks execute independently after rewired view retirement');
        Check(TNyxCodec.Encode(GDocument) = GBefore, 'Complete connection journey preserves every authored byte');
        {$ifdef PAS2JS}
        document.body.setAttribute('data-tool-connection-checks', IntToStr(GChecks - GConnectionChecks));
        {$else}
        WriteLn('PASS ', GChecks - GConnectionChecks, ' actual Tool Canvas connection checks');
        {$endif}
        GFinished := True;
      end;
  end;
  Result := True;
end;
