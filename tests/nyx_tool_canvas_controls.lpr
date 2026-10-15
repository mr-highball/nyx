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


program nyx_tool_canvas_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec, nyx.state,
  nyx.events, nyx.callbacks, nyx.behavior, nyx.scheduler, nyx.application.context,
  nyx.canvas.types, nyx.canvas.geometry, nyx.canvas.scene, nyx.canvas.workspace,
  nyx.view.behavior, nyx.test.tool.canvas, nyx.tool.canvas, nyx.tool.ports, nyx.binding.types
  {$ifdef NYX_TOOL_CONNECTION_TESTS}, nyx.tool.connections, nyx.flow, nyx.flow.types,
    nyx.flow.editing, nyx.flow.declarations, nyx.flow.routing, nyx.logic,
    nyx.logic.joins, nyx.logic.programs, nyx.programs.types, nyx.programs.contract,
    nyx.programs.declarations, nyx.flow.routes, nyx.flow.route.model,
    nyx.controls{$endif}
  {$ifdef NYX_TOOL_COMPANION}, nyx.generated.view{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser, nyx.canvas.browser, nyx.tool.canvas.browser
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, nyx.widgets.lcl,
  nyx.application.lcl, nyx.canvas.lcl, nyx.tool.canvas.lcl
    {$ifdef NYX_TOOL_CONNECTION_TESTS}, Windows, Graphics, Types{$endif}{$endif};

type
  { The ordinary callback receives the exact shared application state through a
    weak per-item context. No canvas-specific callback glue is authored. }
  TToolRunObserver = class(TNyxApplicationEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

var
  GDocument: TNyxDocument;
  GBefore: TNyxText;
  GChecks: Integer;
  GStage: Integer;
  GStaticCalls: Integer;
  GRevision: Integer;
  GFinished: Boolean;
  GSearchPending: Boolean;
  GBehavior: INyxViewBehavior;
  GContext: INyxApplicationContext;
  GRuntime: INyxCanvasRuntimeView;
  GScene: INyxCanvasScene;
  GTools: INyxToolCanvasView;
  {$ifdef NYX_TOOL_COMPANION}
  GExpected: TNyxDocument;
  {$endif}
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GHost: TJSHTMLElement;
  GView: INyxBrowserCanvasWorkspaceView;
  GStarted: Double;
  {$else}
  GApp: TNyxLCLApplication;
  GView: INyxLCLCanvasWorkspaceView;
  GStarted: QWord;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Actual Tool Canvas: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TToolRunObserver.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Inc(GStaticCalls);
  GContext := Context;
  Check(Context.State = GApp.State, 'embedded static callback sees the actual application store');
end;

procedure ConnectView;
var
  LRuntime: INyxCanvasRuntimeView;
begin
  {$ifdef PAS2JS}
  GView := GApp.View.CanvasView(GDocument.Find('tool-canvas').Part(NyxPart('workspace')).ID) as INyxBrowserCanvasWorkspaceView;
  {$else}
  GView := GApp.View.CanvasView(GDocument.Find('tool-canvas').Part(NyxPart('workspace')).ID) as INyxLCLCanvasWorkspaceView;
  {$endif}
  Check(Supports(GView, INyxCanvasRuntimeView, LRuntime), 'canvas exposes optional runtime ownership capability');
  GRuntime := LRuntime;
  GScene := GRuntime.Scene;
  GView.Refresh;
  GView.Execute(nccFit);
  Check(GView.MountedCount > 0, 'canvas items mount at an allocated viewport: scene=' +
    IntToStr(GScene.Count) + ', mounted=' + IntToStr(GView.MountedCount) +
    {$ifdef PAS2JS}', width=' + IntToStr(GView.Surface.clientWidth) + ', height=' + IntToStr(GView.Surface.clientHeight)
    {$else}', width=' + IntToStr(GView.Surface.Width) + ', height=' + IntToStr(GView.Surface.Height){$endif});
  GBehavior := GRuntime.BehaviorFor(NyxCanvasItem('Editor'));
  GTools := GApp.View.ToolCanvasView('tool-canvas');
  GTools.Inspector.SelectControl(NyxCanvasItem('Editor'), NyxControl('source-memo'));
  GTools.Refresh;
end;

procedure ClickRun;
begin
  {$ifdef PAS2JS}
  TJSHTMLButtonElement(GView.RendererFor(NyxCanvasItem('Editor')).ElementFor('run-button')).click;
  {$else}
  TNyxLCLButton(GView.RendererFor(NyxCanvasItem('Editor')).ControlFor('run-button')).Click;
  {$endif}
end;

procedure InspectorGraphs(AValue: Boolean);
var
  LID: TNyxText;
  {$ifdef PAS2JS}
  LTarget: INyxBrowserToolCanvasView;
  LInput: TJSHTMLInputElement;
  {$else}
  LTarget: INyxLCLToolCanvasView;
  {$endif}
begin
  LID := GTools.Inspector.Root.ID + '-graphs';
  {$ifdef PAS2JS}
  Check(Supports(GTools, INyxBrowserToolCanvasView, LTarget), 'public browser Tool Canvas face');
  LInput := TJSHTMLInputElement(LTarget.Renderer.InputFor(LID));

  if LInput.checked <> AValue then
  begin
    LInput.click;
  end;
  {$else}
  Check(Supports(GTools, INyxLCLToolCanvasView, LTarget), 'public native Tool Canvas face');
  TCheckBox(LTarget.Renderer.InputFor(LID)).Checked := AValue;
  {$endif}
  Check(GBehavior.GraphsEnabled = AValue, 'actual inspector switch controls only selected item graphs');
end;

procedure SearchPorts(AVerify: Boolean);
var
  LID: TNyxText;
  {$ifdef PAS2JS}
  LTarget: INyxBrowserToolCanvasView;
  LInput: TJSHTMLInputElement;
  {$else}
  LTarget: INyxLCLToolCanvasView;
  {$endif}
begin
  LID := GTools.Inspector.Root.ID;
  {$ifdef PAS2JS}
  Supports(GTools, INyxBrowserToolCanvasView, LTarget);

  if not AVerify then
  begin
    LInput := TJSHTMLInputElement(LTarget.Renderer.InputFor(LID + '-query'));
    LInput.value := 'Write existing state';
    LInput.dispatchEvent(TJSEvent.new('input'));
    TJSHTMLButtonElement(LTarget.Renderer.ElementFor(LID + '-search')).click;
  end;
  {$else}
  Supports(GTools, INyxLCLToolCanvasView, LTarget);

  if not AVerify then
  begin
    TEdit(LTarget.Renderer.InputFor(LID + '-query')).Text := 'Write existing state';
    TNyxLCLButton(LTarget.Renderer.ControlFor(LID + '-search')).Click;
  end;
  {$endif}

  if AVerify then
  begin
    Check(LTarget.Renderer.Root.Find(LID + '-count').Prop('text') =
      '1 shown / 1 matching ports', 'actual port search exposes one genuine bound state input');
  end;
end;

procedure Edit(const ASource: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLTextAreaElement(GView.InputFor(NyxCanvasItem('Editor'), NyxControl('source-memo'))).value := ASource;
  GView.InputFor(NyxCanvasItem('Editor'), NyxControl('source-memo')).dispatchEvent(TJSEvent.new('input'));
  {$else}
  TMemo(GView.InputFor(NyxCanvasItem('Editor'), NyxControl('source-memo'))).Text := ASource;
  {$endif}
end;

function OutputText: TNyxText;
begin
  {$ifdef PAS2JS}
  Result := GView.RendererFor(NyxCanvasItem('Output')).ElementFor('pretty-code').textContent;
  {$else}
  Result := TNyxText(TMemo(GView.RendererFor(NyxCanvasItem('Output')).ControlFor('pretty-code')).Text);
  {$endif}
end;

function Idle: Boolean;
var
  LIndex: Integer;
begin
  Result := True;
  for LIndex := 0 to GBehavior.Routes.Count - 1 do
  begin
    Result := Result and (GBehavior.Routes.Report(LIndex).Field('active').AsInteger = 0);
  end;
end;

{$ifdef NYX_TOOL_CONNECTION_TESTS}
{$I nyx_tool_connection_controls.inc}
{$endif}

procedure Step;
var
  LFailed: Boolean;
  LBefore: TNyxText;
  LState: TNyxState;
  LContent: INyxNode;
  LPortCount: Integer;
begin
  {$ifdef PAS2JS}
  document.body.setAttribute('data-tool-canvas-stage', IntToStr(GStage));
  {$endif}
  case GStage of
    0:
      begin
        ConnectView;
        LFailed := False;
        try
          GTools.Inspector.SelectControl(NyxCanvasItem('Editor'), NyxControl('missing-control'));
        except
          on E: ENyxModel do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed, 'explicit inspector selection refuses a missing effective control');
        LContent := GScene.Control(NyxCanvasItem('Editor'));
        LPortCount := NyxToolControlPorts(NyxCanvasItem('Editor'),
          GDocument.Find('source-memo'), GDocument).Count;
        LContent.Node.Find('source-memo').SetBinding(TNyxBindingSpec.Clear(bpValue));
        GTools.Inspector.SelectControl(NyxCanvasItem('Editor'), NyxControl('source-memo'));
        Check(Pos(IntToStr(LPortCount - 1) + ' matching ports',
          GTools.Inspector.Document.Find('tool-port-inspector-count').Prop('text')) > 0,
          'exact runtime content change invalidates the inspector writable-port snapshot');
        LContent.Node.Find('source-memo').SetBinding(TNyxBindingSpec.Bound(bpValue, 'source', nskText, bdTwoWay));
        GTools.Inspector.SelectControl(NyxCanvasItem('Editor'), NyxControl('source-memo'));
        Check((GBehavior <> nil) and (GBehavior.Routes.Count = 2),
          'automatic canvas mount binds only its editor fragment');
        Check(GApp.FlowRoutes.Count = 0, 'outer application has no phantom embedded registrations');
        GRevision := GApp.State.Revision;
        ClickRun;
      end;
    1:
      begin

        if not Idle then
        begin
          Exit;
        end;
        if (GTools.RefreshExecution <> nil) and
          (GTools.RefreshExecution.Status in [nesPending, nesRunning]) then
        begin
          Exit;
        end;
        if GSearchPending then
        begin
          Check((GTools.RefreshExecution <> nil) and
            (GTools.RefreshExecution.Status = nesSucceeded), 'deferred inspector publication succeeds');
          SearchPorts(True);
        end
        else
        begin
          SearchPorts(False);
          GSearchPending := True;
          Exit;
        end;
        Check((GStaticCalls = 1) and (GApp.State.Revision = GRevision + 1),
          'ordinary callback and ordered route run once with atomic output');
        Check(Pos('English demo', OutputText) > 0, 'sibling canvas item receives live bound graph output');
        InspectorGraphs(False);
        GRevision := GApp.State.Revision;
        ClickRun;
        Check((GStaticCalls = 2) and (GApp.State.Revision = GRevision),
          'pausing graphs leaves static callbacks enabled');
        InspectorGraphs(True);
        Edit('{"profile":{"name":"Embedded route","exact":9007199254740993}}');
      end;
    2:
      begin

        if not Idle then
        begin
          Exit;
        end;
        Check(Pos('Embedded route', OutputText) > 0, 'real embedded memo input drives the graph');
        Check(Pos('9007199254740993', OutputText) > 0, 'exact JSON number reaches a sibling renderer');
        GView.PanBy(25, 10);
        GView.ZoomAt(0.85, NyxCanvasPoint(150, 100));
        Check(GRuntime.BehaviorFor(NyxCanvasItem('Editor')) = GBehavior,
          'camera changes preserve the exact item behavior attachment');
        Edit('{"profile":{"name":"Removed item"}}');
        LBefore := GApp.State.GetValue(NyxTextState('pretty'));
        GScene.Remove(NyxCanvasItem('Editor'));
        GView.Refresh;
        Check(not GContext.Active, 'item removal revokes retained callback contexts');
        LFailed := False;
        try
          LState := GContext.State;
          Check(LState = nil, 'retired state access must refuse before returning a borrow');
        except
          on E: Exception do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed, 'retired context refuses before reaching a borrowed store');
        Check(GApp.State.GetValue(NyxTextState('pretty')) = LBefore, 'removal cannot publish pending graph results');
      end;
    3:
      begin
        Check(Pos('Removed item', GApp.State.GetValue(NyxTextState('pretty'))) = 0,
          'removed item cannot deliver later');
        Check(TNyxCodec.Encode(GDocument) = GBefore, 'runtime removal/camera/input preserve the saved project');
        GApp.ShowPage('other');
        GView := nil;
        GRuntime := nil;
        GScene := nil;
        GApp.ShowPage('home');
        ConnectView;
        Edit('{"profile":{"name":"Fresh embedded view"}}');
      end;
    4:
      begin

        if not Idle then
        begin
          Exit;
        end;
        Check(Pos('Fresh embedded view', OutputText) > 0,
          'saved content and routes reconstruct after navigation');
        Edit('{"profile":{"name":"Retired page"}}');
        GApp.ShowPage('other');
      end;
    5:
      begin
        Check(Pos('Retired page', GApp.State.GetValue(NyxTextState('pretty'))) = 0,
          'navigation cancels pending embedded output');
        GApp.ShowPage('home');
        ConnectView;
        Edit('{"profile":{"name":"Final connected tool"}}');
      end;
    6:
      begin

        if not Idle then
        begin
          Exit;
        end;
        Check(Pos('Final connected tool', OutputText) > 0, 'fresh generation remains functional');
      end;
    {$ifdef NYX_TOOL_CONNECTION_TESTS}
    7..40:
      begin

        if not ConnectionStep then
        begin
          Exit;
        end;
      end;
    {$else}
    7: GFinished := True;
    {$endif}
  end;
  Inc(GStage);
end;

procedure Retire;
begin
  {$ifdef NYX_TOOL_CONNECTION_TESTS}
  GConnectionEditor := nil;
  GFlowEditing := nil;
  GFormerRoutes := nil;
  GUnrelated := nil;
  {$endif}
  GView := nil;
  GRuntime := nil;
  GScene := nil;
  GTools := nil;
  FreeAndNil(GApp);
  GBehavior := nil;
  GContext := nil;
  FreeAndNil(GDocument);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if TJSDate.now - GStarted > {$ifdef NYX_TOOL_CONNECTION_TESTS}180000{$else}90000{$endif} then
    begin
      raise Exception.Create('Tool canvas deadline at stage ' + IntToStr(GStage));
    end;
    Step;

    if GFinished then
    begin
      { An optional separate run exercises complete host retirement. Ordinary
        captures keep the final tool mounted for visual review. }

      if Pos('retire=1', window.location.search) > 0 then
      begin
        Retire;
        Check((GApp = nil) and (GDocument = nil), 'complete browser host retirement returns safely');
      end;
      document.body.setAttribute('data-tool-canvas', 'passed');
      document.body.setAttribute('data-tool-canvas-checks', IntToStr(GChecks));
      Exit;
    end;
    window.setTimeout(@Drive, 20);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-tool-canvas', 'failed');
      document.body.setAttribute('data-tool-canvas-error', E.Message);
      try
        Retire;
      except
        on LCleanupError: Exception do
        begin
          document.body.setAttribute('data-tool-canvas-cleanup-error', LCleanupError.Message);
        end;
      else
        document.body.setAttribute('data-tool-canvas-cleanup-error', 'Native host cleanup exception');
      end;
    end;
  end;
end;
{$endif}

begin
  try
    RegisterNyxCallback(NyxHandler('TToolRunObserver'), TToolRunObserver);
    {$ifdef NYX_TOOL_COMPANION}
    GDocument := BuildNyxDocument;
    GExpected := ToolCanvasDocument;
    try
      Check(TNyxCodec.Encode(GDocument) = TNyxCodec.Encode(GExpected),
        'actual application constructs the complete exact generated companion');
    finally
      FreeAndNil(GExpected);
    end;
    {$else}
    GDocument := ToolCanvasDocument;
    {$endif}
    {$ifdef NYX_TOOL_CONNECTION_TESTS}SeedConnections;{$endif}
    GBefore := TNyxCodec.Encode(GDocument);
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GHost);
    GApp := TNyxBrowserApplication.Create;
    GApp.Run(GDocument, GHost);
    {$ifdef NYX_TOOL_CONNECTION_TESTS}

    if Pos('connections-only=1', window.location.search) > 0 then
    begin
      ConnectView;
      GStage := 7;
    end;
    {$endif}
    GStarted := TJSDate.now;
    Drive;
    {$else}
    Application.Initialize;
    GApp := TNyxLCLApplication.Create;
    GApp.Mount(GDocument);
    GApp.Window.Show;
    {$ifdef NYX_TOOL_CONNECTION_TESTS}

    if ParamStr(1) = '--connections-only' then
    begin
      ConnectView;
      GStage := 7;
    end;
    {$endif}
    GStarted := GetTickCount64;
    while not GFinished do
    begin
      CheckSynchronize;
      Application.ProcessMessages;

      if GetTickCount64 - GStarted > {$ifdef NYX_TOOL_CONNECTION_TESTS}180000{$else}90000{$endif} then
      begin
        raise Exception.Create('Tool canvas deadline at stage ' + IntToStr(GStage));
      end;
      Step;
      Sleep(1);
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' actual embedded canvas behavior checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-tool-canvas', 'failed');
      document.body.setAttribute('data-tool-canvas-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      {$ifdef PAS2JS}
      try
        Retire;
      except
        on LCleanupError: Exception do
        begin
          document.body.setAttribute('data-tool-canvas-cleanup-error', LCleanupError.Message);
        end;
      else
        document.body.setAttribute('data-tool-canvas-cleanup-error', 'Native host cleanup exception');
      end;
      {$else}
      Retire;
      {$endif}
    end;
  end;
end.
