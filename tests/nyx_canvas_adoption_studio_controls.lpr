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
program nyx_canvas_adoption_studio_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec,
  nyx.codegen, nyx.controls, nyx.state, nyx.root.types, nyx.application.context,
  nyx.application.state, nyx.scheduler, nyx.canvas.types, nyx.canvas.workspace,
  nyx.canvas.routes, nyx.canvas.route.types, nyx.canvas.declarations,
  nyx.canvas.adoption, nyx.flow.routes, nyx.flow.types, nyx.flow.declarations, nyx.tool.canvas,
  nyx.flow, nyx.flow.editing, nyx.logic, nyx.logic.programs, nyx.programs.types,
  nyx.programs.declarations,
  nyx.studio.projects, nyx.studio.sourcejobs, nyx.studio.previewruntime,
  nyx.studio.menu, nyx.test.programs.logic
  {$ifdef PAS2JS}, JS, Web, nyx.studio.browser, nyx.render.browser,
    nyx.canvas.browser, nyx.tool.canvas.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, nyx.studio.lcl,
    nyx.render.lcl, nyx.canvas.lcl, nyx.tool.canvas.lcl{$endif};

type
  { Deliberately deferred external host owns only a request lease. It never
    publishes a project; retirement checks cannot be mistaken for persistence. }
  TDeferredHost = class(TInterfacedObject, INyxCanvasRouteAdoptionHost)
  public
    Request: INyxCanvasRouteAdoptionRequest;
    procedure Submit(const ARequest: INyxCanvasRouteAdoptionRequest);
  end;

  {$ifndef PAS2JS}
  { Expose inherited actual Click without owning or extending widget lifetime. }
  TControlInput = class(TControl);
  { Fixture exception observer prevents a blocking Lazarus dialog. It outlives
    Studio and records callback failures outside borrowed notification stacks. }
  TFailure = class
  public
    Error: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}

var
  GStudio: {$ifdef PAS2JS}TNyxStudio{$else}TNyxNativeStudio{$endif};
  GTools: INyxToolCanvasView;
  GAuthor: INyxToolCanvasRouteAuthoring;
  GLogic: INyxToolCanvasLogicAuthoring;
  GPersistence: INyxToolCanvasRoutePersistence;
  GRoutes: INyxCanvasRouteEditing;
  GRuntime: INyxCanvasRuntimeView;
  GFlowEditing: INyxCanvasFlowEditing;
  GToolTarget: {$ifdef PAS2JS}INyxBrowserToolCanvasView{$else}INyxLCLToolCanvasView{$endif};
  GCanvas: {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};
  GNameFace: {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif};
  GDraftFace: {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif};
  GDeferred: TDeferredHost;
  GDeferredLease: INyxCanvasRouteAdoptionHost;
  GBase: TNyxProjectPair;
  GFirst: TNyxProjectPair;
  GSecond: TNyxProjectPair;
  GThird: TNyxProjectPair;
  GPending: TNyxText;
  GStage: Integer;
  GChecks: Integer;
  GStarted: Double;
  GDone: Boolean;
  GSaveCancelled: Boolean;
  GCancelChecked: Boolean;
  {$ifndef PAS2JS}
  GForm: TForm;
  GFailure: TFailure;
  {$endif}

procedure TDeferredHost.Submit(const ARequest: INyxCanvasRouteAdoptionRequest);
begin
  Request := ARequest;
end;

{$ifndef PAS2JS}
procedure TFailure.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.ClassName + ': ' + AException.Message;
end;
{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Canvas adoption: ' + AReason);
  end;
  Inc(GChecks);
end;

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

function Routes: INyxFlowRoutes;
begin
  Result := NewNyxFlowRoutes.WithRoute(NyxFlowRoute(NyxFlowRouteID('Live greeting'),
    NyxFlowRef('Greeting flow'), ntChange, nfcmAfterDispatch)
      .InputEventValue(ProgramLogicEndpoint('Name'), nftText)
      .OutputState(NyxFlowEndpoint(NyxFlowStep('echo'), NyxFlowPort('text')), NyxTextState('caption'))
      .Using(neUIQueue).Concurrent(nfrLatest).MaximumRuns(8));
end;

{ Build the expected observable two-step tool independently from the runtime
  editor. Original program declarations remain shared immutable source values. }
function EchoGraph(const ASeparator: TNyxText): INyxFlowGraph;
begin
  Result := NewNyxFlowGraph
    .WithStep(NyxFlowStep('greeting'), NewNyxProgramLogic(
      NyxProgramRef('Greeting'), GreetingProgramContract))
    .WithStep(NyxFlowStep('echo'), NewNyxTextJoin(ASeparator))
    .Connect(ProgramLogicEndpoint('Caption'),
      NyxFlowEndpoint(NyxFlowStep('echo'), NyxFlowPort('left')))
    .Connect(ProgramLogicEndpoint('Caption'),
      NyxFlowEndpoint(NyxFlowStep('echo'), NyxFlowPort('right'))).Build;
end;

procedure FocusedContract;
var
  LDocument: TNyxDocument;
  LRuntime: TNyxStudioPreviewRuntime;
  LContext: INyxApplicationContext;
  LOldContract: TNyxApplicationState;
  LProbeState: TNyxState;
  LColumn: INyxColumn;
  LLabel: INyxLabel;
  LBefore: TNyxText;
  LFailed: Boolean;
begin
  LDocument := TNyxDocument.Create;
  LRuntime := nil;
  LOldContract := nil;
  try
    LDocument.State.SetValue(NyxTextState('caption'), 'Saved default');
    LColumn := NewNyxColumn('reusable-only');
    LLabel := NewNyxLabel('focused-caption');
    LLabel.Binds.Text(NyxTextState('caption')).Done;
    LColumn.Add(LLabel);
    LDocument.AddComponent(LColumn);
    LBefore := TNyxCodec.Encode(LDocument);
    LRuntime := TNyxStudioPreviewRuntime.Create(LDocument,
      NyxReusableRoot('reusable-only'), {$ifdef PAS2JS}npfBrowser{$else}npfNativeLCL{$endif},
      NewNyxScheduler);
    LContext := LRuntime.Context;
    LRuntime.State.SetValue(NyxTextState('caption'), 'Preview value');
    Check((LRuntime.Bindings <> nil) and (LContext.State = LRuntime.State) and
      (TNyxCodec.Encode(LDocument) = LBefore),
      'component-only Interact uses independent stores and the ordinary exact root binding context');
    LRuntime.Close;
    Check(not LContext.Active, 'closing preview revokes retained application capabilities');
    LFailed := False;
    try
      LProbeState := LContext.State;
      Check(LProbeState <> nil, 'an active state capability resolves its typed store');
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'retired preview cannot dereference its former raw state owner');
    LFailed := False;
    try
      LOldContract := TNyxApplicationState.Create(LDocument);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'ordinary application construction still requires a real page');
  finally
    LOldContract.Free;
    LRuntime.Free;
    LDocument.Free;
  end;
end;

function ToolID(const ASuffix: TNyxText): TNyxText;
begin
  Result := GTools.Inspector.Root.ID + ASuffix;
end;

procedure ClickShell(const AID: TNyxText);
begin
  {$ifdef PAS2JS}
  GStudio.ShellView.ElementFor(AID).click;
  {$else}
  TControlInput(GStudio.ShellView.ControlFor(AID)).Click;
  {$endif}
end;

procedure ClickTool(const ASuffix: TNyxText);
begin
  {$ifdef PAS2JS}
  GToolTarget.Renderer.ElementFor(ToolID(ASuffix)).click;
  {$else}
  TControlInput(GToolTarget.Renderer.ControlFor(ToolID(ASuffix))).Click;
  {$endif}
end;

procedure Text(ARenderer: {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif};
  const AID, AValue: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(ARenderer.InputFor(AID)).value := AValue;
  ARenderer.InputFor(AID).dispatchEvent(TJSEvent.new('input'));
  {$else}
  TEdit(ARenderer.InputFor(AID)).Text := AValue;
  {$endif}

  if Pos('-logic-', AID) > 0 then
  begin
    Check(ARenderer.Root.Find(AID).Prop('value') = AValue,
      'visible unbound Logic field accepts actual input before saving');
  end;
end;

{ Exercise the real platform choice widget; typed operation names are copied
  through the shared form's closed catalog rather than arbitrary dispatch. }
procedure Choice(const AID, AValue: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLSelectElement(GToolTarget.Renderer.InputFor(AID)).value := AValue;
  GToolTarget.Renderer.InputFor(AID).dispatchEvent(TJSEvent.new('change'));
  {$else}
  TComboBox(GToolTarget.Renderer.InputFor(AID)).ItemIndex :=
    TComboBox(GToolTarget.Renderer.InputFor(AID)).Items.IndexOf(AValue);
  TComboBox(GToolTarget.Renderer.InputFor(AID)).OnChange(
    GToolTarget.Renderer.InputFor(AID));
  {$endif}
end;

procedure AcquireViews;
var
  LAuthor: INyxToolCanvasRouteAuthoring;
  LLogic: INyxToolCanvasLogicAuthoring;
  LPersistence: INyxToolCanvasRoutePersistence;
  LRoutes: INyxCanvasRouteEditing;
  LRuntime: INyxCanvasRuntimeView;
  LFlowEditing: INyxCanvasFlowEditing;
  LToolTarget: {$ifdef PAS2JS}INyxBrowserToolCanvasView{$else}INyxLCLToolCanvasView{$endif};
  LCanvas: {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};
  LWorkspace: TNyxNode;
begin
  GTools := GStudio.CanvasView.ToolCanvasView('program-tools');
  LWorkspace := GStudio.CanvasView.Root.Find('program-tools').Part('workspace');
  LCanvas := GStudio.CanvasView.CanvasView(LWorkspace.ID) as
    {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};
  GCanvas := LCanvas;
  GCanvas.Refresh;
  GCanvas.Execute(nccFit);
  Check(Supports(GCanvas, INyxCanvasRuntimeView, LRuntime),
    'ordinary preview owns an active scoped canvas runtime');
  GRuntime := LRuntime;
  Check(Supports(GCanvas, INyxCanvasFlowEditing, LFlowEditing),
    'actual Interact canvas supplies detached applied graph declarations');
  GFlowEditing := LFlowEditing;
  Check(Supports(GCanvas, INyxCanvasRouteEditing, LRoutes),
    'actual Interact canvas exposes existing runtime route publication');
  GRoutes := LRoutes;
  Check(Supports(GTools, {$ifdef PAS2JS}INyxBrowserToolCanvasView{$else}INyxLCLToolCanvasView{$endif}, LToolTarget),
    'actual target attachment exposes its owned ordinary Nyx inspector renderer');
  GToolTarget := LToolTarget;
  Check(Supports(GTools.Inspector, INyxToolCanvasRouteAuthoring, LAuthor) and
    Supports(GTools.Inspector, INyxToolCanvasRoutePersistence, LPersistence),
    'ordinary inspector supplies route authoring and explicit project adoption');
  GAuthor := LAuthor;
  GPersistence := LPersistence;
  Check(Supports(GTools.Inspector, INyxToolCanvasLogicAuthoring, LLogic),
    'ordinary Studio exposes the public staged graph workbench');
  GLogic := LLogic;
end;

procedure ReleaseViews;
begin
  GAuthor := nil;
  GLogic := nil;
  GPersistence := nil;
  GRoutes := nil;
  GRuntime := nil;
  GFlowEditing := nil;
  GToolTarget := nil;
  GCanvas := nil;
  GTools := nil;
end;

procedure History(ARedo: Boolean);
var
  LID: TNyxText;
  {$ifdef PAS2JS}LFace: TJSHTMLElement;{$endif}
begin
  LID := 'action-undo';

  if ARedo then
  begin
    LID := 'action-redo';
  end;
  {$ifdef PAS2JS}
  LFace := GStudio.ShellView.ElementFor(LID);

  if (LFace = nil) or (LFace.getBoundingClientRect.height = 0) then
  begin
    ClickShell(NyxStudioActionMenuID);

    if ARedo then
    begin
      ClickShell('studio-menu-redo');
    end
    else
    begin
      ClickShell('studio-menu-undo');
    end;
    Exit;
  end;
  {$endif}
  ClickShell(LID);
end;

function BeginDraft: Boolean;
var
  LSource: TNyxText;
  {$ifdef PAS2JS}LCode: TJSHTMLTextAreaElement;{$else}LCode: TControl;{$endif}
begin
  Result := False;
  LSource := GStudio.ProjectSnapshot.Source;
  {$ifdef PAS2JS}
  LCode := TJSHTMLTextAreaElement(document.querySelector('[data-nyx-id="studio-code"] textarea'));

  if LCode = nil then
  begin
    ClickShell('action-code');
    Exit;
  end;
  LCode.value := LSource + #10 + '// unfinished Pascal';
  LCode.dispatchEvent(TJSEvent.new('input'));
  {$else}
  try
    LCode := GStudio.CodeView.InputFor('studio-code');
  except
    on Exception do
    begin
      LCode := nil;
    end;
  end;

  if LCode = nil then
  begin
    ClickShell('action-code');
    Exit;
  end;
  TMemo(LCode).Text := LSource + #10 + '// unfinished Pascal';
  {$endif}
  GPending := EncodeNyxProject(GStudio.ProjectSnapshot);
  Check(GStudio.ProjectSnapshot.Source <> GStudio.ProjectSnapshot.Draft,
    'actual source input creates an unfinished buffer outside the accepted pair');
  Result := True;
end;

{$ifndef PAS2JS}
{ Export exactly the accepted companion and design bytes from this owned input
  journey. Independent compiler processes consume them after Studio retires. }
procedure ExportPair(const AStage: TNyxText; const APair: TNyxProjectPair);
var
  LDirectory: TNyxText;

  procedure WriteBytes(const AName, AValue: TNyxText);
  var
    LStream: TFileStream;
  begin
    LStream := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if Length(AValue) > 0 then
      begin
        LStream.WriteBuffer(AValue[1], Length(AValue));
      end;
    finally
      LStream.Free;
    end;
  end;

begin
  LDirectory := IncludeTrailingPathDelimiter(ParamStr(1)) + 'exports' + PathDelim + AStage + PathDelim;
  ForceDirectories(LDirectory);
  WriteBytes('nyx.generated.view.pas', APair.Source);
  WriteBytes('design.nyx', APair.Design);
end;
{$endif}

procedure Step;
var
  LDocument: TNyxDocument;
  LExpected: TNyxDocument;
begin
  {$ifndef PAS2JS}

  if GFailure.Error <> '' then
  begin
    raise Exception.Create(GFailure.Error);
  end;
  {$endif}

  if Clock - GStarted > 300000 then
  begin
    raise Exception.Create('Actual Studio adoption deadline at stage ' + TNyxText(IntToStr(GStage)));
  end;

  if GStudio.SourceBusy or GStudio.PresentationPending then
  begin
    Exit;
  end;

  if (GTools <> nil) and (GTools.RefreshExecution <> nil) then
  begin

    if GTools.RefreshExecution.Status in [nesPending, nesRunning] then
    begin
      Exit;
    end;

    if GTools.RefreshExecution.Status <> nesSucceeded then
    begin
      raise Exception.Create('Inspector publication failed: ' + GTools.RefreshExecution.Failure);
    end;
  end;
  case GStage of
    0:
      begin
        LDocument := ProgramToolCanvasDocument;
        try
          GStudio.SourceCommands.OpenProject(NyxProjectPair(TNyxCodec.Encode(LDocument),
            TNyxCodegen.Generate(LDocument)), nprRequireMatch);
        finally
          LDocument.Free;
        end;
      end;
    1:
      begin
        GBase := GStudio.ProjectSnapshot;
        ClickShell('action-preview');
      end;
    2:
      begin
        AcquireViews;
        GFlowEditing.ApplyFlowChanges([NyxDefineFlow(NyxFlowRef('Greeting flow'),
          NyxFlowDefinitionFromData(EchoGraph(' / ').ToData))],
          GRuntime.Scene.Revision, GFlowEditing.FlowRevision);
        GRoutes.ApplyRouteChanges([
          NyxCanvasRoutes(NyxCanvasItem('Greeting'), NyxControl('name-input'), Routes),
          NyxCanvasRoutes(NyxCanvasItem('Greeting'), NyxControl('greet-button'), NewNyxFlowRoutes)],
          GRuntime.Scene.Revision, GRoutes.RouteRevision);
        GNameFace := GCanvas.RendererFor(NyxCanvasItem('Greeting')).InputFor('name-input');
        Text(GCanvas.RendererFor(NyxCanvasItem('Greeting')), 'name-input', 'Avery');
      end;
    3:
      begin

        if GCanvas.RendererFor(NyxCanvasItem('Greeting')).Root.Find('caption-label').Prop('text') <>
          'Hello, Avery! / Hello, Avery!' then
        begin
          Exit;
        end;

        if not GSaveCancelled then
        begin
          ClickTool('-save-connections');
          Check(GPersistence.AdoptionPending, 'ordinary Save owns a pending cancellable source command');
          GStudio.SourceCommands.Cancel;
          GSaveCancelled := True;
          Exit;
        end;

        if not GCancelChecked then
        begin
          Check(GStudio.SourceCommands.State = nssCancelled,
            'ordinary cancellation retains its terminal status, actual ' +
              TNyxText(IntToStr(Ord(GStudio.SourceCommands.State))));
          Check(not GPersistence.AdoptionPending,
            'ordinary cancellation retires its save ticket independently of draining worker work');
          Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GBase),
            'ordinary cancellation preserves the exact accepted pair');
          Check(GCanvas.RendererFor(NyxCanvasItem('Greeting')).InputFor('name-input') = GNameFace,
            'cancelled source work retains the actual live input control');
          GCancelChecked := True;
        end;
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GBase),
          'real Interact input runs the Pascal script and binding without changing saved defaults/source');
        GTools.Inspector.SelectControl(NyxCanvasItem('Greeting'), NyxControl('name-input'));
        GAuthor.ShowEventRoutes;
        GTools.Refresh;
        Text(GToolTarget.Renderer, GAuthor.RouteEditor.NameInput.ID, 'An unfinished proposal');
        GDraftFace := GToolTarget.Renderer.InputFor(GAuthor.RouteEditor.NameInput.ID);
        ClickTool('-save-connections');
        Check(GPersistence.AdoptionPending, 'actual Save submits one immutable pending snapshot');
        GFlowEditing.ApplyFlowChanges([NyxReplaceFlowStep(NyxFlowRef('Greeting flow'),
          NyxFlowStep('echo'), NewNyxTextJoin(' + '))],
          GRuntime.Scene.Revision, GFlowEditing.FlowRevision);
        GRoutes.ApplyRouteChanges([NyxCanvasRoutes(NyxCanvasItem('Greeting'), NyxControl('caption-label'),
          NewNyxFlowRoutes)], GRuntime.Scene.Revision, GRoutes.RouteRevision);
      end;
    4:
      begin
        GFirst := GStudio.ProjectSnapshot;
        {$ifndef PAS2JS}ExportPair('first', GFirst);{$endif}
        Check(not GPersistence.AdoptionPending and (GStudio.SourceCommands.State = nssApplied),
          'ordinary per-project source command acknowledges actual paired publication');
        LExpected := TNyxCodec.Decode(GBase.Design);
        try
          LExpected.Flows.Define(NyxFlowRef('Greeting flow'), EchoGraph(' / '));
          LExpected.Canvases.Define(NyxCanvasRef('Greeting canvas'),
            NyxCanvasConnections(LExpected.Canvases.Definition(NyxCanvasRef('Greeting canvas')))
              .ForControl(NyxCanvasItem('Greeting'), NyxControl('name-input'), Routes)
              .ForControl(NyxCanvasItem('Greeting'), NyxControl('greet-button'), NewNyxFlowRoutes).Done);
          Check(GFirst.Design = TNyxCodec.Encode(LExpected),
            'the complete saved model equals the exact first graph and wiring snapshot');
        finally
          LExpected.Free;
        end;
        Check((Pos('NyxCanvasConnections(', GFirst.Source) > 0) and
          (GCanvas.RendererFor(NyxCanvasItem('Greeting')).InputFor('name-input') = GNameFace),
          'typed generated Pascal and accepted source preserve the exact live input widget');
        Check((GToolTarget.Renderer.InputFor(GAuthor.RouteEditor.NameInput.ID) = GDraftFace) and
          (GToolTarget.Renderer.Root.Find(GAuthor.RouteEditor.NameInput.ID).Prop('value') = 'An unfinished proposal'),
          'successful adoption retains the exact unfinished form/editor and its text');
        Check(Pos('Newer live changes', GPersistence.AdoptionFeedback) > 0,
          'acknowledgement distinguishes saved snapshot from newer applied live wiring');
        ClickTool('-save-connections');
      end;
    5:
      begin
        GSecond := GStudio.ProjectSnapshot;
        {$ifndef PAS2JS}ExportPair('second', GSecond);{$endif}
        LDocument := TNyxCodec.Decode(GSecond.Design);
        try
          Check(LDocument.Flows.Definition(NyxFlowRef('Greeting flow')).ToData.ToJSON =
            EchoGraph(' + ').ToData.ToJSON, 'second Save adopts newer graph configuration exactly');
          Check(Length(NyxCanvasConnectionChanges(LDocument.Canvases.Definition(
            NyxCanvasRef('Greeting canvas')))) = 3,
            'a second ordinary Save adopts only remaining live changes against the updated baseline');
        finally
          LDocument.Free;
        end;
        Check((GNameFace = GCanvas.RendererFor(NyxCanvasItem('Greeting')).InputFor('name-input')) and
          (GDraftFace = GToolTarget.Renderer.InputFor(GAuthor.RouteEditor.NameInput.ID)),
          'repeated snapshots preserve both live controls and the unfinished route proposal');
        ClickTool('-save-connections');
        Check(not GPersistence.AdoptionPending and (Pos('already saved', GPersistence.AdoptionFeedback) > 0),
          'saving unchanged applied wiring creates no pending request or extra source command');
        GFlowEditing.ApplyFlowChanges([NyxReplaceFlowStep(NyxFlowRef('Greeting flow'),
          NyxFlowStep('echo'), NewNyxTextJoin(' * '))],
          GRuntime.Scene.Revision, GFlowEditing.FlowRevision);
        GLogic.ShowLogic;
        GTools.Refresh;
        Choice(ToolID('-logic-operation'), 'json-format');
        Text(GToolTarget.Renderer, ToolID('-logic-step-name'), 'future-format');
      end;
    6:
      begin
        ClickTool('-logic-add');
      end;
    7:
      begin
        Check(GLogic.HasPendingLogic, 'incomplete future topology is a local staged proposal');
        Choice(ToolID('-logic-operation'), 'json-format');
      end;
    8:
      begin
        Text(GToolTarget.Renderer, ToolID('-logic-indent'), 'four?');
        GDraftFace := GToolTarget.Renderer.InputFor(ToolID('-logic-indent'));
        ClickTool('-save-connections');
      end;
    9:
      begin
        GThird := GStudio.ProjectSnapshot;
        {$ifndef PAS2JS}ExportPair('graph-only', GThird);{$endif}
        LDocument := TNyxCodec.Decode(GThird.Design);
        LExpected := TNyxCodec.Decode(GSecond.Design);
        try
          LExpected.Flows.Define(NyxFlowRef('Greeting flow'), EchoGraph(' * '));
          Check(GThird.Design = TNyxCodec.Encode(LExpected),
            'graph-only Save changes exact applied declaration without adding route masks');
          Check(LDocument.Flows.Definition(NyxFlowRef('Greeting flow')).StepCount = 2,
            'graph-only Save excludes the unapplied future step');
        finally
          LExpected.Free;
          LDocument.Free;
        end;
        Check(GLogic.HasPendingLogic and
          (GToolTarget.Renderer.InputFor(ToolID('-logic-indent')) = GDraftFace) and
          (GToolTarget.Renderer.Root.Find(ToolID('-logic-indent')).Prop('value') = 'four?'),
          'successful graph saving retains staged topology, malformed field and actual editor identity: ' +
          'pending=' + TNyxText(BoolToStr(GLogic.HasPendingLogic, True)) +
          ', same=' + TNyxText(BoolToStr(
            GToolTarget.Renderer.InputFor(ToolID('-logic-indent')) = GDraftFace, True)) +
          ', value=' + GToolTarget.Renderer.Root.Find(ToolID('-logic-indent')).Prop('value'));
        Check(GCanvas.RendererFor(NyxCanvasItem('Greeting')).InputFor('name-input') = GNameFace,
          'graph-only source publication retains the actual embedded input');
        ReleaseViews;
        History(False);
      end;
    10:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GSecond),
          'one Undo restores the complete pair before graph-only Save');
        History(False);
      end;
    11:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GFirst),
          'one actual Undo restores the exact first design/Pascal pair');
        History(True);
      end;
    12:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GSecond),
          'one actual Redo restores the exact second saved snapshot');
        History(True);
      end;
    13:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GThird),
          'one Redo restores the exact graph-only saved pair');
        AcquireViews;
        GFlowEditing.ApplyFlowChanges([NyxReplaceFlowStep(NyxFlowRef('Greeting flow'),
          NyxFlowStep('echo'), NewNyxTextJoin(' % '))],
          GRuntime.Scene.Revision, GFlowEditing.FlowRevision);

        if not BeginDraft then
        begin
          Exit;
        end;
        ClickTool('-save-connections');
      end;
    14:
      begin
        Check((GStudio.SourceCommands.State = nssRejected) and not GPersistence.AdoptionPending and
          (EncodeNyxProject(GStudio.ProjectSnapshot) = GPending),
          'unfinished Pascal refuses ordinary Save with its complete accepted pair/buffer/base retained');
        GDeferred := TDeferredHost.Create;
        GDeferredLease := GDeferred;
        LDocument := TNyxCodec.Decode(GThird.Design);
        try
          GPersistence.ConfigureAdoption(NyxControl(LDocument.Find('program-tools').Part('workspace').ID),
            GDeferredLease, nil);
        finally
          LDocument.Free;
        end;
        GTools.Refresh;
        ClickTool('-save-connections');
        Check((GDeferred.Request <> nil) and GDeferred.Request.Pending,
          'an explicit asynchronous host may retain its independent snapshot request');
        GTools.Disconnect;
        Check(not GDeferred.Request.Pending and not GDeferred.Request.Refused('Late result') and
          not GDeferred.Request.Accepted(nil),
          'retired ticket refuses every late result without reviving its former inspector');
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GPending,
          'request retirement never changes source, saved design or unfinished Pascal');
        GDone := True;
      end;
  end;
  Inc(GStage);
end;

procedure Cleanup;
{$ifndef PAS2JS}
var
  LTurn: Integer;
  LError: TNyxText;
{$endif}
begin
  ReleaseViews;
  GDeferredLease := nil;
  GDeferred := nil;
  GStudio.Free;
  GStudio := nil;
  {$ifndef PAS2JS}
  FreeAndNil(GForm);
  { Revoked platform couriers still own inert work until their queued turn.
    Exercise those real turns after every borrowed Studio/widget owner is gone;
    neither removing queue entries nor exiting early proves safe retirement. }
  for LTurn := 1 to 3 do
  begin
    CheckSynchronize;
    Application.ProcessMessages;
  end;
  LError := '';

  if GFailure <> nil then
  begin
    LError := GFailure.Error;
  end;
  Application.OnException := nil;
  FreeAndNil(GFailure);
  Check(LError = '', 'retired actual UI turns never call a destroyed receiver: ' + LError);
  {$endif}
end;

procedure Drive;
begin
  try
    Step;

    if GDone then
    begin
      Cleanup;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-adoption-studio', 'passed');
      document.body.setAttribute('data-canvas-adoption-checks', IntToStr(GChecks));
      {$else}
      WriteLn('PASS ', GChecks, ' actual Studio canvas adoption checks');
      {$endif}
      Exit;
    end;
  except
    on E: Exception do
    begin
      GDone := True;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-adoption-studio', 'failed');
      document.body.setAttribute('data-canvas-adoption-error', E.Message);
      document.body.setAttribute('data-canvas-adoption-stage', IntToStr(GStage));
      {$else}
      WriteLn('FAIL stage ', GStage, ' / ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      Cleanup;
      Exit;
    end;
  end;
  {$ifdef PAS2JS}window.setTimeout(@Drive, 25);{$endif}
end;

begin
  FocusedContract;
  {$ifndef PAS2JS}

  if (ParamCount <> 1) or DirectoryExists(ParamStr(1)) then
  begin
    raise Exception.Create('Supply a fresh owned Studio project directory');
  end;
  Application.Initialize;
  GFailure := TFailure.Create;
  Application.OnException := GFailure.Failed;
  GForm := TForm.CreateNew(nil);
  GForm.SetBounds(20, 20, 1280, 900);
  GForm.Show;
  GStudio := TNyxNativeStudio.Create(GForm, ParamStr(1));
  GStudio.Run;
  {$else}
  GStudio := TNyxStudio.Create;
  GStudio.Run(False);
  {$endif}
  GStarted := Clock;
  {$ifdef PAS2JS}
  Drive;
  {$else}
  while not GDone do
  begin
    CheckSynchronize;
    Application.ProcessMessages;
    Drive;
    Sleep(25);
  end;
  {$endif}
end.
