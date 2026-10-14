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


program nyx_logic_collections_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec, nyx.codegen,
  nyx.state, nyx.controls, nyx.events, nyx.behavior, nyx.callbacks, nyx.scheduler,
  nyx.canvas.types, nyx.canvas.workspace, nyx.view.behavior, nyx.logic.editor,
  nyx.studio.session, nyx.studio.projects, nyx.studio.edits,
  nyx.studio.sourcejobs, nyx.studio.inspector,
  nyx.root.types, nyx.flow.declarations,
  nyx.flow, nyx.flow.types, nyx.flow.editing, nyx.logic, nyx.logic.collections,
  nyx.flow.execution, nyx.linq.execution, nyx.test.logic.fixture,
  nyx.test.logic.collections
  {$ifdef NYX_COLLECTION_COMPANION}, nyx.generated.view{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser, nyx.canvas.browser, nyx.render.browser
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, nyx.widgets.lcl,
  nyx.application.lcl, nyx.canvas.lcl, nyx.render.lcl{$endif};

type
  TOrdinaryObserver = class(TNyxApplicationEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  TEditorInput = class
  public
    Captured: Boolean;
    Session: TNyxStudioSession;
    Commands: TNyxSourceCommands;
    FormDocument: TNyxDocument;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
  end;

var
  GDoc: TNyxDocument;
  GExpected: TNyxDocument;
  GBefore: TNyxText;
  GChecks: Integer;
  GStage: Integer;
  GAuthorPhase: Integer;
  GAuthorBefore: TNyxText;
  GAuthorAfter: TNyxText;
  GFinished: Boolean;
  GBehavior: INyxViewBehavior;
  GWorker: INyxQueryRun;
  GEditor: TEditorInput;
  GEditorForm: INyxCard;
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GCanvas: INyxBrowserCanvasWorkspaceView;
  GRenderer: TNyxBrowserRenderer;
  GEditorHost: TJSHTMLElement;
  GHost: TJSHTMLElement;
  GStarted: Double;
  {$else}
  GApp: TNyxLCLApplication;
  GCanvas: INyxLCLCanvasWorkspaceView;
  GRenderer: TNyxLCLRenderer;
  GEditorHost: TForm;
  GStarted: QWord;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Actual collection Logic: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TOrdinaryObserver.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Check(Context.State = GApp.State, 'static callback shares application state beside Logic');
end;

procedure TEditorInput.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LEdit: TNyxStudioDesignEdit;
begin

  if HandleNyxLogicEditor(ANode, GRenderer.Root) then
  begin
    GRenderer.Sync;
  end;

  if (ANode.ID = 'editor-add') and
    CaptureNyxLogicInspector(Session, ANode, GRenderer.Root, LEdit) then
  begin
    Commands.Edit(LEdit);
    Captured := True;
  end;
end;

procedure TEditorInput.Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
begin
  { Queue completion is inspected on a later real UI turn. Never replace a
    notifying checkbox/select or destroy the controller in its own callback. }
end;

procedure Change(const AName, AValue: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TControl;{$endif}
begin
  LID := 'editor-' + AName;
  LInput := GRenderer.InputFor(LID);
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
    TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(AValue);
    TComboBox(LInput).OnChange(LInput);
  end
  else
  begin
    TEdit(LInput).Text := AValue;
  end;
  {$endif}
  Check(GRenderer.Root.Find(LID).Prop('value') = AValue, 'actual typed configuration input: ' + AName);
end;

procedure Click(const AName: TNyxText);
begin
  {$ifdef PAS2JS}
  GRenderer.ElementFor('editor-' + AName).click;
  {$else}
  TNyxLCLButton(GRenderer.ControlFor('editor-' + AName)).Click;
  {$endif}
end;

function Author(AIndex: Integer): Boolean;
const
  CKinds: array[0..3] of TNyxText = ('json-filter', 'json-sort', 'json-map', 'json-slice');
begin
  Result := False;

  if GAuthorPhase = 1 then
  begin
    {$ifdef PAS2JS}

    if (AIndex = 0) and
      (document.body.getAttribute('data-capture-observed') <> 'collection-filter') then
    begin
      Exit;
    end;
    {$endif}
    GAuthorBefore := EncodeNyxProject(GEditor.Session.ProjectSnapshot);
    GEditor.Captured := False;
    Click('add');
    Check(GEditor.Captured, 'actual add button captures a typed collection operation');
    GAuthorPhase := 4;
    Exit;
  end;

  if GAuthorPhase = 4 then
  begin

    if GEditor.Commands.Busy then
    begin
      Exit;
    end;
    Check(GEditor.Commands.State = nssApplied, 'ordinary Studio source worker publishes the typed operation');
    GAuthorAfter := EncodeNyxProject(GEditor.Session.ProjectSnapshot);
    Check(GAuthorBefore <> GAuthorAfter, 'physical configuration publishes a complete changed source/design pair');
    GAuthorPhase := 2;
    Exit;
  end;

  if GAuthorPhase = 2 then
  begin
    GEditor.Session.Undo;
    Check(EncodeNyxProject(GEditor.Session.ProjectSnapshot) = GAuthorBefore, 'physical authoring paired Undo is exact');
    Inc(GAuthorPhase);
    Exit;
  end;

  if GAuthorPhase = 3 then
  begin
    GEditor.Session.Redo;
    Check(EncodeNyxProject(GEditor.Session.ProjectSnapshot) = GAuthorAfter, 'physical authoring paired Redo is exact');
    FreeAndNil(GRenderer);
    GEditor.FormDocument.RemoveRoot(NyxReusableRoot('editor'));
    GEditorForm := nil;
    {$ifdef PAS2JS}
    GRenderer := TNyxBrowserRenderer.Create;
    GRenderer.OnEvent := @GEditor.Event;
    {$else}
    GRenderer := TNyxLCLRenderer.Create;
    GRenderer.OnEvent := GEditor.Event;
    {$endif}
    GAuthorPhase := 0;
    Exit(True);
  end;
  GEditorForm := NewNyxLogicEditor('editor', GEditor.Session.Document, NyxFlowRef('Configuration'));
  {$ifdef PAS2JS}
  GHost.style.setProperty('display', 'none');
  {$endif}
  GEditor.FormDocument.AddComponent(GEditorForm);
  GRenderer.Render(GEditor.FormDocument, GEditorForm.Node, GEditorHost);
  Change('operation', CKinds[AIndex]);
  Change('step-name', 'extra-' + CKinds[AIndex]);

  if AIndex < 3 then
  begin
    case AIndex of
      0: Change('path-part', 'active');
      1: Change('path-part', 'priority');
      2: Change('path-part', 'profile');
    end;
    Click('path-add');
  end;
  case AIndex of
    0:
    begin
      Change('comparison', 'Equal');
      Change('expected-kind', 'Boolean');
      Change('expected', 'true');
    end;
    1:
    begin
      Change('key-kind', 'Exact decimal');
      Change('direction', 'Descending');
    end;
    3:
    begin
      Change('offset', '1');
      Change('count', '3');
    end;
  end;
  {$ifdef PAS2JS}

  if AIndex = 0 then
  begin
    GRenderer.ElementFor('editor-comparison').scrollIntoView(True);
    document.body.setAttribute('data-capture-checkpoint', 'collection-filter');
  end;
  {$endif}
  GAuthorPhase := 1;
end;

procedure Edit(const AValue: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLTextAreaElement(GCanvas.InputFor(NyxCanvasItem('Editor'), NyxControl('source-memo'))).value := AValue;
  GCanvas.InputFor(NyxCanvasItem('Editor'), NyxControl('source-memo')).dispatchEvent(TJSEvent.new('input'));
  {$else}
  TMemo(GCanvas.InputFor(NyxCanvasItem('Editor'), NyxControl('source-memo'))).Text := AValue;
  {$endif}
end;

function Idle: Boolean;
begin
  Result := (GBehavior.Routes.Report(0).Field('active').AsInteger = 0) and
    (GBehavior.Routes.Report(1).Field('active').AsInteger = 0);
end;

function PrettyText: TNyxText;
begin
  {$ifdef PAS2JS}
  Result := GCanvas.RendererFor(NyxCanvasItem('Output')).ElementFor('pretty-code').textContent;
  {$else}
  Result := TNyxText(TMemo(GCanvas.RendererFor(NyxCanvasItem('Output')).ControlFor('pretty-code')).Text);
  {$endif}
end;

procedure Step;
var
  LRuntime: INyxCanvasRuntimeView;
begin
  case GStage of
    0, 1, 2, 3:
    begin

      if not Author(GStage) then
      begin
        Exit;
      end;
    end;
    4:
    begin
      {$ifdef PAS2JS}
      GHost.style.setProperty('display', '');
      window.scrollTo(0, 0);
      GCanvas := GApp.View.CanvasView(GDoc.Find('tool-canvas').Part(NyxPart('workspace')).ID)
        as INyxBrowserCanvasWorkspaceView;
      {$else}
      GCanvas := GApp.View.CanvasView(GDoc.Find('tool-canvas').Part(NyxPart('workspace')).ID)
        as INyxLCLCanvasWorkspaceView;
      {$endif}
      GCanvas.Refresh;
      GCanvas.Execute(nccFit);
      Check(Supports(GCanvas, INyxCanvasRuntimeView, LRuntime), 'actual embedded runtime capability');
      GBehavior := LRuntime.BehaviorFor(NyxCanvasItem('Editor'));
      {$ifdef PAS2JS}
      TJSHTMLElement(GCanvas.RendererFor(NyxCanvasItem('Editor')).ElementFor('run-button')).click;
      {$else}
      TNyxLCLButton(GCanvas.RendererFor(NyxCanvasItem('Editor')).ControlFor('run-button')).Click;
      {$endif}
    end;
    5:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check(GApp.State.GetValue(NyxTextState('compact')) = '[{"name":"Ada"},{"name":"Grace"}]',
        'physical control executes filter/sort/page/map and publishes exact output');
      Edit('[{"active":true,"priority":2,"profile":{"name":"New result"}}]');
    end;
    6:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check(GApp.State.GetValue(NyxTextState('compact')) = '[{"name":"New result"}]',
        'live embedded memo input runs the same connected collection operations');
      Check(Pos('New result', PrettyText) > 0, 'sibling physical code control displays the collection result');
      Edit('[{"active":true,"priority":"wrong family","profile":{"name":"Must not publish"}}]');
    end;
    7:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check((GBehavior.Routes.Report(0).Field('failed').AsInteger +
        GBehavior.Routes.Report(1).Field('failed').AsInteger) > 0,
        'typed sort failure is visible in route feedback');
      Check(GApp.State.GetValue(NyxTextState('compact')) = '[{"name":"New result"}]',
        'failed transform publishes no partial output');
      Check(TNyxCodec.Encode(GDoc) = GBefore, 'runtime/editor instances do not mutate saved demo');
      GWorker := TNyxFlowApplication.Submit(GApp.Context, CollectionLogicGraph, [
        NyxFlowInput(LogicEndpoint('parse', nlpText), NyxFlowText(CollectionLogicSource)),
        NyxFlowInput(LogicEndpoint('summary', nlpLeft), NyxFlowText('Scheduled people'))], [
        NyxFlowAssign(NyxTextState('compact'), LogicEndpoint('compact', nlpText))],
        {$ifdef PAS2JS}neAsynchronous{$else}neThreaded{$endif});
    end;
    8:
    begin

      if GWorker.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
      begin
        Exit;
      end;
      Check((GWorker.Phase = nqpSucceeded) and
        (GApp.State.GetValue(NyxTextState('compact')) = '[{"name":"Ada"},{"name":"Grace"}]'),
        'native worker/browser event-loop collection graph publishes through the UI courier');
      GFinished := True;
    end;
  end;
  Inc(GStage);
end;

procedure Retire;
begin
  FreeAndNil(GRenderer);
  GEditorForm := nil;
  {$ifdef PAS2JS}GEditorHost.remove;{$else}FreeAndNil(GEditorHost);{$endif}
  if GEditor <> nil then
  begin
    GEditor.Commands.Free;
    GEditor.Session.Free;
    GEditor.FormDocument.Free;
    FreeAndNil(GEditor);
  end;
  GCanvas := nil;
  if GWorker <> nil then
  begin
    GWorker.Cancel;
  end;
  FreeAndNil(GApp);
  GWorker := nil;
  GBehavior := nil;
  FreeAndNil(GDoc);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    Step;

    if GFinished then
    begin
      document.body.setAttribute('data-collection-controls', 'passed');
      document.body.setAttribute('data-collection-controls-checks', IntToStr(GChecks));
      Exit;
    end;

    if TJSDate.now - GStarted > 150000 then
    begin
      raise Exception.Create('Collection controls deadline at stage ' + IntToStr(GStage));
    end;
    window.setTimeout(@Drive, 30);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-collection-controls', 'failed');
      document.body.setAttribute('data-collection-controls-error', E.Message);
      Retire;
    end;
  end;
end;
{$endif}

begin
  try
    RegisterNyxCallback(NyxHandler('TToolRunObserver'), TOrdinaryObserver);
    {$ifdef NYX_COLLECTION_COMPANION}
    GDoc := BuildNyxDocument;
    GExpected := CollectionLogicDocument;
    try
      Check(TNyxCodec.Encode(GDoc) = TNyxCodec.Encode(GExpected), 'complete exact compiled collection companion');
    finally
      FreeAndNil(GExpected);
    end;
    {$else}
    GDoc := CollectionLogicDocument;
    {$endif}
    GBefore := TNyxCodec.Encode(GDoc);
    GEditor := TEditorInput.Create;
    { Authoring is an independent ordinary project. The executable companion
      remains a full canvas project; configuration history never changes it.
      Adding an unwired required input to its invoked graph would be invalid. }
    GExpected := TNyxDocument.Create;
    try
      GExpected.Title := 'Collection configuration';
      GExpected.AddPage(NewNyxPage('configuration-home'));
      GExpected.Flows.Define(NyxFlowRef('Configuration'), NewNyxFlowGraph
        .WithStep(NyxFlowStep('initial'), NewNyxJSONMap(NyxDataPath)).Build);
      GEditor.Session := TNyxStudioSession.Create(NyxProjectPair(
        TNyxCodec.Encode(GExpected), TNyxCodegen.Generate(GExpected)));
    finally
      FreeAndNil(GExpected);
    end;
    GEditor.Commands := TNyxSourceCommands.Create(GEditor.Session, {$ifdef PAS2JS}@{$endif}GEditor.Changed);
    GEditor.FormDocument := TNyxDocument.Create;
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GHost);
    GEditorHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GEditorHost);
    GRenderer := TNyxBrowserRenderer.Create;
    GRenderer.OnEvent := @GEditor.Event;
    GApp := TNyxBrowserApplication.Create;
    GApp.Run(GDoc, GHost);
    GStarted := TJSDate.now;
    Drive;
    {$else}
    Application.Initialize;
    GEditorHost := TForm.CreateNew(nil);
    GEditorHost.SetBounds(10, 10, 720, 900);
    GEditorHost.Show;
    GRenderer := TNyxLCLRenderer.Create;
    GRenderer.OnEvent := GEditor.Event;
    GApp := TNyxLCLApplication.Create;
    GApp.Mount(GDoc);
    GApp.Window.Show;
    GStarted := GetTickCount64;
    while not GFinished do
    begin
      CheckSynchronize;
      Application.ProcessMessages;
      Step;

      if GetTickCount64 - GStarted > 150000 then
      begin
        raise Exception.Create('Native collection controls deadline');
      end;
      Sleep(1);
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' actual collection Logic/editor controls');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-collection-controls', 'failed');
      document.body.setAttribute('data-collection-controls-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      Retire;
      {$endif}
    end;
  end;
end.
