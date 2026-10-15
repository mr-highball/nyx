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
  nyx.logic.relations, nyx.numbers.decimal, nyx.logic.joins, nyx.logic.regex,
  nyx.regex.types, nyx.test.logic.regex,
  nyx.flow.execution, nyx.linq.execution, nyx.test.logic.fixture,
  nyx.test.logic.collections
  {$ifdef NYX_COLLECTION_COMPANION}, nyx.generated.view{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser, nyx.canvas.browser, nyx.render.browser
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, nyx.widgets.lcl,
  nyx.application.lcl, nyx.canvas.lcl, nyx.render.lcl{$endif};

type
  { Application-owned static observer verifies ordinary callbacks coexist with
    saved Logic routes; it borrows the context only during Invoke. }
  TOrdinaryObserver = class(TNyxApplicationEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  { Fixture controller owns its session/commands/form document until Retire.
    Renderer callbacks borrow it; publication is observed on a later UI turn. }
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

{ Exercise the public disclosure header before editing advanced inputs; hidden
  model-field writes do not qualify ordinary user access to these settings. }
procedure OpenLimits;
var
  LHeader: TNyxNode;
begin
  LHeader := GRenderer.Root.Find('editor-regex-limits').Part(NyxPart('header'));
  {$ifdef PAS2JS}
  GRenderer.ElementFor(LHeader.ID).click;
  {$else}
  TNyxLCLButton(GRenderer.ControlFor(LHeader.ID)).Click;
  {$endif}
end;

function Author(AIndex: Integer): Boolean;
const
  CKinds: array[0..14] of TNyxText = ('json-filter', 'json-sort', 'json-map', 'json-slice',
    'json-set', 'json-remove', 'json-merge', 'json-group', 'json-distinct', 'json-join', 'json-aggregate', 'flow-join',
    'regex-match', 'regex-replace', 'regex-extract');
var
  LDefinition: INyxFlowDefinition;
  LConfiguration: TNyxDataValue;
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

    if (AIndex = 6) and
      (document.body.getAttribute('data-capture-observed') <> 'json-merge-policy') then
    begin
      Exit;
    end;

    if (AIndex = 9) and
      (document.body.getAttribute('data-capture-observed') <> 'relation-join') then
    begin
      Exit;
    end;

    if (AIndex = 11) and
      (document.body.getAttribute('data-capture-observed') <> 'flow-join') then
    begin
      Exit;
    end;

    if (AIndex = 13) and
      (document.body.getAttribute('data-capture-observed') <> 'regex-replacement') then
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
    LDefinition := GEditor.Session.Document.Flows.Definition(NyxFlowRef('Configuration'));
    LConfiguration := LDefinition.Configuration(LDefinition.StepCount - 1);
    case AIndex of
      7: Check(LConfiguration.Field('path').ToJSON = '["customer"]', 'Ordinary grouping publishes its typed key path');
      8: Check(LConfiguration.Field('path').ToJSON = '["id"]', 'Ordinary distinct publishes its typed key path');
      9: Check((LConfiguration.Field('left').ToJSON = '["customer"]') and
        (LConfiguration.Field('right').ToJSON = '["id"]') and
        (LConfiguration.Field('join').AsInteger = Ord(njjFull)) and
        (LConfiguration.Field('nulls').AsInteger = Ord(njnMatch)), 'Ordinary join publishes independent paths and policies');
      10: Check((LConfiguration.Field('aggregation').AsInteger = Ord(njaAverage)) and
        (LConfiguration.Field('rounding').AsInteger = Ord(ndrFloor)) and
        (LConfiguration.Field('empty').AsInteger = Ord(njaeError)), 'Ordinary summary publishes exact arithmetic policies');
      11: Check((LConfiguration.Field('type').AsInteger = Ord(nftJSON)) and
        (LConfiguration.Field('selection').AsInteger = Ord(nfjLastAvailable)) and
        (LConfiguration.Field('empty').AsInteger = Ord(nfjeError)) and
        (LConfiguration.Field('inputs').Item(0).AsText = 'fallback') and
        (LConfiguration.Field('inputs').Item(1).AsText = 'primary') and
        (LConfiguration.Field('inputs').Count = 2), 'Ordinary branch join publishes exact priority, payload and empty policies');
      12: Check((LConfiguration.Field('options').Field('case').AsInteger = Ord(nrcASCII)) and
        (LConfiguration.Field('options').Field('scan').AsInteger = Ord(nrsFirst)) and
        LConfiguration.Field('options').Field('multiline').AsBoolean and
        LConfiguration.Field('options').Field('dotAll').AsBoolean and
        (LConfiguration.Field('options').Field('limits').Field('work').AsInteger = 123456),
        'Ordinary regex matching preserves flags and the user-adjusted complete-work budget');
      13: Check((LConfiguration.Field('replacement').Count = 3) and
        (LConfiguration.Field('replacement').Item(0).Field('capture').Field('name').AsText = 'last') and
        (LConfiguration.Field('replacement').Item(1).Field('literal').AsText = ', ') and
        (LConfiguration.Field('replacement').Item(2).Field('capture').Field('name').AsText = 'first') and
        (LConfiguration.Field('unmatched').AsInteger = Ord(nruError)),
        'Ordinary replacement uses typed captures/literals in the intended order');
      14: Check((LConfiguration.Field('capture').Field('index').AsInteger = 1) and
        (LConfiguration.Field('missing').AsInteger = Ord(nrxSkip)),
        'Ordinary extraction retains numeric capture identity and explicit missing policy');
    end;
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
    4:
    begin
      Change('path-part', 'profile');
      Click('path-add');
      Change('set-mode', 'Insert');
    end;
    5:
    begin
      Change('path-part', 'obsolete');
      Click('path-add');
      Change('missing-policy', 'Ignore missing');
    end;
    6:
    begin
      Change('merge-policy', 'Merge Patch (null deletes)');
    end;
    7:
    begin
      Change('path-part', 'customer');
      Click('path-add');
      Change('key-kind', 'Text');
    end;
    8:
    begin
      Change('path-part', 'id');
      Click('path-add');
      Change('key-kind', 'Text');
    end;
    9:
    begin
      Change('path-part', 'customer');
      Click('path-add');
      Change('path-side', 'Right key');
      Change('path-part', 'id');
      Click('path-add');
      Change('join-kind', 'Full');
      Change('null-keys', 'Match null');
    end;
    10:
    begin
      Change('path-part', 'amount');
      Click('path-add');
      Change('aggregation', 'Average');
      Change('rounding', 'Floor');
      Change('empty-aggregate', 'Report error');
    end;
    11:
    begin
      Change('branch-type', 'JSON');
      Change('branch-selection', 'Last available');
      Change('branch-empty', 'Report error');
      Change('branch-name', 'third');
      Click('branch-add');
      Click('branch-up');
      Click('branch-down');
      Change('confirm', 'Confirm removal');
      Click('branch-remove');
      Change('branch-choice', 'fallback');
      Click('branch-up');
      Check(GRenderer.Root.Find('editor-branch-preview').Prop('text') =
        TNyxDataValue.ParseJSON('["fallback","primary"]').ToJSON, 'Actual order/removal controls preserve intended alternatives');
    end;
    12:
    begin
      Change('regex-pattern', '(?<word>[A-Z]+)');
      Change('regex-case', 'ASCII insensitive');
      Change('regex-scan', 'First match');
      Change('regex-classes', 'ASCII shorthands');
      Change('regex-multiline', 'Each line');
      Change('regex-dotAll', 'Include line breaks');
      Change('regex-expanded', 'Ignore spaces and comments');
      OpenLimits;
      Change('regex-work', '123456');
    end;
    13:
    begin
      Change('regex-pattern', RegexNamePattern);
      Change('regex-case', 'Unicode insensitive');
      Change('regex-confirm', 'Confirm removal');
      Change('regex-part-choice', '3');
      Click('regex-part-remove');
      Click('regex-part-remove');
      Click('regex-part-remove');
      Change('regex-part-kind', 'Capture name');
      Change('regex-part-value', 'last');
      Click('regex-part-add');
      Change('regex-part-kind', 'Literal text');
      Change('regex-part-value', ', ');
      Click('regex-part-add');
      Change('regex-part-kind', 'Capture name');
      Change('regex-part-value', 'first');
      Click('regex-part-add');
      Click('regex-part-up');
      Click('regex-part-down');
      Change('regex-unmatched', 'Report error');
    end;
    14:
    begin
      Change('regex-pattern', '(a)?b');
      Change('regex-capture-kind', 'Capture number');
      Change('regex-capture', '1');
      Change('regex-missing', 'Skip row');
    end;
  end;
  {$ifdef PAS2JS}

  if AIndex = 0 then
  begin
    GRenderer.ElementFor('editor-comparison').scrollIntoView(True);
    document.body.setAttribute('data-capture-checkpoint', 'collection-filter');
  end;

  if AIndex = 6 then
  begin
    GRenderer.ElementFor('editor-merge-policy').scrollIntoView(True);
    document.body.setAttribute('data-capture-checkpoint', 'json-merge-policy');
  end;

  if AIndex = 9 then
  begin
    GRenderer.ElementFor('editor-path-side').scrollIntoView(True);
    document.body.setAttribute('data-capture-checkpoint', 'relation-join');
  end;

  if AIndex = 11 then
  begin
    GRenderer.ElementFor('editor-branch-type').scrollIntoView(True);
    document.body.setAttribute('data-capture-checkpoint', 'flow-join');
  end;

  if AIndex = 13 then
  begin
    GRenderer.ElementFor('editor-regex-replacement-open').scrollIntoView(True);
    document.body.setAttribute('data-capture-checkpoint', 'regex-replacement');
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
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14:
    begin

      if not Author(GStage) then
      begin
        Exit;
      end;
    end;
    15:
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
    16:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check(GApp.State.GetValue(NyxTextState('compact')) = '[{"name":"Ada"},{"name":"Grace"}]',
        'physical control executes filter/sort/page/map and publishes exact output');
      Edit('[{"active":true,"priority":2,"profile":{"name":"New result"}}]');
    end;
    17:
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
    18:
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
    19:
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

    if TJSDate.now - GStarted > 300000 then
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

      if GetTickCount64 - GStarted > 300000 then
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
