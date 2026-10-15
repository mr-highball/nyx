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

program nyx_http_logic_authoring_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model, nyx.codec,
  nyx.controls, nyx.root.types, nyx.state, nyx.codegen, nyx.logic, nyx.logic.http,
  nyx.logic.editor, nyx.http.types, nyx.http.editor, nyx.flow, nyx.flow.types,
  nyx.flow.declarations, nyx.scheduler, nyx.behavior, nyx.resources.loader, nyx.test.http.authoring,
  nyx.studio.session, nyx.studio.projects, nyx.studio.edits, nyx.studio.inspector,
  nyx.studio.sourcejobs
  {$ifdef NYX_HTTP_COMPANION}, nyx.generated.view{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser, nyx.render.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, Graphics, IntfGraphics, FPWritePNG,
    nyx.widgets.lcl, nyx.application.lcl, nyx.render.lcl{$endif};

type
  { Ordinary Studio source commands own accepted pair publication. The form owns
    no session/controller; this receiver retires after its renderer and commands. }
  TEditor = class
  public
    Session: TNyxStudioSession;
    Commands: TNyxSourceCommands;
    Forms: TNyxDocument;
    Captured: Boolean;
    Failure: TNyxText;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
  end;

var
  GEditor: TEditor;
  GForm: INyxCard;
  GDoc: TNyxDocument;
  GExpected: TNyxDocument;
  GBefore: TNyxText;
  GAfter: TNyxText;
  GSaved: TNyxText;
  GChecks: Integer;
  GStage: Integer;
  GStarted: Double;
  GDone: Boolean;
  GTransport: TNyxTestHTTPTransport;
  GTransportLease: INyxHTTPTransport;
  GServiceScheduler: INyxScheduler;
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GRenderer: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  GEditorHost: TJSHTMLElement;
  {$else}
  GApp: TNyxLCLApplication;
  GRenderer: TNyxLCLRenderer;
  GEditorHost: TForm;
  {$endif}

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TEditor.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LEdit: TNyxStudioDesignEdit;
begin
  try

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
  except
    on E: Exception do
    begin
      Failure := E.Message;
      GRenderer.Sync;
    end;
  end;
end;

procedure TEditor.Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
begin
  { Completion is inspected on a subsequent real UI turn. }
end;

procedure Change(const AName, AValue: TNyxText);
var
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TControl;{$endif}
begin
  LInput := GRenderer.InputFor('editor-' + AName);
  {$ifdef PAS2JS}
  if LInput is TJSHTMLSelectElement then
  begin
    TJSHTMLSelectElement(LInput).value := AValue;
    LInput.dispatchEvent(TJSEvent.new('change'));
  end
  else if LInput is TJSHTMLTextAreaElement then
  begin
    TJSHTMLTextAreaElement(LInput).value := AValue;
    LInput.dispatchEvent(TJSEvent.new('input'));
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
  else if LInput is TMemo then
  begin
    TMemo(LInput).Text := AValue;
  end
  else
  begin
    TEdit(LInput).Text := AValue;
  end;
  {$endif}
  Check(GRenderer.Root.Find('editor-' + AName).Prop('value') = AValue,
    'actual request input: ' + AName);
end;

procedure Click(const AName: TNyxText);
begin
  {$ifdef PAS2JS}
  GRenderer.ElementFor('editor-' + AName).click;
  {$else}
  TNyxLCLButton(GRenderer.ControlFor('editor-' + AName)).Click;
  {$endif}
end;

procedure RenderEditor;
begin
  FreeAndNil(GRenderer);

  if GForm <> nil then
  begin
    GEditor.Forms.RemoveRoot(NyxReusableRoot('editor'));
    GForm := nil;
  end;
  GForm := NewNyxLogicEditor('editor', GEditor.Session.Document, NyxFlowRef('Configuration'));
  GEditor.Forms.AddComponent(GForm);
  GForm.Node.Find('editor-toolbox').Configure.Value(False).Done;
  GForm.Node.Find('editor-connections').Configure.Value(False).Done;
  GForm.Node.Find('editor-removal').Configure.Value(False).Done;
  {$ifdef PAS2JS}
  GRenderer := TNyxBrowserRenderer.Create;
  GRenderer.OnEvent := @GEditor.Event;
  {$else}
  GRenderer := TNyxLCLRenderer.Create;
  GRenderer.OnEvent := GEditor.Event;
  {$endif}
  GRenderer.Render(GEditor.Forms, GForm.Node, GEditorHost);
end;

procedure CaptureNative;
{$ifndef PAS2JS}
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
{$endif}
begin
  {$ifndef PAS2JS}
  if ParamCount <> 1 then
  begin
    Exit;
  end;
  ForceDirectories(ParamStr(1));
  LBitmap := GEditorHost.GetFormImage;
  LImage := nil;
  LWriter := TFPWriterPNG.Create;
  try
    LImage := LBitmap.CreateIntfImage;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + 'http-authoring-native.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
  {$endif}
end;

procedure Step;
var
  LRequest: TNyxHTTPRequest;
  LReport: TNyxDataValue;
begin

  if Clock - GStarted > 150000 then
  begin
    raise Exception.Create('HTTP authoring controls deadline at stage ' + IntToStr(GStage));
  end;
  case GStage of
    0:
    begin
      RenderEditor;
      Change('operation', 'http-call');
      Change('step-name', 'profile-request');
      Change('http-url', 'https://example.invalid/api/profile');
      Change('http-method', 'POST');
      Change('http-body', '{"name":"English demo"}');
      Change('http-deadline', '9000');
      Change('http-maximum', '65536');
      Change('http-header-name', 'content-type');
      Change('http-header-value', 'application/json');
      Click('http-header-set');
      Check(GEditor.Failure = '', 'actual header admission succeeds');
      LRequest := CaptureNyxHTTPRequestEditor('editor-http', GRenderer.Root);
      Check((LRequest.RequestMethod.Name = 'POST') and
        (LRequest.Headers.First('content-type') = 'application/json'),
        'actual typed HTTP form captures method and individual header');
      Change('http-body-kind', 'Base64 bytes');
      Check(NyxDecodeUTF8(CaptureNyxHTTPRequestEditor('editor-http', GRenderer.Root).Body) =
        '{"name":"English demo"}', 'actual body-format change preserves request bytes');
      {$ifdef PAS2JS}
      GRenderer.ElementFor('editor-http').scrollIntoView(True);
      document.body.setAttribute('data-capture-checkpoint', 'http-configuration');
      {$else}
      GRenderer.ScrollView(0, Round(GRenderer.ViewViewport.Y.Position) +
        GRenderer.ControlFor('editor-http').ClientOrigin.Y - GEditorHost.ClientOrigin.Y);
      CaptureNative;
      {$endif}
    end;
    1:
    begin
      {$ifdef PAS2JS}
      if document.body.getAttribute('data-capture-observed') <> 'http-configuration' then
      begin
        Exit;
      end;
      {$endif}
      GBefore := EncodeNyxProject(GEditor.Session.ProjectSnapshot);
      Click('add');
      Check(GEditor.Captured and (GEditor.Failure = ''), 'physical Add captures ordinary typed HTTP edit');
    end;
    2:
    begin

      if GEditor.Commands.Busy then
      begin
        Exit;
      end;
      Check(GEditor.Commands.State = nssApplied, 'matching source worker publishes HTTP operation');
      GAfter := EncodeNyxProject(GEditor.Session.ProjectSnapshot);
      Check((GAfter <> GBefore) and
        (Pos('NewNyxHTTPCall(', GEditor.Session.ProjectSnapshot.Source) > 0),
        'source and design change together with specialized constructors');
      GEditor.Session.Undo;
      Check(EncodeNyxProject(GEditor.Session.ProjectSnapshot) = GBefore, 'actual HTTP authoring paired Undo');
      GEditor.Session.Redo;
      Check(EncodeNyxProject(GEditor.Session.ProjectSnapshot) = GAfter, 'actual HTTP authoring paired Redo');
    end;
    3:
    begin
      RenderEditor;
      GEditor.Captured := False;
      Change('operation', 'utf8-decode');
      Change('step-name', 'decode-response');
      Click('add');
      Check(GEditor.Captured, 'actual decoder Add uses the same ordinary form');
    end;
    4:
    begin

      if GEditor.Commands.Busy then
      begin
        Exit;
      end;
      Check(GEditor.Commands.State = nssApplied, 'matching source worker publishes decoder');
      Check(GEditor.Session.Document.Flows.Definition(NyxFlowRef('Configuration')).StepCount = 3,
        'actual authoring retains both managed operation kinds');
      FreeAndNil(GRenderer);
      {$ifdef PAS2JS}
      GEditorHost.style.setProperty('display', 'none');
      GHost.style.removeProperty('display');
      GApp.View.ElementFor('run-button').click;
      {$else}
      GEditorHost.Hide;
      TNyxLCLButton(GApp.View.ControlFor('run-button')).Click;
      {$endif}
    end;
    5:
    begin
      LReport := GApp.FlowRoutes.Report(0);

      if (LReport.Field('active').AsInteger > 0) or (GTransport.Calls = 0) then
      begin
        Exit;
      end;
      Check((LReport.Field('succeeded').AsInteger = 1) and (GTransport.Calls = 1),
        'exact generated application executes its saved HTTP route with explicit substituted transport');
      Check(GTransport.Last.ToData.ToJSON = HTTPAuthoringRequest.ToData.ToJSON,
        'compiled request retains exact method, duplicate headers and bytes');
      Check((GApp.State.GetValue(NyxIntegerState('http-status')) = 200) and
        GApp.State.GetValue(NyxBooleanState('http-success')) and
        (Pos('9007199254740993', GApp.State.GetValue(NyxTextState('pretty'))) > 0),
        'async outcome and decoded/formatted text publish together through ordinary live state');
      {$ifdef PAS2JS}
      Check(Pos('Fetched profile', GApp.View.ElementFor('pretty-code').textContent) > 0,
        'actual browser code control displays the graph output');
      {$else}
      Check(Pos('Fetched profile', TCustomMemo(GApp.View.ControlFor('pretty-code')).Text) > 0,
        'actual native code control displays the graph output');
      {$endif}
      Check(TNyxCodec.Encode(GDoc) = GSaved, 'runtime and editor retain independent document ownership');
      GDone := True;
    end;
  end;
  Inc(GStage);
end;

procedure Retire;
{$ifndef PAS2JS}
var
  LScheduler: INyxScheduler;
  LMonitor: INyxSchedulerMonitor;
  LUntil: Double;
{$endif}
begin
  FreeAndNil(GRenderer);
  GForm := nil;

  if GEditor <> nil then
  begin
    GEditor.Commands.Free;
    GEditor.Session.Free;
    GEditor.Forms.Free;
    FreeAndNil(GEditor);
  end;
  {$ifndef PAS2JS}
  LScheduler := GApp.Context.Scheduler;
  LMonitor := LScheduler as INyxSchedulerMonitor;
  {$endif}
  FreeAndNil(GApp);
  {$ifndef PAS2JS}
  { The fixture's inspection lease intentionally keeps this scheduler alive
    beyond the application. Close that owned lease explicitly before waiting;
    otherwise its idle worker pool remains valid by design. }
  LScheduler.Shutdown;
  {$endif}
  FreeAndNil(GDoc);
  GTransportLease := nil;
  GServiceScheduler.Shutdown;
  GServiceScheduler := nil;
  {$ifndef PAS2JS}
  LUntil := Clock + 100;
  { Checked qualification owns its workers and services their final couriers;
    product shutdown itself cancels without joining native work. }
  while (Clock < LUntil) or (LMonitor.WorkerLoad.ActiveWorkers > 0) do
  begin
    Application.ProcessMessages;
    CheckSynchronize;
    Sleep(1);

    if Clock > LUntil + 5000 then
    begin
      raise Exception.Create('Owned HTTP authoring workers did not retire');
    end;
  end;
  Check(LMonitor.WorkerLoad.ActiveWorkers = 0, 'owned native application workers retire before heap finalization');
  {$endif}
  {$ifdef PAS2JS}GEditorHost.remove;{$else}FreeAndNil(GEditorHost);{$endif}
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    Step;

    if GDone then
    begin
      document.body.setAttribute('data-http-controls', 'passed');
      document.body.setAttribute('data-http-controls-checks', IntToStr(GChecks));
      Retire;
      Exit;
    end;
    window.setTimeout(@Drive, 25);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-http-controls', 'failed');
      document.body.setAttribute('data-http-controls-error', E.Message);
      Retire;
    end;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    {$ifdef NYX_HTTP_COMPANION}
    GDoc := BuildNyxDocument;
    GExpected := HTTPAuthoringDocument;
    try
      Check(TNyxCodec.Encode(GDoc) = TNyxCodec.Encode(GExpected), 'exact compiled HTTP document companion');
    finally
      FreeAndNil(GExpected);
    end;
    {$else}
    GDoc := HTTPAuthoringDocument;
    {$endif}
    GSaved := TNyxCodec.Encode(GDoc);
    GEditor := TEditor.Create;
    GExpected := TNyxDocument.Create;
    try
      GExpected.AddPage(NewNyxPage('configuration-home'));
      GExpected.Flows.Define(NyxFlowRef('Configuration'), NewNyxFlowGraph.WithStep(
        NyxFlowStep('initial'), NewNyxJSONParse).Build);
      GEditor.Session := TNyxStudioSession.Create(NyxProjectPair(
        TNyxCodec.Encode(GExpected), TNyxCodegen.Generate(GExpected)));
    finally
      FreeAndNil(GExpected);
    end;
    GEditor.Commands := TNyxSourceCommands.Create(GEditor.Session,
      {$ifdef PAS2JS}@{$endif}GEditor.Changed);
    GEditor.Forms := TNyxDocument.Create;
    GServiceScheduler := NewNyxScheduler;
    GTransport := TNyxTestHTTPTransport.Create(GServiceScheduler);
    GTransportLease := GTransport;
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GHost);
    GHost.style.setProperty('display', 'none');
    GEditorHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GEditorHost);
    GApp := TNyxBrowserApplication.Create;
    GApp.ConfigureLogic(NewNyxHTTPLogicResolver(GTransportLease));
    GApp.Run(GDoc, GHost);
    {$else}
    GEditorHost := TForm.CreateNew(nil);
    GEditorHost.SetBounds(10, 10, 800, 1000);
    GEditorHost.Show;
    GApp := TNyxLCLApplication.Create;
    GApp.ConfigureLogic(NewNyxHTTPLogicResolver(GTransportLease));
    GApp.Mount(GDoc);
    GApp.Window.Show;
    {$endif}
    GStarted := Clock;
    {$ifdef PAS2JS}Drive;{$else}
    while not GDone do
    begin
      Application.ProcessMessages;
      CheckSynchronize;
      Step;
      Sleep(1);
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' actual HTTP authoring/application checks / substituted transport');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-http-controls', 'failed');
      document.body.setAttribute('data-http-controls-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
