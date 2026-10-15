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

program nyx_logic_table_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec,
  nyx.controls, nyx.state, nyx.collections, nyx.scheduler, nyx.behavior,
  nyx.resources.loader, nyx.logic, nyx.logic.http, nyx.flow, nyx.flow.types,
  nyx.flow.declarations, nyx.flow.execution, nyx.application.context,
  nyx.linq.execution, nyx.test.logic.table
  {$ifdef NYX_TABLE_COMPANION}, nyx.generated.view{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, Grids, Graphics, IntfGraphics,
    FPWritePNG, nyx.widgets.lcl, nyx.application.lcl{$endif};

type
  { Handwritten application callback using the public mixed-output contract.
    Its graph, provider and run belong to this consumer. No document/renderer is
    captured by a worker. Saved row-route authoring is a separate integration. }
  TConsumer = class
  public
    Context: INyxApplicationContext;
    Graph: INyxFlowGraph;
    Run: INyxQueryRun;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;

var
  GDoc: TNyxDocument;
  GExpected: TNyxDocument;
  GConsumer: TConsumer;
  GProvider: TLogicTableTransport;
  GProviderLease: INyxHTTPTransport;
  GService: INyxScheduler;
  GSaved: TNyxText;
  GBefore: TNyxText;
  GRowsBefore: INyxCollectionSnapshot;
  {$ifdef NYX_TABLE_SAVED_ROUTE}GReportBefore: TNyxDataValue;{$endif}
  GChecks: Integer;
  GStage: Integer;
  GWaiting: Boolean;
  GDone: Boolean;
  GStarted: Double;
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GOther: TNyxBrowserApplication;
  GHost: TJSHTMLElement;
  GOtherHost: TJSHTMLElement;
  {$else}
  GApp: TNyxLCLApplication;
  GOther: TNyxLCLApplication;
  {$endif}

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure TConsumer.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if (ANode.ID <> 'load') or (AEvent.Trigger <> ntClick) then
  begin
    Exit;
  end;

  if Run <> nil then
  begin
    Run.Cancel;
  end;
  Run := TNyxFlowApplication.Submit(Context, Graph, [
    NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpTrigger)),
      NyxFlowSignal)], [
    NyxFlowAssign(NyxTextState('summary'),
      NyxFlowEndpoint(NyxFlowStep('format'), NyxLogicPort(nlpText))),
    NyxFlowAssign(NyxIntegerState('status'),
      NyxFlowEndpoint(NyxFlowStep('request'), NyxHTTPPort(nhpStatus)))], [
    NyxFlowAssignRows(NyxCollection('people'),
      NyxFlowEndpoint(NyxFlowStep('profile'), NyxLogicPort(nlpValue)), LogicTableRows)],
    neAsynchronous);
end;

procedure Click;
begin
  {$ifdef PAS2JS}GApp.View.ElementFor('load').click;{$else}
  TNyxLCLButton(GApp.View.ControlFor('load')).Click;{$endif}
  {$ifdef NYX_TABLE_SAVED_ROUTE}
  Check(GApp.FlowRoutes.Report(0).Field('started').AsInteger = GStage + 1,
    'real adapter button enters the saved typed row route without a handwritten callback');
  {$else}
  Check(GConsumer.Run <> nil, 'real adapter button enters the typed runtime callback');
  {$endif}
end;

function RunPhase: TNyxQueryPhase;
{$ifdef NYX_TABLE_SAVED_ROUTE}
var
  LReport: TNyxDataValue;
{$endif}
begin
  {$ifdef NYX_TABLE_SAVED_ROUTE}
  LReport := GApp.FlowRoutes.Report(0);

  if LReport.Field('active').AsInteger > 0 then
  begin
    Exit(nqpComputing);
  end;

  if LReport.Field('failed').AsInteger > GReportBefore.Field('failed').AsInteger then
  begin
    Exit(nqpFailed);
  end;

  if LReport.Field('cancelled').AsInteger > GReportBefore.Field('cancelled').AsInteger then
  begin
    Exit(nqpCancelled);
  end;

  if LReport.Field('discarded').AsInteger > GReportBefore.Field('discarded').AsInteger then
  begin
    Exit(nqpDiscarded);
  end;
  Result := nqpSucceeded;
  {$else}
  Result := GConsumer.Run.Phase;
  {$endif}
end;

function ExpectedName(AIndex: Integer): TNyxText;
begin
  {$ifdef NYX_TABLE_EDITED_ROWS}
  Result := 'ada';

  if AIndex = 1 then
  begin
    Result := 'grace';
  end;
  {$else}
  Result := 'Ada';

  if AIndex = 1 then
  begin
    Result := 'Grace';
  end;
  {$endif}
end;

function Caption: TNyxText;
begin
  {$ifdef PAS2JS}Result := GApp.View.ElementFor('summary').textContent;{$else}
  Result := TNyxText(RawByteString(TMemo(GApp.View.ControlFor('summary')).Text));{$endif}
end;

function Cell(AColumn, ARow: Integer): TNyxText;
{$ifdef PAS2JS}
var
  LCells: TJSNodeList;
{$endif}
begin
  {$ifdef PAS2JS}
  LCells := GApp.View.ElementFor('people-table').querySelectorAll('[role="gridcell"]');
  Result := LCells.item(ARow * 2 + AColumn).textContent;
  {$else}
  Result := TNyxText(RawByteString(TStringGrid(
    GApp.View.ControlFor('people-table')).Cells[AColumn, ARow + 1]));
  {$endif}
end;

procedure Capture;
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
  LBitmap := GApp.Window.GetFormImage;
  LWriter := TFPWriterPNG.Create;
  LImage := nil;
  try
    LImage := LBitmap.CreateIntfImage;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + 'logic-table-native.png', LWriter);
  finally
    LImage.Free;
    LWriter.Free;
    LBitmap.Free;
  end;
  {$endif}
end;

procedure Step;
var
  LRows: INyxCollectionSnapshot;
begin

  if Clock - GStarted > 30000 then
  begin
    raise Exception.Create('Actual Logic Table deadline at stage ' + IntToStr(GStage));
  end;

  if not GWaiting then
  begin

    if GStage = 5 then
    begin
      Capture;
      GDone := True;
      Exit;
    end;
    GBefore := GApp.State.GetValue(NyxTextState('summary'));
    {$ifdef NYX_TABLE_SAVED_ROUTE}GReportBefore := GApp.FlowRoutes.Report(0);{$endif}
    GRowsBefore := GApp.Collections.Collection(NyxCollection('people')).Snapshot;
    GProvider.Payload := LogicTablePayload;

    if GStage = 1 then
    begin
      GProvider.Payload :=
        '{"items":[{"active":true,"profile":{"id":"wrong","name":17,"rank":4}}]}';
    end;
    Click;

    if GStage = 2 then
    begin
      GApp.State.SetValue(NyxTextState('summary'), 'User edit wins');
    end;

    if GStage = 3 then
    begin
      {$ifdef NYX_TABLE_SAVED_ROUTE}GApp.FlowRoutes.Cancel(0);{$else}GConsumer.Run.Cancel;{$endif}
    end;
    GWaiting := True;
    Exit;
  end;

  if RunPhase in [nqpPending, nqpComputing, nqpAwaitingUI] then
  begin
    Exit;
  end;
  LRows := GApp.Collections.Collection(NyxCollection('people')).Snapshot;
  case GStage of
    0, 4:
    begin
      Check(RunPhase = nqpSucceeded, 'complete mixed run succeeds');
      Check((LRows.Count = 2) and (LRows.ItemAt(0).Ref.ID = 'ada') and
        (LRows.ItemAt(1).Ref.ID = 'grace'), 'filter, exact sort and map retain stable row identity');
      Check((Cell(0, 0) = ExpectedName(0)) and (Cell(0, 1) = ExpectedName(1)),
        'actual Table paints the authored field path in sorted rows');
      Check(Cell(1, 1) = '9007199254740993', 'actual Table preserves exact rank above IEEE integer range');
      Check(LRows.ItemAt(0).GetValue(NyxBooleanField('ready')) and
        (LRows.ItemAt(0).GetValue(NyxIntegerField('count')) = 3) and
        (LRows.ItemAt(0).GetValue(NyxNumberField('ratio')) = 1.25),
        'saved typed rows preserve Boolean, Integer and Number through the real application');
      Check(LRows.ItemAt(0).GetValue(NyxTextField('note')) = 'English note',
        'generated structural mapping retains supplementary and NUL JSON member names');
      Check(Pos('Grace', Caption) > 0, 'actual code control paints the same published frame');
      Check(GApp.State.GetValue(NyxIntegerState('status')) = 200, 'HTTP status publishes alongside rows');
    end;
    1, 2, 3:
    begin
      Check(RunPhase in [nqpFailed, nqpCancelled, nqpDiscarded],
        'invalid, stale or cancelled run refuses');
      Check((LRows.Revision = GRowsBefore.Revision) and
        (LRows.ItemAt(0).GetValue(NyxTextField('name')) = ExpectedName(0)), 'rejected run preserves rows');

      if GStage = 2 then
      begin
        Check(GApp.State.GetValue(NyxTextState('summary')) = 'User edit wins',
          'stale result preserves the newer user text');
      end
      else
      begin
        Check(GApp.State.GetValue(NyxTextState('summary')) = GBefore,
          'invalid/cancelled rows never publish companion state');
      end;
      Check(Cell(0, 0) = ExpectedName(0), 'actual table keeps the old visible row after refusal');
    end;
  end;
  Check((GOther.Collections.Collection(NyxCollection('people')).Snapshot.Count = 0) and
    (GOther.State.GetValue(NyxTextState('summary')) = 'Ready to load'),
    'a second application retains independent runtime stores');
  Check(TNyxCodec.Encode(GDoc) = GSaved, 'runtime execution never changes the saved document');
  GConsumer.Run := nil;
  GWaiting := False;
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
  {$ifndef PAS2JS}
  LScheduler := GApp.Context.Scheduler;
  LMonitor := LScheduler as INyxSchedulerMonitor;
  {$endif}
  GConsumer.Context := nil;
  GConsumer.Run := nil;
  GConsumer.Free;
  FreeAndNil(GOther);
  FreeAndNil(GApp);
  GProviderLease := nil;
  GService.Shutdown;
  GService := nil;
  FreeAndNil(GDoc);
  {$ifndef PAS2JS}
  LScheduler.Shutdown;
  LUntil := Clock + 100;
  while (Clock < LUntil) or (LMonitor.WorkerLoad.ActiveWorkers > 0) do
  begin
    Application.ProcessMessages;
    CheckSynchronize;
    Sleep(1);

    if Clock > LUntil + 5000 then
    begin
      raise Exception.Create('Owned table workers did not retire');
    end;
  end;
  Check(LMonitor.WorkerLoad.ActiveWorkers = 0, 'owned native workers retire before heap finalization');
  {$endif}
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    Step;

    if GDone then
    begin
      document.body.setAttribute('data-logic-table-controls', 'passed');
      document.body.setAttribute('data-logic-table-controls-checks', IntToStr(GChecks));
      Exit;
    end;
    window.setTimeout(@Drive, 5);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-logic-table-controls', 'failed');
      document.body.setAttribute('data-logic-table-controls-error', E.Message);
    end;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    {$ifdef NYX_TABLE_COMPANION}
    GDoc := BuildNyxDocument;
    {$ifdef NYX_TABLE_EDITED_ROWS}GExpected := LogicTableEditedDocument;{$else}
    {$ifdef NYX_TABLE_SAVED_ROUTE}GExpected := LogicTableRoutedDocument;{$else}
    GExpected := LogicTableDocument;{$endif}
    {$endif}
    try
      Check(TNyxCodec.Encode(GDoc) = TNyxCodec.Encode(GExpected), 'exact compiled Table document');
    finally
      FreeAndNil(GExpected);
    end;
    {$else}
    {$ifdef NYX_TABLE_SAVED_ROUTE}GDoc := LogicTableRoutedDocument;{$else}
    GDoc := LogicTableDocument;{$endif}
    {$endif}
    GSaved := TNyxCodec.Encode(GDoc);
    GService := NewNyxScheduler;
    GProvider := TLogicTableTransport.Create(GService);
    GProviderLease := GProvider;
    GConsumer := TConsumer.Create;
    GConsumer.Graph := GDoc.Flows.Definition(NyxFlowRef('Team')).Build(
      NewNyxHTTPLogicResolver(GProviderLease));
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GHost);
    GOtherHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GOtherHost);
    GOtherHost.style.setProperty('display', 'none');
    GApp := TNyxBrowserApplication.Create;
    GOther := TNyxBrowserApplication.Create;
    {$ifdef NYX_TABLE_SAVED_ROUTE}
    GApp.ConfigureLogic(NewNyxHTTPLogicResolver(GProviderLease));
    GOther.ConfigureLogic(NewNyxHTTPLogicResolver(GProviderLease));
    {$endif}
    GApp.Run(GDoc, GHost);
    GOther.Run(GDoc, GOtherHost);
    {$ifndef NYX_TABLE_SAVED_ROUTE}GApp.View.OnEvent := @GConsumer.Event;{$endif}
    {$else}
    GApp := TNyxLCLApplication.Create;
    GOther := TNyxLCLApplication.Create;
    {$ifdef NYX_TABLE_SAVED_ROUTE}
    GApp.ConfigureLogic(NewNyxHTTPLogicResolver(GProviderLease));
    GOther.ConfigureLogic(NewNyxHTTPLogicResolver(GProviderLease));
    {$endif}
    GApp.Mount(GDoc);
    GOther.Mount(GDoc);
    GApp.Window.SetBounds(20, 20, 780, 750);
    GApp.Window.Show;
    {$ifndef NYX_TABLE_SAVED_ROUTE}GApp.View.OnEvent := GConsumer.Event;{$endif}
    {$endif}
    GConsumer.Context := GApp.Context;
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
    WriteLn('PASS ', GChecks, ' actual connected Table checks / substituted HTTP');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-logic-table-controls', 'failed');
      document.body.setAttribute('data-logic-table-controls-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
