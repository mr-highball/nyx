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
program nyx_resource_http_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.data,
  nyx.state, nyx.collections, nyx.scheduler, nyx.resource.sources,
  nyx.application.resources, nyx.resource.workshop, nyx.logic.http,
  nyx.canvas.types, nyx.canvas.workspace, nyx.flow.routing
  {$ifdef NYX_COMPILED_RESOURCE_HTTP}, nyx.generated.view{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser, nyx.canvas.browser,
    nyx.resources.http.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, Grids, Graphics,
    IntfGraphics, FPWritePNG, nyx.application.lcl, nyx.canvas.lcl,
    nyx.resources.http.lcl{$endif};

type
  {$ifdef PAS2JS}
  { Independent browser application owns transports and its revocable UI scope. }
  TReviewApplication = TNyxBrowserApplication;
  { Borrowed embedded renderers remain valid only while their application lives. }
  TReviewCanvas = INyxBrowserCanvasWorkspaceView;
  {$else}
  { Independent native application owns transports and its revocable UI scope. }
  TReviewApplication = TNyxLCLApplication;
  { Borrowed embedded renderers remain valid only while their application lives. }
  TReviewCanvas = INyxLCLCanvasWorkspaceView;
  {$endif}

var
  GDocument: TNyxDocument;
  GApplications: array[0..1] of TReviewApplication;
  GViews: array of TReviewCanvas;
  GRoutes: INyxFlowRouteBinding;
  GHostedRoute: Integer;
  GHTTPRoute: Integer;
  GStage: Integer;
  GChecks: Integer;
  GStarted: Double;
  GDone: Boolean;
  GBaseURL: TNyxText;
  GSaved: TNyxText;
  GSummary: TNyxText;
  GRowRevision: Integer;
  GAttempt: Integer;

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

procedure ClickID(const AID: TNyxText);
begin
  {$ifdef PAS2JS}
  GViews[0].RendererFor(ResourceWorkshopItem).ElementFor(AID).click;
  {$else}
  TButton(GViews[0].RendererFor(ResourceWorkshopItem).ControlFor(AID)).Click;
  {$endif}
end;

procedure Click(AControl: TResourceWorkshopControl);
begin
  ClickID(ResourceWorkshopControl(AControl).ID);
end;

function Cell(AColumn, ARow: Integer): TNyxText;
{$ifdef PAS2JS}
var
  LCells: TJSNodeList;
{$endif}
begin
  {$ifdef PAS2JS}
  LCells := GViews[0].RendererFor(ResourceWorkshopItem)
    .ElementFor(ResourceWorkshopControl(rwcTable).ID).querySelectorAll('[role="gridcell"]');
  Result := LCells.item(ARow * 2 + AColumn).textContent;
  {$else}
  Result := TNyxText(RawByteString(TStringGrid(GViews[0].RendererFor(ResourceWorkshopItem)
    .ControlFor(ResourceWorkshopControl(rwcTable).ID)).Cells[AColumn, ARow + 1]));
  {$endif}
end;

procedure CheckRows(const AGrace: TNyxText = 'Grace');
begin
  Check((Cell(0, 0) = 'Ada') and (Cell(0, 1) = AGrace),
    'painted Table shows filtered exact-number ordering: ' + Cell(0, 0) + ' / ' + Cell(0, 1));
  Check((Cell(1, 0) = '9007199254740992') and (Cell(1, 1) = '9007199254740993'),
    'real response preserves neighboring integers above IEEE exact range');
  Check(GApplications[1].Collections.Collection(NyxCollection('team')).Snapshot.Count = 0,
    'real transport publication keeps the peer application independent');
  Check(TNyxCodec.Encode(GDocument) = GSaved,
    'network activity never changes the authored reusable or its saved defaults');
end;

function Report(ARoute: Integer): TNyxDataValue;
begin
  Result := GRoutes.Report(ARoute);
end;

function Finished(ARoute: Integer): Boolean;
var
  LReport: TNyxDataValue;
begin
  LReport := Report(ARoute);
  Result := (LReport.Field('started').AsInteger >= GAttempt) and
    (LReport.Field('active').AsInteger = 0);
end;

procedure StartHTTP(const AFile: TNyxText);
var
  LURL: TNyxText;
begin
  LURL := GBaseURL + AFile;
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GViews[0].InputFor(ResourceWorkshopItem,
    ResourceWorkshopControl(rwcURL))).value := LURL;
  TJSHTMLElement(GViews[0].InputFor(ResourceWorkshopItem,
    ResourceWorkshopControl(rwcURL))).dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(GViews[0].InputFor(ResourceWorkshopItem,
    ResourceWorkshopControl(rwcURL))).Text := LURL;
  {$endif}
  Check(GApplications[0].State.GetValue(ResourceWorkshopURL) = LURL,
    'actual URL input updates its typed state before invocation');
  GAttempt := Report(GHTTPRoute).Field('started').AsInteger + 1;
  Click(rwcHTTP);
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
  ForceDirectories(ParamStr(1));
  LBitmap := GApplications[0].Window.GetFormImage;
  LImage := LBitmap.CreateIntfImage;
  LWriter := TFPWriterPNG.Create;
  try
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + 'resource-http.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
  {$endif}
end;

procedure Step;
var
  LRuntime: INyxCanvasRuntimeView;
  LIndex: Integer;
  LHeader: TNyxText;
begin

  if Clock - GStarted > 90000 then
  begin
    raise Exception.Create('Actual Resource/HTTP deadline at stage ' + IntToStr(GStage));
  end;
  case GStage of
    0:
      begin

        if (GViews[0].MountedCount = 0) or (GViews[1].MountedCount = 0) then
        begin
          Exit;
        end;
        Check(Supports(GViews[0], INyxCanvasRuntimeView, LRuntime),
          'actual embedded controls expose their revocable route binding');
        GRoutes := LRuntime.BehaviorFor(ResourceWorkshopItem).Routes;
        GHostedRoute := -1;
        GHTTPRoute := -1;
        for LIndex := 0 to GRoutes.Count - 1 do
        begin

          if Report(LIndex).Field('route').AsText = 'Load hosted team' then
          begin
            GHostedRoute := LIndex;
          end;

          if Report(LIndex).Field('route').AsText = 'Load team URL' then
          begin
            GHTTPRoute := LIndex;
          end;
        end;
        Check((GHostedRoute >= 0) and (GHTTPRoute >= 0),
          'authored resource and direct HTTP routes are both mounted');
        LHeader := GViews[0].RendererFor(ResourceWorkshopItem).Root
          .Find(ResourceWorkshopControl(rwcHTTPSection).ID).Part(NyxPart('header')).ID;
        ClickID(LHeader);
        Click(rwcEmbedded);
        Inc(GStage);
      end;
    1:
      begin

        if GApplications[0].Collections.Collection(NyxCollection('team')).Snapshot.Count <> 2 then
        begin
          Exit;
        end;
        {$ifndef PAS2JS}

        if Cell(0, 0) = '' then
        begin
          Exit;
        end;
        {$endif}
        CheckRows;
        GAttempt := Report(GHostedRoute).Field('started').AsInteger + 1;
        Click(rwcHosted);
        Inc(GStage);
      end;
    2:
      begin

        if not Finished(GHostedRoute) then
        begin
          Exit;
        end;
        Check(Report(GHostedRoute).Field('succeeded').AsInteger = 1,
          'declared hosted resource reaches the real platform HTTP transport');
        CheckRows;
        StartHTTP('team.json');
        Inc(GStage);
      end;
    3:
      begin

        if not Finished(GHTTPRoute) then
        begin
          Exit;
        end;
        Check(Report(GHTTPRoute).Field('succeeded').AsInteger = 1,
          'HTTP bytes decode, parse, filter, sort and publish through the authored graph');
        Check(GApplications[0].State.GetValue(ResourceWorkshopHTTPStatus) = 200,
          'actual successful HTTP status reaches typed integer state: ' +
          IntToStr(GApplications[0].State.GetValue(ResourceWorkshopHTTPStatus)) + ' / ' +
          GApplications[0].State.GetValue(NyxTextState('team-error')));
        CheckRows;
        GSummary := GApplications[0].State.GetValue(NyxTextState('team-summary'));
        GRowRevision := GApplications[0].Collections.Collection(NyxCollection('team')).Snapshot.Revision;
        StartHTTP('invalid.json');
        Inc(GStage);
      end;
    4, 5:
      begin

        if not Finished(GHTTPRoute) then
        begin
          Exit;
        end;
        Check(Report(GHTTPRoute).Field('failed').AsInteger = GStage - 3,
          'invalid JSON or row family refuses the complete result frame');
        Check(GApplications[0].Collections.Collection(NyxCollection('team')).Snapshot.Revision = GRowRevision,
          'invalid input publishes no partial collection');
        Check(GApplications[0].State.GetValue(NyxTextState('team-summary')) = GSummary,
          'invalid input retains the accepted companion summary');
        CheckRows;

        if GStage = 4 then
        begin
          StartHTTP('wrong-rows.json');
        end
        else
        begin
          StartHTTP('missing.json');
        end;
        Inc(GStage);
      end;
    6:
      begin

        if not Finished(GHTTPRoute) then
        begin
          Exit;
        end;
        Check(GApplications[0].State.GetValue(ResourceWorkshopHTTPStatus) = 404,
          'actual unsuccessful HTTP status remains inspectable without parsing its error body');
        Check(GApplications[0].Collections.Collection(NyxCollection('team')).Snapshot.Revision = GRowRevision,
          'unsuccessful HTTP preserves the accepted collection');
        Check(GApplications[0].State.GetValue(NyxTextState('team-summary')) = GSummary,
          'unsuccessful HTTP preserves the accepted summary');
        CheckRows;
        StartHTTP('unicode.json');
        Inc(GStage);
      end;
    7:
      begin

        if not Finished(GHTTPRoute) then
        begin
          Exit;
        end;
        Check(GApplications[0].State.GetValue(ResourceWorkshopHTTPStatus) = 200,
          'a later successful URL recovers after both parse and HTTP failures');
        CheckRows('Grace 🌙');
        Capture;
        StartHTTP('team.json');
        Check(Report(GHTTPRoute).Field('active').AsInteger = 1,
          'the real HTTP invocation is admitted before a newer edit');
        GApplications[0].State.SetValue(NyxTextState('team-summary'), 'My newer network note');
        GRowRevision := GApplications[0].Collections.Collection(NyxCollection('team')).Snapshot.Revision;
        Inc(GStage);
      end;
    8:
      begin

        if not Finished(GHTTPRoute) then
        begin
          Exit;
        end;
        Check(GApplications[0].State.GetValue(NyxTextState('team-summary')) = 'My newer network note',
          'a late real HTTP result cannot overwrite a newer user edit');
        Check(GApplications[0].Collections.Collection(NyxCollection('team')).Snapshot.Revision = GRowRevision,
          'stale mixed HTTP publication preserves accepted rows');
        CheckRows('Grace 🌙');
        StartHTTP('team.json');
        Check(Report(GHTTPRoute).Field('active').AsInteger = 1,
          'another real HTTP invocation is admitted before navigation');
        GApplications[0].ShowPage('away');
        GViews[0] := nil;
        Inc(GStage);
      end;
    9:
      begin

        if not Finished(GHTTPRoute) then
        begin
          Exit;
        end;
        Check(Report(GHTTPRoute).Field('closed').AsBoolean,
          'navigation revokes the embedded HTTP route binding');
        Check(GApplications[0].State.GetValue(NyxTextState('team-summary')) = 'My newer network note',
          'retired real HTTP work retains the last accepted summary');
        GDone := True;
      end;
  end;
end;

procedure Retire;
var
  LIndex: Integer;
  {$ifndef PAS2JS}
  LScheduler: INyxScheduler;
  LMonitor: INyxSchedulerMonitor;
  LUntil: Double;
  {$endif}
begin
  GRoutes := nil;
  for LIndex := 0 to 1 do
  begin

    if LIndex < Length(GViews) then
    begin
      GViews[LIndex] := nil;
    end;
    {$ifndef PAS2JS}
    LScheduler := nil;

    if GApplications[LIndex] <> nil then
    begin
      LScheduler := GApplications[LIndex].Context.Scheduler;
    end;
    {$endif}
    FreeAndNil(GApplications[LIndex]);
    {$ifndef PAS2JS}

    if LScheduler <> nil then
    begin
      LMonitor := LScheduler as INyxSchedulerMonitor;
      LScheduler.Shutdown;
      LUntil := Clock + 5000;
      while LMonitor.WorkerLoad.ActiveWorkers > 0 do
      begin
        Application.ProcessMessages;
        CheckSynchronize;
        Sleep(1);

        if Clock > LUntil then
        begin
          raise Exception.Create('Actual HTTP workers did not retire');
        end;
      end;
    end;
    {$endif}
  end;
  FreeAndNil(GDocument);
end;

procedure InitializeApplications;
var
  LIndex: Integer;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  {$endif}
begin
  {$ifdef PAS2JS}
  GBaseURL := window.location.origin + '/resource-http-01/data/';
  {$else}
  GBaseURL := ParamStr(3);
  Check(GBaseURL <> '', 'native real-network test requires its explicit fixture URL');
  {$endif}
  {$ifdef NYX_COMPILED_RESOURCE_HTTP}
  GDocument := BuildNyxDocument;
  {$else}
  GDocument := BuildResourceWorkshopDocument(NyxResourceURL(GBaseURL + 'team.json'));
  {$endif}
  GSaved := TNyxCodec.Encode(GDocument);
  SetLength(GViews, 2);
  for LIndex := 0 to 1 do
  begin
    GApplications[LIndex] := TReviewApplication.Create;
    GApplications[LIndex].ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand));
    {$ifdef PAS2JS}
    GApplications[LIndex].ConfigureLogic(NewNyxHTTPLogicResolver(NewNyxBrowserHTTPTransport));
    LHost := TJSHTMLElement(document.createElement('main'));
    document.body.appendChild(LHost);
    GApplications[LIndex].Run(GDocument, LHost);
    {$else}
    GApplications[LIndex].ConfigureLogic(NewNyxHTTPLogicResolver(
      NewNyxNativeHTTPTransport(GApplications[LIndex].View.Events.Scheduler)));
    GApplications[LIndex].Mount(GDocument);
    GApplications[LIndex].Window.ClientWidth := StrToIntDef(ParamStr(2), 1280);
    GApplications[LIndex].Window.ClientHeight := 900;
    GApplications[LIndex].Window.Show;
    {$endif}
    GViews[LIndex] := GApplications[LIndex].View.CanvasView(
      GApplications[LIndex].View.Root.Find(ResourceWorkshopCanvas.ID)
        .Part(NyxPart('workspace')).ID) as TReviewCanvas;
  end;
  GStarted := Clock;
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    Step;

    if GDone then
    begin
      Retire;
      document.body.setAttribute('data-resource-http', 'passed');
      document.body.setAttribute('data-test-checks', IntToStr(GChecks));
      document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' actual Resource/HTTP control checks';
      Exit;
    end;
    window.setTimeout(@Drive, 5);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-resource-http', 'failed');
      document.body.textContent := E.Message + ' / stage ' + IntToStr(GStage);
      Retire;
    end;
    else
    begin
      document.body.setAttribute('data-resource-http', 'failed');
      document.body.textContent := String(JSExceptValue) + ' / stage ' + IntToStr(GStage);
      Retire;
    end;
  end;
end;
{$endif}

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  try
    InitializeApplications;
    {$ifdef PAS2JS}Drive;{$else}
    while not GDone do
    begin
      Application.ProcessMessages;
      CheckSynchronize;
      Step;
      Sleep(5);
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' actual Resource/HTTP control checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-resource-http', 'failed');
      document.body.textContent := E.Message + ' / admission';
      {$else}
      WriteLn('FAIL ', E.Message, ' / stage ', GStage);
      Retire;
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
