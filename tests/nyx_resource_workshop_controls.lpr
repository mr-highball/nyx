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
program nyx_resource_workshop_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.types, nyx.bytes, nyx.data, nyx.model,
  nyx.state, nyx.collections, nyx.resource.sources, nyx.resource.cache,
  nyx.resources, nyx.resources.loader, nyx.application.resources,
  nyx.resource.workshop, nyx.scheduler, nyx.codec, nyx.canvas.types,
  nyx.canvas.workspace, nyx.flow.routing, nyx.tool.canvas
  {$ifdef NYX_RESOURCE_WORKSHOP_STUDIO}, nyx.test.json.workshop.studio,
    nyx.studio.projects{$endif}
  {$ifdef NYX_COMPILED_RESOURCE_WORKSHOP}, nyx.generated.view{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser, nyx.canvas.browser,
    nyx.tool.canvas.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, Grids, Graphics,
    IntfGraphics, FPWritePNG, nyx.application.lcl, nyx.canvas.lcl,
    nyx.tool.canvas.lcl{$endif};

type
  {$ifdef PAS2JS}
  { Owns one ordinary application; borrowed embedded controls retire with it. }
  TReviewApplication = TNyxBrowserApplication;
  { Retained workspace facet borrows item renderers until application teardown. }
  TReviewCanvas = INyxBrowserCanvasWorkspaceView;
  { Borrow the independently owned inspector renderer without creating a face. }
  TReviewTools = INyxBrowserToolCanvasView;
  {$else}
  { Owns one ordinary application; borrowed embedded controls retire with it. }
  TReviewApplication = TNyxLCLApplication;
  { Retained workspace facet borrows item renderers until application teardown. }
  TReviewCanvas = INyxLCLCanvasWorkspaceView;
  { Borrow the independently owned inspector renderer without creating a face. }
  TReviewTools = INyxLCLToolCanvasView;
  {$endif}

  { Controlled response remains revocable through the ordinary request base.
    It exercises graph/resource/publication ownership, not real network I/O. }
  TRequest = class(TNyxResourceRequest)
  public
    procedure Deliver(const AJSON, AError: TNyxText);
  end;

  { One explicit byte host is shared by independent applications. Delayed replies
    let actual controls race new edits and navigation without timing guesses. }
  TTransport = class(TInterfacedObject, INyxResourceTransport)
  public
    Calls: Integer;
    Pending: TRequest;
    Lease: INyxResourceRequest;
    function Request(const AURL: TNyxResourceURL;
      const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
      AReply: TNyxResourceHTTPReply): INyxResourceRequest;
    procedure Deliver(const AJSON, AError: TNyxText);
  end;

var
  {$ifdef NYX_RESOURCE_WORKSHOP_STUDIO}
  GStudioJourney: INyxToolWorkshopStudioJourney;
  {$endif}
  GInitialized: Boolean;
  GDocument: TNyxDocument;
  GApplications: array[0..1] of TReviewApplication;
  GViews: array of TReviewCanvas;
  GTransport: TTransport;
  GTransportLease: INyxResourceTransport;
  GResolver: INyxResourceResolver;
  GStage: Integer;
  GChecks: Integer;
  GStarted: Double;
  GRoutes: INyxFlowRouteBinding;
  GHostedRoute: Integer;
  GDone: Boolean;
  GSaved: TNyxText;
  GBefore: TNyxText;
  GRowRevision: Integer;

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

procedure TRequest.Deliver(const AJSON, AError: TNyxText);
var
  LResponse: TNyxResourceHTTPResult;
begin
  LResponse := Default(TNyxResourceHTTPResult);
  LResponse.Status := 200;
  LResponse.Bytes := NyxEncodeUTF8(AJSON);
  LResponse.Error := AError;
  LResponse.Hints := NyxResourceCacheHeaders('no-store', '');
  Complete(LResponse);
end;

function TTransport.Request(const AURL: TNyxResourceURL;
  const AOptions: TNyxResourceLoadOptions; AMaximumBytes: Integer;
  AReply: TNyxResourceHTTPReply): INyxResourceRequest;
begin
  Check(Pending = nil, 'the previous controlled response is retired');
  Inc(Calls);
  Pending := TRequest.Create(AReply);
  Result := Pending;
  Lease := Result;
end;

procedure TTransport.Deliver(const AJSON, AError: TNyxText);
var
  LRequest: TRequest;
  LLease: INyxResourceRequest;
begin
  LRequest := Pending;
  LLease := Lease;
  Pending := nil;
  Lease := nil;
  LRequest.Deliver(AJSON, AError);
end;

procedure Click(AControl: TResourceWorkshopControl);
begin
  {$ifdef PAS2JS}
  GViews[0].RendererFor(ResourceWorkshopItem).ElementFor(ResourceWorkshopControl(AControl).ID).click;
  {$else}
  TButton(GViews[0].RendererFor(ResourceWorkshopItem).ControlFor(ResourceWorkshopControl(AControl).ID)).Click;
  {$endif}
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

function HostedReport: TNyxDataValue;
begin
  Result := GRoutes.Report(GHostedRoute);
end;

procedure CheckRows;
begin
  Check((Cell(0, 0) = 'Ada') and (Cell(0, 1) = 'Grace'),
    'embedded Table paints only active people in exact numeric order: ' + Cell(0, 0) + ' / ' + Cell(0, 1));
  Check(Cell(1, 1) = '9007199254740993', 'embedded Table retains an integer above IEEE exact range');
  Check(GApplications[1].Collections.Collection(NyxCollection('team')).Snapshot.Count = 0,
    'peer application retains independently empty runtime data');
  Check(TNyxCodec.Encode(GDocument) = GSaved, 'runtime resource input never edits authored defaults');
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

  if ParamCount < 1 then
  begin
    Exit;
  end;
  ForceDirectories(ParamStr(1));
  LBitmap := GApplications[0].Window.GetFormImage;
  LImage := nil;
  LWriter := TFPWriterPNG.Create;
  try
    LImage := LBitmap.CreateIntfImage;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + 'resource-workshop.png', LWriter);
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
  LRoute: Integer;
  LTools: INyxToolCanvasView;
  LTarget: TReviewTools;
  LRootID: TNyxText;
begin

  if Clock - GStarted > 45000 then
  begin
    raise Exception.Create('Resource Workshop deadline at stage ' + IntToStr(GStage));
  end;
  case GStage of
    0:
      begin

        if (GViews[0].MountedCount = 0) or (GViews[1].MountedCount = 0) then
        begin
          Exit;
        end;
        LTools := GApplications[0].View.ToolCanvasView(ResourceWorkshopCanvas.ID);
        Check(Supports(LTools, TReviewTools, LTarget), 'actual application exposes its managed Tool inspector');
        LRootID := LTools.Inspector.Root.ID;
        Check(LTarget.Renderer.Root.Find(LRootID + '-graphs').Prop(NyxAttributeName(atEnabled)) = 'true',
          'initial mounted attachment enables the actual graph switch without manual Refresh');
        Check(Pos('3 routes', LTarget.Renderer.Root.Find(LRootID + '-status')
          .Prop(NyxAttributeName(atText))) = 1, 'initial inspector describes the mounted route binding');
        Check(Supports(GViews[0], INyxCanvasRuntimeView, LRuntime),
          'canvas exposes its optional runtime inspection capability');
        GRoutes := LRuntime.BehaviorFor(ResourceWorkshopItem).Routes;
        GHostedRoute := -1;
        for LRoute := 0 to GRoutes.Count - 1 do
        begin

          if GRoutes.Report(LRoute).Field('route').AsText = 'Load hosted team' then
          begin
            GHostedRoute := LRoute;
          end;
        end;
        Check(GHostedRoute >= 0, 'embedded reusable exposes its ordinary saved route binding');
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
        Check(GTransport.Calls = 0, 'embedded resource event starts no HTTP request');
        Click(rwcHosted);
        Inc(GStage);
      end;
    2:
      begin

        if GTransport.Pending = nil then
        begin
          Exit;
        end;
        GBefore := GApplications[0].State.GetValue(NyxTextState('team-summary'));
        GRowRevision := GApplications[0].Collections.Collection(NyxCollection('team')).Snapshot.Revision;
        GTransport.Deliver('{"items":[{"id":"wrong","name":17,"active":true,"rank":4}]}', '');
        Inc(GStage);
      end;
    3:
      begin

        if HostedReport.Field('active').AsInteger > 0 then
        begin
          Exit;
        end;
        Check(HostedReport.Field('failed').AsInteger = 1, 'actual route records failed row admission');
        Check(GApplications[0].Collections.Collection(NyxCollection('team')).Snapshot.Revision = GRowRevision,
          'wrong row family never publishes a partial collection');
        Check(GApplications[0].State.GetValue(NyxTextState('team-summary')) = GBefore,
          'row admission failure preserves companion state');
        CheckRows;
        Click(rwcHosted);
        Inc(GStage);
      end;
    4:
      begin

        if GTransport.Pending = nil then
        begin
          Exit;
        end;
        GTransport.Deliver(ResourceWorkshopSample, '');
        Inc(GStage);
      end;
    5:
      begin

        if HostedReport.Field('active').AsInteger > 0 then
        begin
          Exit;
        end;
        Check(HostedReport.Field('succeeded').AsInteger = 1, 'actual hosted route completes successfully');
        CheckRows;
        Check(GTransport.Calls = 2, 'hosted controls use the injected shared resource resolver');
        Click(rwcHosted);
        Inc(GStage);
      end;
    6:
      begin

        if GTransport.Pending = nil then
        begin
          Exit;
        end;
        GApplications[0].State.SetValue(NyxTextState('team-summary'), 'My newer note');
        GRowRevision := GApplications[0].Collections.Collection(NyxCollection('team')).Snapshot.Revision;
        GTransport.Deliver(ResourceWorkshopSample, '');
        Inc(GStage);
      end;
    7:
      begin

        if HostedReport.Field('active').AsInteger > 0 then
        begin
          Exit;
        end;
        Check((HostedReport.Field('discarded').AsInteger +
          HostedReport.Field('cancelled').AsInteger = 1) or
          ((HostedReport.Field('failed').AsInteger = 2) and
           (Pos('State update uses a stale or invalid revision',
             HostedReport.Field('failure').AsText) > 0)),
          'actual route records stale-publication refusal: ' + HostedReport.ToJSON);
        Check(GApplications[0].Collections.Collection(NyxCollection('team')).Snapshot.Revision = GRowRevision,
          'stale mixed publication never changes rows before refusing companion state');
        Check(GApplications[0].State.GetValue(NyxTextState('team-summary')) = 'My newer note',
          'late hosted result preserves a newer user edit');
        CheckRows;
        Click(rwcHosted);
        Inc(GStage);
      end;
    8:
      begin

        if GTransport.Pending = nil then
        begin
          Exit;
        end;
        GTransport.Deliver('', 'The team service is offline');
        Inc(GStage);
      end;
    9:
      begin

        if GApplications[0].State.GetValue(NyxTextState('team-error')) = '' then
        begin
          Exit;
        end;
        CheckRows;
        Check(GApplications[0].State.GetValue(NyxTextState('team-summary')) = 'My newer note',
          'failed loading reports an error while retaining accepted summary and rows');
        GApplications[0].Resources.Localize(NyxLocale('en-US'), NyxDefaultLocale);
        CheckRows;
        Check(GApplications[1].Resources.Context.Locale.Name = NyxDefaultLocale.Name,
          'coordinated resource localization retains the peer application locale');
        Capture;
        Click(rwcHosted);
        Inc(GStage);
      end;
    10:
      begin

        if GTransport.Pending = nil then
        begin
          Exit;
        end;
        GApplications[0].ShowPage('away');
        GViews[0] := nil;
        GBefore := GApplications[0].State.GetValue(NyxTextState('team-summary'));
        GTransport.Deliver(ResourceWorkshopSample, '');
        Inc(GStage);
      end;
    11:
      begin

        if HostedReport.Field('active').AsInteger > 0 then
        begin
          Exit;
        end;
        Check(HostedReport.Field('closed').AsBoolean, 'retired embedded route binding is closed');
        Check(GApplications[0].State.GetValue(NyxTextState('team-summary')) = GBefore,
          'navigation revokes publication from the retired resource graph');
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
  {$ifdef NYX_RESOURCE_WORKSHOP_STUDIO}
  GStudioJourney := nil;
  {$endif}
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
          raise Exception.Create('Resource Workshop workers did not retire');
        end;
      end;
    end;
    {$endif}
  end;
  GResolver := nil;
  GTransportLease := nil;
  FreeAndNil(GDocument);
end;

procedure InitializeApplications;
var
  LIndex: Integer;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  {$endif}
  {$ifdef NYX_RESOURCE_WORKSHOP_STUDIO}
  LPair: TNyxProjectPair;
  {$ifndef PAS2JS}
  LBytes: TNyxBytes;
  LFile: TFileStream;
  {$endif}
  {$endif}
begin
  {$ifdef NYX_RESOURCE_WORKSHOP_STUDIO}
  LPair := GStudioJourney.Pair;
  Inc(GChecks, GStudioJourney.Checks);
  GDocument := TNyxCodec.Decode(LPair.Design);
  {$ifndef PAS2JS}
  ForceDirectories(ParamStr(1));
  LBytes := NyxEncodeUTF8(LPair.Source);
  LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) +
    'nyx.generated.view.pas', fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LFile.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LFile.Free;
  end;
  {$endif}
  GStudioJourney := nil;
  {$else}
  {$ifdef NYX_COMPILED_RESOURCE_WORKSHOP}
  GDocument := BuildNyxDocument;
  {$else}
  GDocument := BuildResourceWorkshopDocument;
  {$endif}
  {$endif}
  SetLength(GViews, 2);
  GSaved := TNyxCodec.Encode(GDocument);
  GTransport := TTransport.Create;
  GTransportLease := GTransport;
  GResolver := NewNyxResourceResolver(GTransportLease);
  for LIndex := 0 to 1 do
  begin
    GApplications[LIndex] := TReviewApplication.Create;
    GApplications[LIndex].ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand), GResolver);
    {$ifdef PAS2JS}
    LHost := TJSHTMLElement(document.createElement('main'));
    document.body.appendChild(LHost);
    GApplications[LIndex].Run(GDocument, LHost);
    {$else}
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
  GInitialized := True;
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    {$ifdef NYX_RESOURCE_WORKSHOP_STUDIO}

    if not GInitialized and GStudioJourney.Tick then
    begin
      InitializeApplications;
    end;
    {$endif}

    if GInitialized then
    begin
      Step;
    end;

    if GDone then
    begin
      Retire;
      document.body.setAttribute('data-resource-workshop', 'passed');
      {$ifdef NYX_RESOURCE_WORKSHOP_STUDIO}
      document.body.appendChild(document.createTextNode(
        'PASS ' + IntToStr(GChecks) + ' connected resource Workshop checks'));
      {$else}
      document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' connected resource Workshop checks';
      {$endif}
      Exit;
    end;
    window.setTimeout(@Drive, 5);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-resource-workshop', 'failed');
      document.body.textContent := E.Message + ' / stage ' + IntToStr(GStage);
      Retire;
    end;
    {$ifdef PAS2JS}
    else
    begin
      document.body.setAttribute('data-resource-workshop', 'failed');
      document.body.textContent := String(JSExceptValue) + ' / stage ' + IntToStr(GStage);
      Retire;
    end;
    {$endif}
  end;
end;
{$endif}

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  try
    {$ifdef NYX_RESOURCE_WORKSHOP_STUDIO}
    GStudioJourney := NewResourceWorkshopStudioJourney(
      {$ifdef PAS2JS}''{$else}ParamStr(1){$endif},
      {$ifdef PAS2JS}window.innerWidth{$else}StrToIntDef(ParamStr(2), 1280){$endif});
    {$else}
    InitializeApplications;
    {$endif}
    {$ifdef PAS2JS}Drive;{$else}
    while not GDone do
    begin
      Application.ProcessMessages;
      CheckSynchronize;
      {$ifdef NYX_RESOURCE_WORKSHOP_STUDIO}

      if not GInitialized and GStudioJourney.Tick then
      begin
        InitializeApplications;
        { Let the actual native UI queue settle the newly mounted application's
          attachment observation before inspecting its painted Tool controls. }
        Continue;
      end;
      {$endif}

      if GInitialized then
      begin
        Step;
      end;
      Sleep(5);
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' connected resource Workshop checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-resource-workshop', 'failed');
      document.body.textContent := E.Message + ' / admission';
      {$else}
      WriteLn('FAIL ', E.Message, ' / stage ', GStage);
      Retire;
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
