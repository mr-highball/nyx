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

program nyx_resource_persistence_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.bytes, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.resources, nyx.resource.sources, nyx.resource.cache, nyx.resources.loader,
  nyx.application.resources, nyx.resource.context, nyx.scheduler
  , nyx.test.resource.failures
  {$ifdef PAS2JS}
  , JS, Web, nyx.application.browser, nyx.resource.cache.browser,
    nyx.resources.http.browser
  {$else}
  , Interfaces, Forms, StdCtrls, Graphics, IntfGraphics, FPWritePNG, SyncObjs,
    nyx.application.lcl, nyx.resource.cache.lcl, nyx.resources.http.lcl
  {$endif};

type
  { The boundary names are closed qualification phases. Each invocation creates
    a new application/resolver. A separate Pascal driver owns process retirement
    and the fresh private cache/profile reused by Store and Restore. }
  TPersistencePhase = (ppStore, ppRestore, ppQuota, ppCorrupt, ppRespect, ppDeadline, ppFailures);
  TPersistenceApplication = {$ifdef PAS2JS}TNyxBrowserApplication{$else}TNyxLCLApplication{$endif};
  {$ifndef PAS2JS}
  { Occupies only the fixture's own transport pool. Its owned event and worker
    lease survive cancellation until the worker observes release/shutdown. }
  TDeadlineGate = class(TInterfacedObject, INyxWork)
  private
    FRelease: TEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Release;
    procedure Execute(const AExecution: INyxExecution);
  end;
  {$endif}

const
  CPhaseNames: array[TPersistencePhase] of TNyxText =
    ('store', 'restore', 'quota', 'corrupt', 'respect', 'deadline', 'failures');
  CLoaded: TNyxText = 'Keep creating 🌙';
  CPrompt: TNyxText = 'Project name 🌙';
  CEnglish: TNyxText = 'Your English workspace';

type
  { Borrows no document from another project. The document outlives its host;
    subscriptions retire before their method receiver and retained runtime
    snapshots safely outlive the host. Ordinary target controls do all painting. }
  TJourney = class
  private
    FPhase: TPersistencePhase;
    FDocument: TNyxDocument;
    FApplication: TPersistenceApplication;
    FResources: INyxApplicationResources;
    FSubscription: INyxResourceSubscription;
    FScheduler: INyxScheduler;
    FBefore: TNyxText;
    FBusy: Boolean;
    FStage: Integer;
    FStarted: Double;
    FRetiredAt: Double;
    FChecks: Integer;
    FFinished: Boolean;
    FFailure: TNyxResourceFailure;
    FBeforeFailure: TNyxText;
    FChanges: Integer;
    FBeforeChanges: Integer;
    {$ifdef PAS2JS}
    FTimer: NativeInt;
    FCaptionIdentity: TJSHTMLElement;
    FInputIdentity: TJSHTMLElement;
    {$else}
    FGate: TDeadlineGate;
    FGateLease: INyxWork;
    FFailureFile: TNyxResourceFailureFile;
    FCaptionIdentity: TObject;
    FInputIdentity: TObject;
    {$endif}
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    function Caption(const AID: TNyxText): TNyxText;
    function Validate(const AContext: INyxResourceContext): Boolean;
    procedure Finish;
    procedure Capture;
    procedure DeadlineNext;
    procedure FailureNext;
    procedure ResourceChanged(const AContext: INyxResourceContext);
    function Checkpoint(const AName: TNyxText): Boolean;
    {$ifndef PAS2JS}
    procedure OccupyTransport;
    {$endif}
    {$ifdef PAS2JS}
    procedure CorruptAndLoad(const AURL: TNyxText); async;
    {$endif}
  public
    destructor Destroy; override;
    procedure Start;
    procedure Next;
    property Finished: Boolean read FFinished;
  end;

function Milliseconds: Double;
begin
  {$ifdef PAS2JS}
  Result := window.performance.now;
  {$else}
  Result := GetTickCount64;
  {$endif}
end;

{$ifndef PAS2JS}
constructor TDeadlineGate.Create;
begin
  inherited Create;
  FRelease := TEvent.Create(nil, True, False, '');
end;

destructor TDeadlineGate.Destroy;
begin
  FRelease.Free;
  inherited Destroy;
end;

procedure TDeadlineGate.Release;
begin
  FRelease.SetEvent;
end;

procedure TDeadlineGate.Execute(const AExecution: INyxExecution);
begin
  while not AExecution.Cancelled and (FRelease.WaitFor(10) <> wrSignaled) do
  begin
  end;
end;

procedure TJourney.OccupyTransport;
var
  LStarted: QWord;
begin
  { Admission and bounded pumping happen in this fixture, not in the product.
    The gate is released on every test exit, including assertion failures. }
  FGate := TDeadlineGate.Create;
  FGateLease := FGate;
  FScheduler.Submit(FGateLease, neThreaded);
  LStarted := GetTickCount64;
  while ((FScheduler as INyxSchedulerMonitor).WorkerLoad.Running <> 1) and
    (GetTickCount64 - LStarted < 2000) do
  begin
    Application.ProcessMessages;
    CheckSynchronize(0);
    Sleep(1);
  end;
  Check((FScheduler as INyxSchedulerMonitor).WorkerLoad.Running = 1,
    'private transport worker is occupied before actual application loading');
end;
{$endif}

procedure TJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxResource.Create('Application persistence: ' + AReason);
  end;
  Inc(FChecks);
end;

function TJourney.Caption(const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := FApplication.View.ElementFor(AID).textContent;
  {$else}
  Result := TNyxText(RawByteString(TLabel(FApplication.View.ControlFor(AID)).Caption));
  {$endif}
end;

function TJourney.Validate(const AContext: INyxResourceContext): Boolean;
begin
  Result := not FBusy;
end;

procedure TJourney.ResourceChanged(const AContext: INyxResourceContext);
begin
  Inc(FChanges);
end;

{$ifndef PAS2JS}
function ReadOwnedText(const APath: String; AMaximum: Integer): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try

    if (LStream.Size < 1) or (LStream.Size > AMaximum) then
    begin
      raise ENyxResource.Create('Private qualification file exceeds its exact byte budget');
    end;
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
    Result := NyxDecodeUTF8(LBytes);
  finally
    LStream.Free;
  end;
end;

procedure WriteOwnedText(const APath: String; const AText: TNyxText);
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  LStream := TFileStream.Create(APath, fmCreate);
  try
    LStream.WriteBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
end;

{ Refuse existing user cache directories before any store/corruption action.
  Later phases admit only this fixture's exact origin-bound marker. Corruption
  targets one previously parsed entry belonging to the same owned URL/kind. }
procedure AdmitNativeHome(APhase: TPersistencePhase; const AURL: TNyxText);
const
  CMarker = '.persistence-qualification.json';
var
  LHome: String;
  LMarker: TNyxDataValue;
  LSearch: TSearchRec;
  LEntry: TNyxResourceCacheEntry;
  LPath: String;
  LTarget: String;
  LCount: Integer;
begin

  if APhase in [ppDeadline, ppFailures] then
  begin
    Exit;
  end;
  LHome := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));

  if APhase = ppStore then
  begin

    if DirectoryExists(LHome) then
    begin
      raise ENyxResource.Create('Store requires a fresh private qualification cache home');
    end;
    ForceDirectories(LHome);
    WriteOwnedText(LHome + CMarker, NyxObject([NyxField('version', NyxData(1)),
      NyxField('url', NyxData(AURL))]).ToJSON);
  end
  else
  begin
    LMarker := TNyxDataValue.ParseJSON(ReadOwnedText(LHome + CMarker, 4096));

    if (LMarker.Count <> 2) or (LMarker.Field('version').AsInteger <> 1) or
      (LMarker.Field('url').AsText <> AURL) then
    begin
      raise ENyxResource.Create('Cache home belongs to a different qualification');
    end;
  end;

  if APhase <> ppCorrupt then
  begin
    Exit;
  end;
  LCount := 0;
  LTarget := '';

  if FindFirst(LHome + '*.nyx-resource', faAnyFile, LSearch) = 0 then
  begin
    try
      repeat
        LPath := LHome + LSearch.Name;
        LEntry := TNyxResourceCacheEntry.FromData(TNyxDataValue.ParseJSON(
          ReadOwnedText(LPath, 2 * NyxMaximumPackedBytes + 16384)));

        if (LEntry.URL.Address = AURL) and (LEntry.Kind = nrkJSON) then
        begin
          Inc(LCount);
          LTarget := LPath;
        end;
      until FindNext(LSearch) <> 0;
    finally
      SysUtils.FindClose(LSearch);
    end;
  end;

  if LCount <> 1 then
  begin
    raise ENyxResource.Create('Corruption requires one exact previously stored qualification entry');
  end;
  WriteOwnedText(LTarget, 'damaged');
end;
{$endif}

{$ifdef PAS2JS}
procedure TJourney.CorruptAndLoad(const AURL: TNyxText); async;
var
  LCache: TJSCache;
  LKey: String;
  LResponse: TJSResponse;
  LEntry: TNyxResourceCacheEntry;
begin
  try
    { This fresh driver's private origin/profile owns the cache. Target the
      existing version-one key only after its envelope proves URL/kind identity.
      Real Cache Storage corruption is exercised without replacing host APIs. }
    LCache := TJSCache(await(TJSCacheStorage(TJSObject(window)['caches']).open('nyx-resource-cache-v1')));
    LKey := window.location.origin + '/.nyx/resource-cache?kind=' + IntToStr(Ord(nrkJSON)) +
      '&url=' + encodeURIComponent(AURL);
    LResponse := TJSResponse(await(LCache.match(LKey)));
    Check((LResponse <> nil) and not isUndefined(LResponse), 'owned cached envelope exists before corruption');
    LEntry := TNyxResourceCacheEntry.FromData(TNyxDataValue.ParseJSON(await(LResponse.text())));
    Check((LEntry.URL.Address = AURL) and (LEntry.Kind = nrkJSON), 'corruption targets exact owned bytes');
    LResponse := TJSResponse.new('damaged');
    LResponse.headers.append('x-nyx-cache-bytes', '7');
    await(LCache.put(LKey, LResponse));
    FResources.Reload(NyxResourceRef('copy'), NyxDefaultLocale);
    FTimer := window.setInterval(@Next, 20);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
    end;
  end;
end;
{$endif}

procedure TJourney.Start;
var
  LName: TNyxText;
  LURL: TNyxText;
  LPhase: TPersistencePhase;
  LFound: Boolean;
  LPage: INyxColumn;
  LCopy: TNyxResourceRef;
  LCache: INyxResourceCacheStorage;
  LTransport: INyxResourceTransport;
  LResolver: INyxResourceResolver;
  LLabels: TNyxResourceLabels;
  LPolicy: TNyxResourceCachePolicy;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  {$endif}
begin
  FStarted := Milliseconds;
  {$ifdef PAS2JS}
  LName := Copy(window.location.search, Length('?phase=') + 1, MaxInt);
  LURL := window.location.origin + Copy(window.location.pathname, 1,
    LastDelimiter('/', window.location.pathname)) + 'copy.json';
  {$else}

  if ParamCount <> 4 then
  begin
    raise ENyxResource.Create('Supply phase, owned HTTP file URL, cache home (or marked failure directory), and PNG path');
  end;
  LName := TNyxText(ParamStr(1));
  LURL := TNyxText(ParamStr(2));
  {$endif}
  LFound := False;
  for LPhase := Low(TPersistencePhase) to High(TPersistencePhase) do
  begin

    if LName = CPhaseNames[LPhase] then
    begin
      FPhase := LPhase;
      LFound := True;
    end;
  end;
  Check(LFound, 'phase is explicit before loading');
  {$ifndef PAS2JS}
  AdmitNativeHome(FPhase, LURL);
  {$endif}

  if FPhase = ppQuota then
  begin
    LURL := LURL + TNyxText('?quota=1');
  end;
  LLabels := NyxResourceLabels.Add(NyxResourceLabel('Current captions'));

  if FPhase = ppStore then
  begin
    LLabels := NyxResourceLabels.Add(NyxResourceLabel('Stored captions'));
  end;
  FDocument := TNyxDocument.Create;
  FDocument.Title := 'Persistent resource workshop';

  if FPhase = ppFailures then
  begin
    FDocument.Title := 'Resource recovery workshop';
  end;
  LCopy := NyxResourceRef('copy');
  LPolicy := NyxResourceCache.Persistent.FreshFor(3600).ServerPolicy(rcspOverride);

  if FPhase = ppDeadline then
  begin
    LPolicy := NyxResourceCache.Bypass;
  end;

  if FPhase = ppFailures then
  begin
    LPolicy := NyxResourceCache.Bypass.MaximumBytes(128);
  end;

  if FPhase = ppRespect then
  begin
    { A previous caller deliberately stored this server's no-store response.
      Current caller policy must govern reuse as well as the next write. }
    LPolicy := LPolicy.ServerPolicy(rcspRespect);
  end;

  if FPhase = ppFailures then
  begin
    FDocument.Resources.Define(LCopy,
      NyxResourceDiscovery(NyxHostedResource(nrkJSON, NyxResourceURL(LURL))
        .Cache(LPolicy).Fallback(NyxJSONResource(
          NyxDecodeUTF8(NyxResourceFailureBytes(nrfHealthy)))))
        .WithLabels(LLabels));
  end
  else
  begin
    FDocument.Resources.Define(LCopy,
      NyxResourceDiscovery(NyxHostedResource(nrkJSON, NyxResourceURL(LURL))
        .Cache(LPolicy)
        .Fallback(NyxJSONResource('{"headline":"Ready while loading","prompt":"Local project name"}')))
        .WithLabels(LLabels));
  end;
  FDocument.Resources.Define(LCopy, NyxLocale('en-GB'),
    NyxJSONResource('{"headline":"Your English workspace","prompt":"Programme name"}'));
  LPage := NewNyxColumn('home');
  LPage.Configure.Padding(24).Gap(16).Done;
  FDocument.AddPage(LPage);
  LPage.Add(NewNyxHeading('workshop-title').WithText('Keep your workspace close'));
  LPage.Add(NewNyxLabel('caption').Binds.Text(NyxResourceValue(LCopy).Field('headline')).Done);
  LPage.Add(NewNyxLabel('pinned-caption').Binds.Text(NyxResourceValue(LCopy)
    .Field('headline').Localize(NyxDefaultLocale, NyxDefaultLocale)).Done);
  LPage.Add(NewNyxInput('project-name').Binds.Placeholder(
    NyxResourceValue(LCopy).Field('prompt')).Done);
  LPage := NewNyxColumn('details');
  FDocument.AddPage(LPage);
  LPage.Add(NewNyxLabel('details-caption').Binds.Text(
    NyxResourceValue(LCopy).Field('headline')).Done);
  FBefore := TNyxCodec.Encode(FDocument);
  FApplication := TPersistenceApplication.Create;
  {$ifdef PAS2JS}
  LTransport := NewNyxBrowserResourceTransport;
  LCache := NewNyxBrowserResourceCache;

  if FPhase = ppQuota then
  begin
    LCache := NewNyxBrowserResourceCache(1, 1);
  end;
  {$else}
  FScheduler := NewNyxScheduler;

  if FPhase = ppDeadline then
  begin
    FScheduler := NewNyxScheduler(TNyxSchedulerOptions.Defaults.Workers(1));
  end;
  LTransport := NewNyxNativeResourceTransport(FScheduler);
  LCache := nil;

  if not (FPhase in [ppDeadline, ppFailures]) then
  begin
    LCache := NewNyxFileResourceCache(TNyxText(ParamStr(3)));
  end;

  if FPhase = ppQuota then
  begin
    LCache := NewNyxFileResourceCache(TNyxText(ParamStr(3)), 1, 1);
  end;
  {$endif}
  LResolver := NewNyxResourceResolver(LTransport, nil, LCache);

  if FPhase = ppDeadline then
  begin
    FApplication.ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand)
      .Request(NyxResourceLoadOptions.WholeRequest(500)), LResolver);
  end
  else
  begin
    FApplication.ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand), LResolver);
  end;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(LHost);
  FApplication.Run(FDocument, LHost);
  {$else}
  FApplication.Mount(FDocument);
  FApplication.Window.Show;
  {$endif}
  FResources := FApplication.Resources;

  if FPhase = ppFailures then
  begin
    FFailure := nrfHealthy;
    FSubscription := FResources.Subscribe(nil, ResourceChanged);
    {$ifdef PAS2JS}
    FCaptionIdentity := FApplication.View.ElementFor('caption');
    FInputIdentity := FApplication.View.InputFor('project-name');
    {$else}
    FCaptionIdentity := FApplication.View.ControlFor('caption');
    FInputIdentity := FApplication.View.InputFor('project-name');
    FFailureFile := TNyxResourceFailureFile.Create(ParamStr(3), LURL);
    {$endif}
    FResources.Reload(NyxResourceRef('copy'), NyxDefaultLocale);
    {$ifdef PAS2JS}
    FTimer := window.setInterval(@Next, 20);
    {$endif}
    Exit;
  end;
  Check(Caption('caption') = 'Ready while loading', 'initial fallback is not a completed fetch');

  if FPhase = ppDeadline then
  begin
    {$ifdef PAS2JS}
    FTimer := window.setInterval(@Next, 20);
    {$endif}
    Exit;
  end;
  {$ifdef PAS2JS}

  if FPhase = ppCorrupt then
  begin
    CorruptAndLoad(LURL);
    Exit;
  end;
  {$endif}
  FResources.Reload(LCopy, NyxDefaultLocale);
  {$ifdef PAS2JS}
  FTimer := window.setInterval(@Next, 20);
  {$endif}
end;

function TJourney.Checkpoint(const AName: TNyxText): Boolean;
begin
  {$ifdef PAS2JS}
  document.body.setAttribute('data-capture-checkpoint', AName);
  Result := document.body.getAttribute('data-capture-observed') = AName;
  {$else}
  Capture;
  Result := True;
  {$endif}
end;

procedure TJourney.FailureNext;
var
  LStatus: TNyxApplicationResourceStatus;
  LCaption: TNyxText;
  LPrompt: TNyxText;
begin
  Check(Milliseconds - FStarted < 30000, 'actual hosted failure journey stays bounded');
  LStatus := FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale);

  if LStatus.Phase in [nrpIdle, nrpQueued, nrpLoading, nrpWaiting] then
  begin
    Exit;
  end;
  LCaption := CLoaded;
  LPrompt := CPrompt;

  if FFailure = nrfCorrected then
  begin
    LCaption := 'Back to creating 🌙';
    LPrompt := 'A refreshed project 🌙';
  end;
  Check((Caption('caption') = LCaption) and (Caption('pinned-caption') = LCaption),
    'actual hosted failure never partially publishes a label');
  {$ifdef PAS2JS}
  Check((FApplication.View.ElementFor('caption') = FCaptionIdentity) and
    (FApplication.View.InputFor('project-name') = FInputIdentity),
    'actual browser controls retain identity across hosted failures');
  Check(TJSHTMLInputElement(FInputIdentity).placeholder = LPrompt,
    'actual browser prompt preserves the complete admitted value');
  {$else}
  Check((FApplication.View.ControlFor('caption') = FCaptionIdentity) and
    (FApplication.View.InputFor('project-name') = FInputIdentity),
    'actual native controls retain identity across hosted failures');
  Check(TNyxText(RawByteString(TEdit(FInputIdentity).TextHint)) = LPrompt,
    'actual native prompt preserves the complete admitted value');
  {$endif}
  Check(TNyxCodec.Encode(FDocument) = FBefore, 'HTTP failures never rewrite authored defaults');
  case FFailure of
    nrfHealthy, nrfCorrected:
      begin
        Check((LStatus.Phase = nrpReady) and (LStatus.Origin = rloNetwork) and
          (LStatus.Error = ''), 'real healthy/corrected bytes publish successfully at the same URL');
      end;
    nrfNotFound, nrfMalformed, nrfOversized:
      begin
        Check((LStatus.Phase = nrpReady) and (LStatus.Origin = rloFallback) and
          (LStatus.Error <> ''), 'real host failure uses explicit caller fallback with a visible error');

        if FFailure = nrfNotFound then
        begin
          Check(Pos('404', LStatus.Error) > 0, 'actual missing file preserves its HTTP status');
        end;

        if FFailure = nrfMalformed then
        begin
          Check(Pos('JSON position', LStatus.Error) > 0,
            'malformed bytes reach JSON admission rather than failing another transport operation');
        end;

        if FFailure = nrfOversized then
        begin
          Check(Pos('byte budget', LStatus.Error) > 0,
            'actual oversized reply reports the caller payload limit');
        end;
      end;
    nrfWrongType:
      begin
        Check((LStatus.Phase = nrpRejected) and (LStatus.Origin = rloNetwork),
          'parsed HTTP JSON is still subject to typed control admission');
        Check((FResources.Context.Snapshot.ToData.ToJSON = FBeforeFailure) and
          (FChanges = FBeforeChanges), 'typed refusal preserves the entire catalog and skips Changed');
        {$ifdef PAS2JS}
        document.body.setAttribute('data-resource-diagnostic', LStatus.Error);
        {$else}
        WriteLn('Observed resource rejection / ', LStatus.Error);
        {$endif}
        Check((Pos('copy', LStatus.Error) > 0) and (Pos('prompt', LStatus.Error) > 0) and
          (Pos('text', LStatus.Error) > 0), 'typed diagnostic names resource, selector and expected type');
      end;
  end;

  if not Checkpoint('failure-' + NyxResourceFailureName(FFailure)) then
  begin
    Exit;
  end;

  if FFailure = nrfCorrected then
  begin
    {$ifndef PAS2JS}
    FFailureFile.Apply(nrfHealthy);
    {$endif}
    FSubscription.Disconnect;
    FSubscription := nil;
    FreeAndNil(FApplication);
    Check(NyxApplicationResourceDiagnostics(FResources).CaptureRuntime.Stopped,
      'host retires after real failure and correction');
    Finish;
    Exit;
  end;
  FFailure := TNyxResourceFailure(Ord(FFailure) + 1);
  FStage := Ord(FFailure);
  FBeforeFailure := FResources.Context.Snapshot.ToData.ToJSON;
  FBeforeChanges := FChanges;
  {$ifndef PAS2JS}
  FFailureFile.Apply(FFailure);
  {$endif}
  FResources.Reload(NyxResourceRef('copy'), NyxDefaultLocale);
end;

procedure TJourney.DeadlineNext;
var
  LStatus: TNyxApplicationResourceStatus;
begin
  Check(Milliseconds - FStarted < 30000, 'actual deadline journey stays bounded');
  LStatus := FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale);
  case FStage of
    0:
      begin

        if not Checkpoint('deadline-ready') then
        begin
          Exit;
        end;
        {$ifndef PAS2JS}
        OccupyTransport;
        {$endif}
        FResources.Reload(NyxResourceRef('copy'), NyxDefaultLocale);
        FStage := 1;
      end;
    1:
      begin

        if LStatus.Phase in [nrpIdle, nrpQueued, nrpLoading, nrpWaiting] then
        begin
          Exit;
        end;
        Check((LStatus.Phase = nrpReady) and (LStatus.Origin = rloFallback) and
          (Pos('deadline', LStatus.Error) > 0), 'actual request expiry publishes explicit fallback once');
        Check((Caption('caption') = 'Ready while loading') and
          (Caption('pinned-caption') = 'Ready while loading'),
          'expired actual loading preserves complete fallback controls');
        Check(TNyxCodec.Encode(FDocument) = FBefore, 'deadline cannot rewrite saved declarations');
        {$ifndef PAS2JS}
        Check((FScheduler as INyxSchedulerMonitor).WorkerLoad.Running = 1,
          'application expiry reports before the occupied transport worker returns');
        {$endif}

        if not Checkpoint('deadline-expired') then
        begin
          Exit;
        end;
        {$ifndef PAS2JS}
        FGate.Release;
        FGate := nil;
        FGateLease := nil;
        {$endif}
        FStage := 2;
      end;
    2:
      begin
        {$ifndef PAS2JS}

        if (FScheduler as INyxSchedulerMonitor).WorkerLoad.Running <> 0 then
        begin
          Exit;
        end;
        {$endif}
        FResources.Reload(NyxResourceRef('copy'), NyxDefaultLocale);
        FStage := 3;
      end;
    3:
      begin

        if LStatus.Phase in [nrpQueued, nrpLoading, nrpWaiting] then
        begin
          Exit;
        end;
        Check((LStatus.Phase = nrpReady) and (LStatus.Origin = rloNetwork) and
          (LStatus.Error = '') and (Caption('caption') = CLoaded),
          'healthy real HTTP recovers after deadline without stale timeout delivery');
        {$ifdef PAS2JS}
        Check(TJSHTMLInputElement(FApplication.View.InputFor('project-name')).placeholder = CPrompt,
          'recovered actual browser prompt consumes the complete reply');
        {$else}
        Check(TNyxText(RawByteString(TEdit(FApplication.View.InputFor('project-name')).TextHint)) = CPrompt,
          'recovered actual native prompt consumes the complete reply');
        {$endif}

        if not Checkpoint('deadline-recovered') then
        begin
          Exit;
        end;
        {$ifndef PAS2JS}
        OccupyTransport;
        {$endif}
        FResources.Reload(NyxResourceRef('copy'), NyxDefaultLocale);
        FStage := 4;
      end;
    4:
      begin

        if LStatus.Phase = nrpQueued then
        begin
          Exit;
        end;
        Check((LStatus.Phase = nrpLoading) and (Caption('caption') = CLoaded),
          'active queued/fetch attempt retains installed content before cancellation');
        FResources.Cancel;
        Check(FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale).Phase = nrpCancelled,
          'actual request cancellation retires its borrowed receiver immediately');
        Check(NyxApplicationResourceDiagnostics(FResources).CaptureRuntime.Entry(0).HasPublishedLoad,
          'cancel retains successful installed-load evidence');
        Check(TNyxCodec.Encode(FDocument) = FBefore, 'request cancellation preserves saved defaults');
        {$ifndef PAS2JS}
        FGate.Release;
        FGate := nil;
        FGateLease := nil;
        {$endif}
        FreeAndNil(FApplication);
        FRetiredAt := Milliseconds;
        FStage := 5;
      end;
    5:
      begin

        if Milliseconds - FRetiredAt < 800 then
        begin
          Exit;
        end;
        Check(NyxApplicationResourceDiagnostics(FResources).CaptureRuntime.Stopped,
          'retired application remains stopped beyond the cancelled request deadline');
        Check(TNyxCodec.Encode(FDocument) = FBefore, 'late timer/transport retirement cannot change the document');
        {$ifndef PAS2JS}
        Check(((FScheduler as INyxSchedulerMonitor).WorkerLoad.Pending = 0) and
          ((FScheduler as INyxSchedulerMonitor).WorkerLoad.Running = 0),
          'cancelled native worker attempt retires without late publication');
        {$endif}
        Finish;
      end;
  end;
end;

procedure TJourney.Capture;
{$ifndef PAS2JS}
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
{$endif}
begin
  {$ifdef PAS2JS}
  document.body.setAttribute('data-capture-checkpoint', 'persistent-' + CPhaseNames[FPhase]);
  {$else}
  Application.ProcessMessages;
  LBitmap := TBitmap.Create;
  LImage := nil;
  try
    LBitmap.SetSize(FApplication.Window.Width, FApplication.Window.Height);
    FApplication.Window.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;

    if FPhase in [ppDeadline, ppFailures] then
    begin
      LImage.SaveToFile(ParamStr(4) + '.' + IntToStr(FStage) + '.png');
    end
    else
    begin
      LImage.SaveToFile(ParamStr(4));
    end;
  finally
    LImage.Free;
    LBitmap.Free;
  end;
  {$endif}
end;

procedure TJourney.Next;
var
  LStatus: TNyxApplicationResourceStatus;
  LRuntime: INyxResourceRuntimeSnapshot;
  LRefused: Boolean;
begin
  try

    if FPhase = ppFailures then
    begin
      FailureNext;
      Exit;
    end;

    if FPhase = ppDeadline then
    begin
      DeadlineNext;
      Exit;
    end;
    Check(Milliseconds - FStarted < 30000, 'actual application journey stays bounded');
    LStatus := FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale);
    case FStage of
      0:
        begin

          if LStatus.Phase in [nrpQueued, nrpLoading, nrpWaiting] then
          begin
            Exit;
          end;
          Check(LStatus.Phase = nrpReady, 'persistent load publishes atomically');
          Check((Caption('caption') = CLoaded) and (Caption('pinned-caption') = CLoaded),
            'real controls paint exact hosted Unicode');
          {$ifdef PAS2JS}
          Check(TJSHTMLInputElement(FApplication.View.InputFor('project-name')).placeholder = CPrompt,
            'real browser prompt consumes persistent data');
          {$else}
          Check(TNyxText(RawByteString(TEdit(FApplication.View.InputFor('project-name')).TextHint)) = CPrompt,
            'real native prompt consumes persistent data');
          {$endif}

          if FPhase = ppRestore then
          begin
            Check((LStatus.Origin = rloFreshCache) and (LStatus.CacheRead = rcuPersistent),
              'a fresh process consumes persistent bytes without HTTP');
            Check((NyxResourceLabelsOf(FResources.Context.Snapshot.Definition(
              NyxResourceRef('copy'), NyxDefaultLocale)).Count = 1) and
              NyxResourceLabelsOf(FResources.Context.Snapshot.Definition(
              NyxResourceRef('copy'), NyxDefaultLocale)).Contains(NyxResourceLabel('Current captions')),
              'cached bytes use current creator labels');
          end
          else if FPhase = ppQuota then
          begin
            Check((LStatus.Origin = rloNetwork) and (LStatus.CacheWrite = rcuMemory) and
              (LStatus.CacheWarning <> ''), 'real storage budget failure falls back to reported memory');
          end
          else if FPhase = ppRespect then
          begin
            Check((LStatus.Origin = rloNetwork) and (LStatus.CacheRead = rcuNone) and
              (LStatus.CacheWrite = rcuNone), 'current Respect refuses reuse/storage of prior override bytes');
          end
          else if FPhase = ppCorrupt then
          begin
            Check((LStatus.Origin = rloNetwork) and (LStatus.CacheWrite = rcuPersistent) and
              (LStatus.CacheWarning <> ''), 'damaged storage reports refusal and heals through real HTTP');
          end
          else
          begin
            Check((LStatus.Origin = rloNetwork) and (LStatus.CacheWrite = rcuPersistent),
              'caller override stores actual no-store HTTP bytes privately');
          end;
          Check(TNyxCodec.Encode(FDocument) = FBefore, 'loading never rewrites authored declarations');
          Capture;
          FStage := 1;
        end;
      1:
        begin
          {$ifdef PAS2JS}

          if document.body.getAttribute('data-capture-observed') <>
            'persistent-' + CPhaseNames[FPhase] then
          begin
            Exit;
          end;
          {$endif}
          FResources.Localize(NyxLocale('en-GB'), NyxDefaultLocale);
          Check((Caption('caption') = CEnglish) and (Caption('pinned-caption') = CLoaded),
            'locale inheritance changes independently of explicit default pin');
          FApplication.ShowPage('details');
          Check(Caption('details-caption') = CEnglish, 'a new page uses the accepted runtime locale');
          FApplication.ShowPage('home');
          FResources.Localize(NyxLocale('missing'), NyxLocale('en-GB'));
          Check(Caption('caption') = CEnglish, 'missing locale uses explicit caller fallback');
          FResources.Localize(NyxDefaultLocale, NyxDefaultLocale);
          FBusy := True;
          FSubscription := FResources.Subscribe(Validate, nil);
          FResources.Reload(NyxResourceRef('copy'), NyxDefaultLocale);
          FStage := 2;
        end;
      2:
        begin

          if LStatus.Phase <> nrpWaiting then
          begin
            Check(LStatus.Phase in [nrpQueued, nrpLoading], 'reload waits for actual target admission');
            Exit;
          end;
          Check(Caption('caption') = CLoaded, 'a busy publication preserves installed controls');
          FResources.Cancel;
          FSubscription.Disconnect;
          FSubscription := nil;
          Check(FResources.Status(NyxResourceRef('copy'), NyxDefaultLocale).Phase = nrpCancelled,
            'cancellation retires a pending real cache publication');
          LRuntime := NyxApplicationResourceDiagnostics(FResources).CaptureRuntime;
          Check(LRuntime.Entry(0).HasPublishedLoad, 'cancel keeps prior installed load evidence');
          Check(TNyxCodec.Encode(FDocument) = FBefore, 'locale, navigation and cancel preserve saved defaults');
          FreeAndNil(FApplication);
          Check(NyxApplicationResourceDiagnostics(FResources).CaptureRuntime.Stopped,
            'retained runtime owner reports host retirement');
          LRefused := False;
          try
            FResources.Reload;
          except
            on ENyxResource do
            begin
              LRefused := True;
            end;
          end;
          Check(LRefused, 'a retained stopped owner refuses new loading');
          Finish;
        end;
    end;
  except
    on LException: Exception do
    begin
      FFinished := True;
      {$ifdef PAS2JS}
      window.clearInterval(FTimer);
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
      {$else}
      raise;
      {$endif}
    end;
  end;
end;

procedure TJourney.Finish;
begin
  FFinished := True;
  {$ifdef PAS2JS}
  window.clearInterval(FTimer);
  document.body.setAttribute('data-persistence-checks', IntToStr(FChecks));
  document.body.setAttribute('data-test-result', 'passed');
  {$else}
  WriteLn('PASS / application persistence / ', CPhaseNames[FPhase], ' / ', FChecks, ' checks');
  {$endif}
end;

destructor TJourney.Destroy;
{$ifndef PAS2JS}
var
  LStarted: QWord;
{$endif}
begin

  if FSubscription <> nil then
  begin
    FSubscription.Disconnect;
  end;
  FApplication.Free;
  FResources := nil;
  FDocument.Free;
  {$ifndef PAS2JS}
  FFailureFile.Free;
  {$endif}

  if FScheduler <> nil then
  begin
    {$ifndef PAS2JS}

    if FGate <> nil then
    begin
      FGate.Release;
      FGate := nil;
      FGateLease := nil;
    end;
    {$endif}
    FScheduler.Shutdown;
    {$ifndef PAS2JS}
    { Only qualification waits for its own workers. Product shutdown remains
      nonblocking and every work item owns the state needed for retirement. }
    LStarted := GetTickCount64;
    while ((FScheduler as INyxSchedulerMonitor).WorkerLoad.ActiveWorkers > 0) and
      (GetTickCount64 - LStarted < 2000) do
    begin
      Application.ProcessMessages;
      CheckSynchronize(0);
      Sleep(1);
    end;
    {$endif}
  end;
  inherited Destroy;
end;

var
  GJourney: TJourney;
begin
  {$ifndef PAS2JS}
  Application.Initialize;
  {$endif}
  GJourney := TJourney.Create;
  try
    GJourney.Start;
    {$ifndef PAS2JS}
    try
      while not GJourney.Finished do
      begin
        CheckSynchronize(0);
        Application.ProcessMessages;
        GJourney.Next;
        Sleep(1);
      end;
    finally
      FreeAndNil(GJourney);
      CheckSynchronize(0);
      Application.ProcessMessages;
    end;
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
      {$else}
      FreeAndNil(GJourney);
      WriteLn('FAIL / ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
